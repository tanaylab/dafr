#!/usr/bin/env bash
# Ship dev → main: replace main's tree with dev's, strip dev-only
# paths, commit, optionally push + tag + release.
#
# Mirrors ~/src/pymisha/dev/skills/pymisha-ship/ship.sh.

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

DEV_REMOTE="private"        # aviezerl/dafr
MAIN_REMOTE="origin"        # tanaylab/dafr
DEV_BRANCH="dev"
MAIN_BRANCH="main"
# Paths that exist on dev but are stripped from main.
DEV_ONLY_PATHS=("dev/" "CLAUDE.md" "AGENTS.md" ".a5c/" ".claude/")

die()  { echo "ERROR: $*" >&2; exit 1; }
info() { echo "==> $*"; }

cleanup() {
    local current
    current="$(git branch --show-current 2>/dev/null || true)"
    if [[ "$current" != "$DEV_BRANCH" ]]; then
        info "Returning to $DEV_BRANCH branch..."
        git checkout -f "$DEV_BRANCH" 2>/dev/null || true
    fi
}
trap cleanup EXIT

guard_remote() {
    local branch="$1" remote="$2" action="$3"
    if [[ "$branch" == "$DEV_BRANCH" && "$remote" == "$MAIN_REMOTE" ]]; then
        die "BLOCKED: Cannot $action $DEV_BRANCH to $MAIN_REMOTE (tanaylab). Use $DEV_REMOTE instead."
    fi
    if [[ "$branch" == "$MAIN_BRANCH" && "$remote" == "$DEV_REMOTE" ]]; then
        die "BLOCKED: Cannot $action $MAIN_BRANCH to $DEV_REMOTE (aviezerl). Use $MAIN_REMOTE instead."
    fi
}

[[ "$(git branch --show-current)" == "$DEV_BRANCH" ]] \
    || die "Must be on $DEV_BRANCH branch. Currently on: $(git branch --show-current)"

[[ -z "$(git status --porcelain)" ]] \
    || die "Working tree is not clean. Commit or stash changes first."

git rev-parse --verify "$MAIN_BRANCH" >/dev/null 2>&1 \
    || die "Branch '$MAIN_BRANCH' does not exist. Create it first (see SKILL.md)."

# Optional R CMD check sanity — skip if R isn't on PATH.
if command -v R >/dev/null 2>&1; then
    info "Running light test sanity (devtools::test)..."
    R --quiet --no-save -e 'devtools::test()' 2>&1 | tail -3 || die "Tests failed. Fix before shipping."
fi

COMMIT_MSG="${1:-}"
DO_PUSH=false
for arg in "$@"; do
    [[ "$arg" == "--push" ]] && DO_PUSH=true
done

info "Switching to $MAIN_BRANCH..."
git checkout "$MAIN_BRANCH"

info "Replacing $MAIN_BRANCH tree with $DEV_BRANCH content..."
git read-tree --reset -u "$DEV_BRANCH"

for path in "${DEV_ONLY_PATHS[@]}"; do
    if git ls-files --error-unmatch "$path" >/dev/null 2>&1 || [[ -e "$path" ]]; then
        info "Removing dev-only: $path"
        # Drop from the index regardless of on-disk removability.
        git rm -rf --cached "$path" >/dev/null 2>&1 || true
        # Try to remove from the working tree but tolerate live-session
        # locks (e.g. .claude/.nfs* on mounted filesystems). These are
        # already ignored on main and will be re-excluded on next ship.
        rm -rf "$path" 2>/dev/null || true
    fi
done

echo ""
echo "══════════════════════════════════════════════"
echo "  Ship Summary (dev → main)"
echo "══════════════════════════════════════════════"
git diff --cached --stat
echo ""

if [[ -z "$COMMIT_MSG" ]]; then
    info "DRY RUN: No commit message provided."
    echo ""
    echo "To commit:  git commit -m \"your message\""
    echo "To push:    git push $MAIN_REMOTE $MAIN_BRANCH"
    echo "To abort:   git checkout -f $DEV_BRANCH"
    echo ""
    trap - EXIT
    exit 0
fi

info "Committing: $COMMIT_MSG"
git commit -m "$COMMIT_MSG"

if $DO_PUSH; then
    guard_remote "$MAIN_BRANCH" "$MAIN_REMOTE" "push"
    info "Pushing $MAIN_BRANCH to $MAIN_REMOTE..."
    git push "$MAIN_REMOTE" "$MAIN_BRANCH"

    # R-package version lives in DESCRIPTION's `Version:` line.
    VERSION="$(awk '/^Version:/ {print $2; exit}' DESCRIPTION)"
    TAG="v${VERSION}"

    if git rev-parse "$TAG" >/dev/null 2>&1; then
        info "Tag $TAG already exists, skipping."
    else
        info "Tagging $TAG..."
        git tag "$TAG"
        git push "$MAIN_REMOTE" "$TAG"
    fi

    if command -v gh >/dev/null 2>&1; then
        if gh release view "$TAG" >/dev/null 2>&1; then
            info "GitHub release $TAG already exists, skipping."
        else
            # Release notes from NEWS.md: everything between "# dafr vX.Y.Z"
            # and the next "# " heading.
            NOTES="$(awk '/^# dafr '"${VERSION}"'/{found=1; next} /^# /{if(found) exit} found{print}' NEWS.md)"
            if [[ -n "$NOTES" ]]; then
                info "Creating GitHub release $TAG..."
                gh release create "$TAG" --title "dafr $VERSION" --notes "$NOTES"
            else
                info "Creating GitHub release $TAG (using commit message)..."
                gh release create "$TAG" --title "dafr $VERSION" --notes "$COMMIT_MSG"
            fi
        fi
    else
        info "gh CLI not found — skipping GitHub release creation."
        info "Create manually: gh release create $TAG --title \"dafr $VERSION\" --notes \"...\""
    fi

    info "Returning to $DEV_BRANCH to push to $DEV_REMOTE..."
    git checkout "$DEV_BRANCH"
    guard_remote "$DEV_BRANCH" "$DEV_REMOTE" "push"
    git push "$DEV_REMOTE" "$DEV_BRANCH"
fi

info "Done! Returning to $DEV_BRANCH."
