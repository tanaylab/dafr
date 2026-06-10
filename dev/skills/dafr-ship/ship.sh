#!/usr/bin/env bash
# Ship dev → main: replace main's tree with dev's, strip dev-only
# paths, commit, optionally push + tag + release.
#
# Mirrors ~/src/pymisha/dev/skills/pymisha-ship/ship.sh.
#
# main is permanently checked out in a sibling worktree (~/src/dafr-main),
# so `git checkout main` in this (dev) worktree fails with
# "main is already used by worktree at ...". This script detects that
# worktree and runs the tree-replacement + commit there, leaving the dev
# worktree untouched. If main is NOT checked out anywhere it falls back to
# an in-place `git checkout main`.

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

DEV_REMOTE="private"        # aviezerl/dafr
MAIN_REMOTE="origin"        # tanaylab/dafr
DEV_BRANCH="dev"
MAIN_BRANCH="main"
# Paths that exist on dev but are stripped from main.
# docs/ is pkgdown's build-output dir on main (generated fresh + deployed to
# gh-pages by the pkgdown workflow); committing anything there - e.g. the
# superpowers specs/plans kept on dev - makes pkgdown's check_dest_is_pkgdown()
# refuse to build. Keep such docs on dev, strip from main.
DEV_ONLY_PATHS=("dev/" "docs/" "CLAUDE.md" "AGENTS.md" ".a5c/" ".claude/")

die()  { echo "ERROR: $*" >&2; exit 1; }
info() { echo "==> $*"; }

# Locate the worktree (if any) that currently has MAIN_BRANCH checked out.
# Prints its absolute path, or nothing if main is not checked out anywhere.
find_main_worktree() {
    local path="" line
    while IFS= read -r line; do
        case "$line" in
            "worktree "*)               path="${line#worktree }" ;;
            "branch refs/heads/$MAIN_BRANCH") echo "$path"; return 0 ;;
        esac
    done < <(git worktree list --porcelain)
    return 0
}

MAIN_WT="$(find_main_worktree)"
USE_WT=false
if [[ -n "$MAIN_WT" && "$MAIN_WT" != "$REPO_ROOT" ]]; then
    USE_WT=true
fi
# `git -C <dir>` runs as if started in <dir>; in-place mode is just the cwd.
G=(git)
$USE_WT && G=(git -C "$MAIN_WT")

COMMITTED=false
cleanup() {
    if $USE_WT; then
        # Never touched the dev worktree's HEAD. If we staged dev content into
        # the main worktree but did not commit it, roll that worktree back to
        # its committed main so a failed/aborted run leaves no half-shipped tree.
        if ! $COMMITTED; then
            info "Restoring $MAIN_BRANCH worktree ($MAIN_WT)..."
            # reset --hard restores tracked files to main; dev-only paths that
            # read-tree dropped in become untracked-on-disk, so remove just
            # those. Deliberately NOT `git clean -fd` — that would also delete
            # pre-existing untracked files there (test snapshots, build .o/.so).
            git -C "$MAIN_WT" reset --hard HEAD >/dev/null 2>&1 || true
            for path in "${DEV_ONLY_PATHS[@]}"; do
                rm -rf "${MAIN_WT:?}/$path" 2>/dev/null || true
            done
        fi
        return
    fi
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

if $USE_WT; then
    info "main is checked out in worktree: $MAIN_WT (operating there)"
    [[ -z "$(git -C "$MAIN_WT" status --porcelain)" ]] \
        || die "Main worktree ($MAIN_WT) is not clean. Commit or stash there first."
fi

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

if $USE_WT; then
    info "Replacing $MAIN_BRANCH tree (in $MAIN_WT) with $DEV_BRANCH content..."
else
    info "Switching to $MAIN_BRANCH..."
    git checkout "$MAIN_BRANCH"
    info "Replacing $MAIN_BRANCH tree with $DEV_BRANCH content..."
fi
# read-tree --reset -u resets the target worktree's index + working tree to
# dev's committed tree while keeping HEAD on main; the next commit lands on main.
"${G[@]}" read-tree --reset -u "$DEV_BRANCH"

for path in "${DEV_ONLY_PATHS[@]}"; do
    wt_path="$path"
    $USE_WT && wt_path="$MAIN_WT/$path"
    if "${G[@]}" ls-files --error-unmatch "$path" >/dev/null 2>&1 || [[ -e "$wt_path" ]]; then
        info "Removing dev-only: $path"
        # Drop from the index regardless of on-disk removability.
        "${G[@]}" rm -rf --cached "$path" >/dev/null 2>&1 || true
        # Try to remove from the working tree but tolerate live-session
        # locks (e.g. .claude/.nfs* on mounted filesystems). These are
        # already ignored on main and will be re-excluded on next ship.
        rm -rf "$wt_path" 2>/dev/null || true
    fi
done

echo ""
echo "══════════════════════════════════════════════"
echo "  Ship Summary (dev → main)"
echo "══════════════════════════════════════════════"
"${G[@]}" diff --cached --stat
echo ""

if [[ -z "$COMMIT_MSG" ]]; then
    info "DRY RUN: No commit message provided."
    echo ""
    echo "To commit:  ${G[*]} commit -m \"your message\""
    echo "To push:    ${G[*]} push $MAIN_REMOTE $MAIN_BRANCH"
    if $USE_WT; then
        echo "To abort:   git -C $MAIN_WT reset --hard HEAD"
    else
        echo "To abort:   git checkout -f $DEV_BRANCH"
    fi
    echo ""
    trap - EXIT
    $USE_WT && cleanup   # discard the staged dev tree from the dry run
    exit 0
fi

info "Committing: $COMMIT_MSG"
"${G[@]}" commit -m "$COMMIT_MSG"
COMMITTED=true

if $DO_PUSH; then
    guard_remote "$MAIN_BRANCH" "$MAIN_REMOTE" "push"
    info "Pushing $MAIN_BRANCH to $MAIN_REMOTE..."
    "${G[@]}" push "$MAIN_REMOTE" "$MAIN_BRANCH"

    # R-package version lives in DESCRIPTION's `Version:` line. Read the shipped
    # copy (main worktree in USE_WT mode) so the tag matches what was pushed.
    DESC_DIR="$REPO_ROOT"
    $USE_WT && DESC_DIR="$MAIN_WT"
    VERSION="$(awk '/^Version:/ {print $2; exit}' "$DESC_DIR/DESCRIPTION")"
    TAG="v${VERSION}"

    if git rev-parse "$TAG" >/dev/null 2>&1; then
        info "Tag $TAG already exists, skipping."
    else
        info "Tagging $TAG..."
        "${G[@]}" tag "$TAG"
        "${G[@]}" push "$MAIN_REMOTE" "$TAG"
    fi

    if command -v gh >/dev/null 2>&1; then
        if gh release view "$TAG" >/dev/null 2>&1; then
            info "GitHub release $TAG already exists, skipping."
        else
            # Release notes from NEWS.md: everything between "# dafr vX.Y.Z"
            # and the next "# " heading.
            NOTES="$(awk '/^# dafr '"${VERSION}"'/{found=1; next} /^# /{if(found) exit} found{print}' "$DESC_DIR/NEWS.md")"
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

    info "Pushing $DEV_BRANCH to $DEV_REMOTE..."
    guard_remote "$DEV_BRANCH" "$DEV_REMOTE" "push"
    # The dev worktree never left dev; push it straight from here.
    git push "$DEV_REMOTE" "$DEV_BRANCH"
fi

info "Done!"
