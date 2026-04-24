# Ship: dev → main

Ship changes from the `dev` branch to a clean `main` branch via
`git read-tree`, excluding development-only files.

## Remote Layout

| Remote    | Repository        | Branch | Purpose                                          |
|-----------|-------------------|--------|--------------------------------------------------|
| `private` | `aviezerl/dafr`   | `dev`  | Development (messy history, design docs, plans)  |
| `origin`  | `tanaylab/dafr`   | `main` | Public release (clean commits, no dev files)     |

**Guardrails (enforced by the script):**

- `dev` must **never** be pushed to `origin` (tanaylab).
- `main` must **never** be pushed to `private` (aviezerl).

## Dev-Only Files (excluded from main)

Stripped during the ship:

- `dev/` — design docs, notes, plans, benchmarks, scripts, skills.
- `CLAUDE.md`, `AGENTS.md` — agent instruction files (if present).
- `.a5c/`, `.claude/` — babysitter / Claude Code session state.

## Usage

### Dry-run

```bash
bash dev/skills/dafr-ship/ship.sh
```

Leaves you on `main` with the dev-stripped tree staged, no commit.
Review the diff, then either commit manually or run with a message.

### Ship with a commit message

```bash
bash dev/skills/dafr-ship/ship.sh "Release dafr 0.1.1"
```

Commits on `main`; stays local.

### Ship + push + tag + release

```bash
bash dev/skills/dafr-ship/ship.sh "Release dafr 0.1.1" --push
```

Additionally:

1. Pushes `main` to `origin` (tanaylab/dafr).
2. Tags `v$(grep ^Version DESCRIPTION | cut -d' ' -f2)` on the new
   commit and pushes the tag (triggers any tag-based CI).
3. Creates a GitHub release with notes extracted from `NEWS.md`
   (the section matching `# dafr <Version>`).
4. Returns to `dev` and pushes to `private` (aviezerl/dafr).

### Rollback

If something goes wrong mid-ship:

```bash
git checkout -f dev          # back to dev
git branch -D main           # delete broken local main
git checkout -b main origin/main   # restore from public remote
```

## First-Time Setup (already done)

- Private remote added: `git remote add private git@github.com:aviezerl/dafr.git`.
- `dev/` un-ignored on the `dev` branch (see `.gitignore`).
- Inner `dev/.git` nested repo flattened into the outer repo on the
  `dev` branch.
- Clean orphan `main` commit force-pushed to `origin`.

## Post-Ship Housekeeping

The ship script handles push + tag + release in one pass when `--push`
is supplied. Outside of that, useful commands:

```bash
# Push the latest dev work to private without shipping:
git checkout dev && git push private dev

# Undo the last ship commit on main (if unpushed):
git checkout main && git reset --hard HEAD~1 && git checkout dev
```
