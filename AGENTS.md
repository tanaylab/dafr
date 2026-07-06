# dafr Development Guide

## Branch Model

| Branch | Purpose | Remote |
|----|----|----|
| `dev` | Active development. All work happens here. | `private` (aviezerl/dafr) |
| `main` | Clean release history. One squash commit per release. | `origin` (tanaylab/dafr) |

### Rules

- **All commits go to `dev` first.** Never commit directly to `main`.
- **`main` gets squash-merged releases only.** Each release is a single
  commit on `main` with a clean summary message.
- **Tags live on `main`.** Version tags (`v0.1.0`, `v0.1.1`, …) are
  created on `main` after the squash commit.
- **`dev` must NEVER be pushed to `origin` (tanaylab).**
- **`main` must NEVER be pushed to `private` (aviezerl).**
- **Never add AI attribution to commits.** No `Co-authored-by`,
  `Co-Authored-By`, or any similar trailer referencing Claude, AI
  assistants, or LLMs in commit messages. Ever.

### Dev-only paths (MUST NOT appear on `main`)

The ship script strips these before committing on `main`:

- `dev/` — development notes, skills, benchmarks, planning docs
- `CLAUDE.md`, `AGENTS.md` — agent instruction files
- `.a5c/`, `.claude/` — agent / Claude Code session state

**When shipping to main, always use `dev/skills/dafr-ship/ship.sh`**
which handles exclusion automatically. If shipping manually, verify
after committing:
`git ls-tree -r --name-only HEAD | grep -E '^(dev/|CLAUDE\.md|AGENTS\.md)$'`
must return nothing.

## Development workflow

**Do not `R CMD INSTALL` to validate work.** Use `devtools` so the
source tree is exercised directly:

- **Document (regen `NAMESPACE`, `man/`):**
  `R --quiet --no-save -e 'devtools::document()'`
- **Run tests:** `R --quiet --no-save -e 'devtools::test()'`
  - Single file:
    `R --quiet --no-save -e 'devtools::test(filter = "query-mask-string")'`
  - Stress tests / `NOT_CRAN`-gated tests: prepend `NOT_CRAN=true`.
  - Plain testthat alternative:
    `cd tests && NOT_CRAN=true Rscript testthat.R`.
- **Full check (CRAN equivalent):**
  `R --quiet --no-save -e 'devtools::check(error_on = "note")'`
- **C++ rebuild after `src/` changes:**
  `R --quiet --no-save -e 'pkgbuild::compile_dll()'`
  (devtools::test/check do this implicitly).

Only `R CMD INSTALL` (or `devtools::install()`) when validating
*installed-package* behaviour — e.g. running benchmarks that
[`library(dafr)`](https://tanaylab.github.io/dafr/) against the real
install path. Inside the dev loop, prefer `devtools::load_all()`.

## Pushing to main (release workflow)

1.  **Ensure dev is clean and tested:**

    ``` bash
    git checkout dev
    R --quiet --no-save -e 'devtools::check(error_on = "note")'   # 0 / 0 / 0
    ```

2.  **Bump version** in `DESCRIPTION` (`Version:` line) and update
    `NEWS.md`:

    - Patch (`0.1.x`): bug fixes, performance, docs
    - Minor (`0.x.0`): new features, API additions
    - Major (`x.0.0`): breaking API changes

3.  **Commit the version bump on dev:**

    ``` bash
    git add DESCRIPTION NEWS.md
    git commit -m "release: vX.Y.Z — short description"
    ```

4.  **Ship dev onto main (preferred: use the ship script):**

    ``` bash
    bash dev/skills/dafr-ship/ship.sh "vX.Y.Z: summary of changes" --push
    ```

    The script: replaces main’s tree with dev’s via
    `git read-tree --reset -u dev`, strips dev-only paths, commits,
    pushes `main` to `origin`, tags `v$(Version)`, creates a GitHub
    release from the matching `# dafr X.Y.Z` section of `NEWS.md`, and
    finally pushes `dev` to `private`. See
    `dev/skills/dafr-ship/SKILL.md` for the manual equivalent and
    rollback steps.

## Project structure

    R/                 R sources (public API + internals)
    src/               C++ extension sources (Rcpp / cpp11 kernels, ALTREP)
    tests/testthat/    testthat suite — Julia-parity ports + R-quirk regressions
    inst/              installed assets (datasets, headers, scripts)
    benchmarks/        public benchmark drivers (run after `R CMD INSTALL`)
    vignettes/         pkgdown vignettes
    man/               roxygen-generated man pages (do not hand-edit)
    dev/               development skills, notes, internal docs (dev-only, stripped from main)

## Key conventions

- **Julia DAF.jl is the parity reference.** When R semantics diverge
  from Julia, default to aligning R with Julia rather than documenting
  the gap. See `~/src/DataAxesFormats.jl`.
- **Format API contracts are named.** `format_get_vector` /
  `format_get_matrix` and helpers must preserve
  [`names()`](https://rdrr.io/r/base/names.html) /
  [`dimnames()`](https://rdrr.io/r/base/dimnames.html) — do not strip
  names in lower layers.
- **One finding, one commit, one test** for parity audits / R-quirk
  fixes. Commit subject style: `fix(parity): <one-line>` referencing the
  Julia source location in the body.
- **Benchmarks require a fresh install.** `benchmarks/R/run-bakeoff.R`
  does [`library(dafr)`](https://tanaylab.github.io/dafr/); stale
  installs silently produce false numbers. Each iteration must call
  [`empty_cache()`](https://tanaylab.github.io/dafr/reference/empty_cache.md)
  before the timed call or hits saturate the ~0.5 ms cache lookup.
