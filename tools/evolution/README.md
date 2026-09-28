# Evolution compat lab

This directory contains a manually-run harness that verifies the claims made
in `vignette("evolution")`: what happens to a downstream package when an
upstream S7 package changes its generics or classes.

Each scenario in `scenarios.R` defines an upstream package (`evoA`) at
versions 1.0.0 and 2.0.0, plus a downstream package (`evoB`) written against
1.0.0. The runner installs the fixtures into a temporary library and records
what happens at each stage: installing evoB, loading it, running its smoke
test — both with a stale evoB (only evoA upgraded) and with evoB rebuilt
against evoA 2.0.0.

The lab includes deprecation helpers with imported and deferred registrations,
class aliases, package moves, saved instances, property validators, and lifecycle
warning/error policies. It also checks `new_label` when renaming an export while
preserving its class or generic identity. See
[deprecation-review.md](deprecation-review.md) for the design assessment and
compatibility limits.

## Usage

From the S7 package root:

```sh
# Install + load + smoke test for every scenario
Rscript tools/evolution/run.R

# Also run R CMD check on evoB for every scenario (slower)
Rscript tools/evolution/run.R --check

# Run independent scenarios in four worker processes
Rscript tools/evolution/run.R --check --jobs=4

# Run a subset of scenarios
Rscript tools/evolution/run.R gen-add-arg class-rename

# Install S7 from somewhere other than the current directory
Rscript tools/evolution/run.R --s7=../S7-other-branch
```

Results are written to `results.md` (committed, so changes show up in review)
and full per-stage logs to `logs/` (ignored).

Set `S7_EVOLUTION_SOURCE` to a commit or source description when testing another
branch, so the report records which implementation was used. Set
`S7_EVOLUTION_WORK` to retain fixture libraries for inspection after the run.
The runner requires withr for restoring its working directory, and lifecycle
for the lifecycle scenarios.

Every scenario must install and pass its smoke test against evoA 1.0.0; a
broken baseline stops the run. After upgrading evoA, expected errors are
recorded, including deliberate breaking changes and transitions that require
rebuilding downstream packages. If rebuilding evoB fails, subsequent rebuilt
load/test stages are `SKIPPED`: R restores the old installation, which must
not be mistaken for a successfully rebuilt package. A filtered run replaces `results.md` with just
that subset; run the complete lab before updating the committed report.

## When to run it

* Before each release (it's in the `release_bullets()` checklist): re-run and
  check that `results.md` is unchanged. If it changed, S7's cross-package
  behavior changed — update `vignette("evolution")` to match.
* When changing method registration, `check_method()`, constructors, or
  external generics/classes.
