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

## Usage

From the S7 package root:

```sh
# Fast mode: install + load + smoke test for every scenario (~2 min)
Rscript tools/evolution/run.R

# Also run R CMD check on evoB for every scenario (slower)
Rscript tools/evolution/run.R --check

# Run a subset of scenarios
Rscript tools/evolution/run.R gen-add-arg class-rename

# Install S7 from somewhere other than the current directory
Rscript tools/evolution/run.R --s7=../S7-other-branch
```

Results are written to `results.md` (committed, so changes show up in review)
and full per-stage logs to `logs/` (ignored).

## When to run it

* Before each release (it's in the `release_bullets()` checklist): re-run and
  check that `results.md` is unchanged. If it changed, S7's cross-package
  behavior changed — update `vignette("evolution")` to match.
* When changing method registration, `check_method()`, constructors, or
  external generics/classes.
