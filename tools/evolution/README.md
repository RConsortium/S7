# Evolution compat lab

This directory contains a manually-run harness that verifies the claims made
in `vignette("evolution")`: what happens to a downstream package when an
upstream S7 package changes its generics or classes.

There is one scenario per "Changing a ..." section of the vignette, named
after the section it verifies. Where a section describes several transitions
(such as a bare change versus the recommended deprecation path), the scenario
packs each variant into the upstream package as a separate generic or class,
and the smoke test asserts the expected outcome for each.

Each scenario in `scenarios.R` defines an upstream package (`evoA`) at
versions 1.0.0 and 2.0.0, plus a downstream package (`evoB`) written against
1.0.0. The runner installs the fixtures into a temporary library and records
what happens at each stage: installing evoB, loading it, running its smoke
test — both with a stale evoB (only evoA upgraded) and with evoB rebuilt
against evoA 2.0.0.

## Scenarios

| Scenario | Vignette section |
|---|---|
| `gen-change-dispatch` | Changing dispatch |
| `gen-add-arg` | Adding an argument |
| `gen-remove-arg` | Removing an argument |
| `gen-change-default` | Changing a default |
| `gen-rename` | Renaming a generic |
| `gen-remove` | Removing a generic |
| `gen-move-package` | Moving a generic to another package |
| `class-add-prop` | Adding a property |
| `class-remove-prop` | Removing a property |
| `class-rename-prop` | Renaming a property |
| `class-change-prop-type` | Changing a property's type or validator |
| `class-rename` | Renaming a class |
| `class-remove` | Removing a class |
| `class-move` | Moving a class |

## Results

From the committed `results.md` (R 4.6.1, S7 working tree, install + test +
R CMD check). Every scenario installs and passes its smoke test against evoA
1.0.0; the interesting results are what evoA 2.0.0 changes.

| Scenario | Outcome with evoA 2.0.0 |
|---|---|
| `gen-change-dispatch` | Deliberately broken: calls through the stale no-`...` generic fail, evoB can no longer install (multidispatch registration needs a list signature), and R CMD check errors. There is no compatible version of evoB. |
| `gen-add-arg` | Fully compatible; R CMD check reports one NOTE for the method that lacks the new argument. |
| `gen-remove-arg` | Fully compatible; one NOTE while the deprecated argument remains in the generic. |
| `gen-change-default` | Fully compatible; one NOTE; callers receive the generic's new default, not the method's. |
| `gen-rename` | A bare rename breaks callers at run time; `deprecated_generic()` aliases keep registration and calls working through both names (warning through the old one), including deferred external registration and `new_label`. |
| `gen-remove` | `deprecated_generic()` without `new` keeps registration and calls working (warning on calls), preserving a custom function's lexical scope and defaults. |
| `gen-move-package` | NAMESPACE re-export and deprecated alias both work; re-export by assignment breaks dispatch through evoA once evoB is rebuilt. |
| `class-add-prop` | Compatible, stale or rebuilt; a subclass whose property clashes with the new parent property errors with "must narrow". |
| `class-remove-prop` | Outright removal fails at run time (unused argument, missing property); `deprecated_property()` keeps construction silent, warns on reads and changed writes, and preserves the default and validator. |
| `class-rename-prop` | `deprecated_property(new = )` warns and delegates through the old name; stale subclasses fail validation until rebuilt. |
| `class-change-prop-type` | Narrowing the type fails at run time for now-invalid values; warning in the validator is the safe transition. |
| `class-rename` | `deprecated_class(new = )` warns on construction but preserves the identity of subclasses, methods, and saved instances; replacement classes need their own or union methods. |
| `class-remove` | `deprecated_class()` without `new` keeps subclasses, property types, unions, and dispatch silent; only constructor calls warn. A stale direct property default warns until rebuilt. |
| `class-move` | `deprecated_class(new = evoACore::Foo)` preserves `evoA::Foo`'s identity; the warning names the new home. |

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

Set `S7_EVOLUTION_SOURCE` to a commit or source description when testing
another branch, so the report records which implementation was used. Set
`S7_EVOLUTION_WORK` to retain fixture libraries for inspection after the run.
The runner requires withr for restoring its working directory.

Every scenario must install and pass its smoke test against evoA 1.0.0; a
broken baseline stops the run. After upgrading evoA, expected errors are
recorded, including deliberate breaking changes and transitions that require
rebuilding downstream packages. If rebuilding evoB fails, subsequent rebuilt
load/test stages are `SKIPPED`: R restores the old installation, which must
not be mistaken for a successfully rebuilt package. A filtered run replaces
`results.md` with just that subset; run the complete lab before updating the
committed report.

## When to run it

* Before each release (it's in the `release_bullets()` checklist): re-run and
  review changes in `results.md`. If the recorded behavior changed, update
  `vignette("evolution")` to match. The date and source metadata also change
  between runs.
* When changing method registration, `check_method()`, constructors, or
  external generics/classes.
