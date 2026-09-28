# Deprecation design review

The helpers cover same-package generic renames and retirement, class and stored-property retirement, and class/property renames after rebuilding downstream packages. They keep definition-time references separate from warnings on use. The main implementation gap is moving a generic across package namespaces. Property signaling and validation also need narrower documentation or API changes before the helpers can cover all the transitions described below.

This review uses PR #734 at `5c2d72a48d9108262e95655eede3a520fc0147d8`, combined with main at `cf91eb3f`. The combined source tree is `c556fcbec55207f1b457fcddc4b42867a0531652`. No changes to #734 are included in this branch. The package lab holds S7 fixed while upgrading the fixture packages; it does not test upgrading S7 itself across serialized package versions.

## Package scenarios

The executable cases live in `scenarios.R`; `results.md` records installation, namespace loading, smoke tests, and `R CMD check`. Each starts with a downstream package that works with the old upstream version, then tests both an upstream-only upgrade and a rebuilt downstream package.

| Transition               | Coverage                                                                                                |
| ------------------------ | ------------------------------------------------------------------------------------------------------- |
| Rename a generic         | Imported and external registrations; calls through both names; method lookup through the alias          |
| Retire a generic         | Existing downstream methods still dispatch through `old`                                                |
| Move a generic           | Compare NAMESPACE re-export, binding copy, and deprecated wrapper                                       |
| Rename a class           | External and direct parents; external and direct method signatures; stale and rebuilt subclasses        |
| Move a class             | External parent construction and dispatch through the old-home alias                                    |
| Retire a class           | Subclass construction, property types, unions, inheritance, method dispatch, explicit constructor calls |
| Preserve saved instances | An instance stored in the downstream namespace before a class rename                                    |
| Rename a property        | Preserved default, old constructor argument, reads, writes, inherited properties, silent printing       |
| Retire a property        | Preserved type and default, silent construction, reads, writes, silent printing                         |
| Write the existing value | Regression case requiring a warning on an explicit deprecated write                                     |
| Change signaling policy  | Generic calls, class constructors, renamed and retired properties, with both lifecycle methods          |

The original breaking-change cases remain as comparisons. An `ERROR` can be intentional, for example when an export is removed or `lifecycle(stop)` makes a formerly supported call fail. Read each scenario's description rather than treating the report as an all-green test suite.

## Adjustments to consider for #734

### Resolve a moved generic in its owning namespace

The `gen-move-package-deprecated` case installs three real packages:

```r
# evoACore, version 2
gen := new_generic("x")

# evoA, version 2
gen := deprecated_generic(new = evoACore::gen, when = "2.0.0")

# evoB, installed before or after the move
# NAMESPACE: importFrom(evoA, gen)
BClass := new_class()
method(gen, BClass) <- function(x, ...) "B"
```

The wrapper retains the foreign generic object in its closure. Installing evoA serializes a separate method table, just as the existing binding-copy scenario does. A stale evoB registers through the old home and reaches the copy; a rebuilt evoB records the target's new home and registers in evoACore. The two exported calls do not consistently see the same methods. The smoke test requires both names to dispatch and fails for both stale and rebuilt evoB.

This needs an implementation change if cross-package moves remain part of `deprecated_generic()`'s contract. Resolve a foreign target through its owning namespace for calls, registration, and introspection, using the existing external-generic resolution machinery. Preserve the original local target for `old =`: resolving its name after the binding has become the deprecated wrapper would recurse. Add installed-package coverage; an in-session alias test cannot expose the serialized table split.

Until then, the vignette recommends a NAMESPACE re-export, which passes the package scenario.

### Define the boundary for already-installed subclasses

With `Foo := deprecated_class(new = Bar, when = "2.0.0")`, an external parent resolves to Bar and a stale subclass can construct. However, its stored dispatch vector still includes the old Foo identity, while an external method signature resolves to Bar. Dispatch on the subclass fails; rebuilding the downstream package repairs it. The same problem occurs when the class moves to another package.

The alias therefore supports rebuilding unchanged downstream source, not transparent migration of every installed subclass. Rebuilding is a reasonable documented boundary for #734; automatically updating cached class identities would be a broader change. If the intended contract includes upstream-only upgrades without rebuilding, that needs additional implementation and tests. Saved instances remain a separate data-migration problem even after downstream code is rebuilt.

Property renaming has a related failure even though the class name stays unchanged. In `class-rename-prop-deprecated`, a stale subclass retains the old stored `count` property definition, but its new parent stores `size` and makes `count` dynamic. Constructing the subclass fails with `@count must be <double>, not <NULL>`. Rebuilding repairs it. A runtime reference to the parent does not refresh the subclass's stored property metadata. This limit also needs documentation in #734, or an implementation change if property deprecation is intended to work without rebuilding installed subclasses.

There is also a useful transition that preserves the original class object: export `Bar <- Foo`, then deprecate the old export. The class identity stays Foo, so its existing subclasses and instances remain compatible. However, `Foo := deprecated_class(new = Bar, when = "2.0.0")` currently tells the caller to use `Foo()` instead of `Foo()`, because it takes the replacement label from the class's internal name. This was reproduced directly. An explicit replacement label would let this safer rename use the helper without misleading advice. Without that addition, keeping both exports as silent aliases is an option.

### Distinguish explicit property use from default initialization

Both property setters use equality with the stored or replacement value to suppress initialization warnings. That also suppresses explicit writes of the current value. For a renamed property, explicitly supplying the deprecated constructor argument equal to the replacement is silent too.

```r
Basket := new_class(
  properties = list(
    size = class_double,
    deprecated_property(
      "count",
      new = "size",
      when = "2.0.0",
      method = "lifecycle(stop)"
    )
  )
)
x <- Basket(size = 2, count = 2) # succeeds
x@count <- 2 # succeeds
x@count <- 3 # deprecation error
x@count # deprecation error
```

The `class-deprecated-prop-same-value` scenario fails its warning assertion on this behavior. The stop-mode example above was also exercised directly. Retired properties similarly accept construction and unchanged writes in stop mode. Construction without a replacement is already documented as silent; unchanged writes and equal constructor arguments need the same clarity.

To promise warnings or errors on every explicit use, the setter needs construction context that does not infer intent from equality. Otherwise, document the narrower contract in #734's helper reference pages and avoid presenting `lifecycle(stop)` as a complete ban on supplying the old property. The vignette now describes the actual boundary.

### Preserve property-specific validation when retiring a property

`deprecated_property()` accepts `class` and `default`, but no `validator`. A renamed property's validation can live on the replacement property. A retired property has no replacement on which to keep that validator.

```r
# Before
count <- new_property(
  class_double,
  default = 1,
  validator = function(value) if (value < 0) "must be non-negative"
)

# Copying the available arguments loses the validator.
count <- deprecated_property(
  "count",
  when = "2.0.0",
  class = class_double,
  default = 1
)
```

A class using the second property accepts a negative value; supplying `validator` to the helper gives an unused-argument error. Both were verified directly. Consider accepting `validator` and forwarding it to `new_property()` so retirement can preserve the property's contract. Existing computed properties or custom setters also need a custom deprecation recipe; the helper is not a general wrapper around an existing property definition.

## Intentional limits

- Changing a generic's dispatch arguments or formals still needs an adapter or a separate old generic. `deprecated_generic(new = ...)` uses the replacement's signature; it does not translate old methods.
- Argument deprecations belong in the generic's function. The helper deprecates the whole generic.
- Class retirement warns on explicit constructor calls, not on subclass definitions, method signatures, unions, or property types. Even stop mode allows those class contexts. This separation is useful when retiring direct construction while keeping an abstract parent.
- `print()` and `str()` omit deprecated properties; `props()` deliberately reads them and signals deprecation.
- A warning in a plain test script need not affect `R CMD check`, but warning-as-error tests can fail and warnings during namespace loading can produce a NOTE. Stopping deprecations fail when exercised.

## Validation

All 40 package scenarios completed, including `R CMD check`, with valid version-1 baselines. Same-package generic rename and retirement, class retirement, and rebuilt class/property aliases worked in warning mode. The report retains the deliberate breaking cases, stop-mode failures, and the unresolved regressions described above. Several fixture checks also report an unused-Imports NOTE for `evoA`; those notes are unrelated to deprecation signaling.

On R 4.6.1, the focused `deprecated`, `external-class`, `external-generic`, and `class-spec` tests passed all 294 assertions without warnings. Direct public-API probes also covered alias chains, lazy forwarding, method lookup and removal, conversion through a class alias, preservation of a renamed property's validator, `props()`, and the property and naming limitations above.

The combined source with the updated vignette passed `pkgdown::check_pkgdown()` and rendered the vignette. `R CMD check` completed with 0 errors, 0 check warnings, and 2 notes, concerning `methods:::assignClassDef` and the non-API R call `Rf_findVarInFrame`. Its test log reported 1,443 passing assertions and two fixture-install warnings from `local_dev_S7_lib()` in the binding and operator tests: those fixtures attempted to install the check directory. These are recorded separately from the check's top-level warning count.
