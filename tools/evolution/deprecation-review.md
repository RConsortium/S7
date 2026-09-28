# Deprecation design review

The helpers support generic renames, moves, and retirement; class and stored-property retirement; and class/property renames after rebuilding downstream packages. `new_label` also supports renaming an export while preserving the original class or generic identity. Definition-time references remain separate from warnings on use.

The tested transitions do not require further changes to #734 within its documented contract. Automatic migration of installed subclasses and warnings on every explicit property write remain outside that contract, as described below.

This review uses PR #734 at `81a9c25a5390be9ad8caf30ff2348a318bec2118`, combined with main at `cf91eb3f`. The combined source tree is `9386bfee7a9a03babc7c751b1aa8497ae97f6a94`. No changes to #734 are included in this branch. The package lab holds S7 fixed while upgrading the fixture packages; it does not test upgrading S7 itself across serialized package versions.

## Package scenarios

The executable cases live in `scenarios.R`; `results.md` records installation, namespace loading, smoke tests, and `R CMD check`. Each starts with a downstream package that works with the old upstream version, then tests both an upstream-only upgrade and a rebuilt downstream package.

| Transition               | Coverage                                                                                                                |
| ------------------------ | ----------------------------------------------------------------------------------------------------------------------- |
| Rename a generic         | Imported and external registrations; calls through both names; method lookup through the alias                          |
| Retire a generic         | Existing downstream methods still dispatch through `old`                                                                |
| Move a generic           | Compare NAMESPACE re-export, binding copy, and deprecated wrapper                                                       |
| Rename an export         | `new_label` for generic and class exports while preserving identity, including installed subclasses and saved instances |
| Rename a class           | External and direct parents; external and direct method signatures; stale and rebuilt subclasses                        |
| Move a class             | External parent construction and dispatch through the old-home alias                                                    |
| Retire a class           | Subclass construction, property types, unions, inheritance, method dispatch, explicit constructor calls                 |
| Preserve saved instances | An instance stored in the downstream namespace before a class rename, compared with an export-only rename               |
| Rename a property        | Preserved default, old constructor argument, reads, writes, inherited properties, silent printing                       |
| Retire a property        | Preserved type, default, and validator; silent construction; reads, writes, and silent printing                         |
| Write the existing value | Equal constructor arguments and unchanged writes stay silent with base warnings and both lifecycle methods              |
| Change signaling policy  | Generic calls, class constructors, renamed and retired properties, with both lifecycle methods                          |

The original breaking-change cases remain as comparisons. An `ERROR` can be intentional, for example when an export is removed, a stale subclass needs rebuilding, or `lifecycle(stop)` makes a formerly supported call fail. Read each scenario's description rather than treating the report as an all-green test suite.

## Supported transitions

### Moving a generic

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

The wrapper resolves the foreign generic in its owning namespace, so calls through either package reach the same method table. Registrations through the deprecated export also reach that table. Both stale and rebuilt evoB can dispatch through both exports. The target must remain available under its original name in the owning namespace.

A NAMESPACE re-export remains useful when the old home can stay available without warnings. Copying the foreign generic into an ordinary binding still duplicates its method table during installation; the binding-copy case remains a failing comparison. Local targets, including `old =`, retain the original object so looking up the deprecated name cannot recurse back into the wrapper.

### Renaming an export while preserving identity

Keep the original class object under the new export and supply `new_label`:

```r
Foo := new_class(properties = list(size = class_double))
Bar <- Foo
Foo := deprecated_class(new = Bar, when = "2.0.0", new_label = "Bar()")
```

The class identity remains Foo, while constructor warnings recommend `Bar()`. The `class-export-rename-deprecated` scenario verifies existing subclass dispatch and a saved instance before and after rebuilding the downstream package. The analogous generic scenario verifies both exported calls and the replacement label. `new_label` only changes the message; it does not change dispatch identity or the target.

### Preserving validation when retiring a property

`deprecated_property()` accepts `class`, `default`, and `validator` when there is no replacement:

```r
count <- deprecated_property(
  "count",
  when = "2.0.0",
  class = class_double,
  default = 1,
  validator = function(value) {
    if (any(value < 0)) "must be non-negative"
  }
)
```

The `class-retire-prop-validator` scenario verifies that negative values fail during construction and assignment, rejected assignments preserve the previous value, and valid assignments continue to work.

When renaming a property, put its validator on the replacement. The helper rejects `validator` together with `new`, preventing validation rules from being attached only to the old name. Existing computed properties or custom setters still need a custom deprecation recipe; the helper does not wrap an existing property definition.

## Compatibility limits

### Already-installed subclasses and saved instances

Creating a new class named Bar and deprecating Foo changes the class identity. An external parent can resolve to Bar while an installed subclass retains Foo in its dispatch vector. A method registered through the external reference now targets Bar, so subclass dispatch fails until the downstream package is rebuilt. Direct parents and cross-package class moves have the same rebuilding boundary.

Property renaming has a related limit even when the class name stays unchanged. In `class-rename-prop-deprecated`, a stale subclass retains the old stored `count` property definition, but its new parent stores `size` and makes `count` dynamic. Construction fails validation; rebuilding the downstream package repairs it. Resolving the parent at run time does not refresh the subclass's stored metadata.

The helper reference pages document this boundary. The aliases support rebuilding unchanged downstream source; they do not migrate installed class definitions. Saved instances also retain their old identity and may need explicit conversion or reconstruction. Renaming only the export, as above, avoids changing that identity.

### Property initialization and unchanged writes

Property setters use equality with the stored or replacement value to suppress initialization warnings. Some explicit uses are therefore silent too:

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

The `class-deprecated-prop-same-value` scenarios verify this documented exception with base warnings, `lifecycle(warn)`, and `lifecycle(stop)`. Retired properties also accept constructor arguments silently. Stop mode does not prohibit every use of the old argument or every assignment to the old property.

Guaranteeing a warning or error on every explicit use would require distinguishing constructor initialization from assignment without inferring intent from value equality. That is outside the helper's current contract; packages needing that policy require a custom constructor/property implementation.

### Other boundaries

- Changing a generic's dispatch arguments or formals still needs an adapter or a separate old generic. `deprecated_generic(new = ...)` uses the replacement's signature; it does not translate old methods.
- Argument deprecations belong in the generic's function. The helper deprecates the whole generic.
- Class retirement warns on explicit constructor calls, not on subclass definitions, method signatures, unions, or property types. Even stop mode allows those class contexts.
- `print()` and `str()` omit deprecated properties; `props()` deliberately reads them and signals deprecation.
- A warning in a plain test script need not affect `R CMD check`, but warning-as-error tests can fail and warnings during namespace loading can produce a NOTE. Stopping deprecations fail when exercised.

## Validation

All 45 package scenarios completed, including `R CMD check`, with valid version-1 baselines. The moved-generic, export-rename, and retired-validator scenarios pass for both stale and rebuilt downstream packages, with the expected deprecation warnings. Equal-value constructor arguments and writes stay silent under all three signaling policies. The report retains deliberate breaking cases and the documented rebuilding limits. Some fixture checks report unused-Imports notes unrelated to deprecation signaling.

Direct public-API probes also verify that renamed-property writes use the replacement's validator, rejected writes preserve the previous value, and supplying `validator` together with `new` is rejected.

On R 4.6.1, all 91 focused deprecation assertions passed without warnings. The combined source with this branch's documentation passed `pkgdown::check_pkgdown()` and rendered the evolution vignette, including the `new_label` and validator examples.
