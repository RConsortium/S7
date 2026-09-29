# Deprecation design review

The helpers support generic renames, moves, and retirement; deprecating a class while preserving its definition; and renaming or retiring stored properties. `deprecated_class()` keeps the original type, methods, and subclasses. Its `replacement` argument recommends another class without forwarding construction or method registration to it.

This covers a transition in which maintainers keep the original class available while downstream users adopt a replacement. Class aliases that change identity, migration of saved objects, and changes to installed property definitions need separate compatibility work. The tests identify two follow-ups for #734: fix lifecycle warning attribution through `props()`, and clarify that generated direct property defaults in an already-installed package can still call the deprecated constructor. Reproductions and suggested adjustments are below.

This review uses the two local commits on PR #734, `8bb5206f` and `f423df4c`, through `f423df4c183b459bf1007e72ed6803ba7a679970`, combined with main at `cf91eb3f10f8667ff6348b32b814b21894efe560`. The combined source tree is `5d3d844bc31ee1c94d75562e8b4e80e6d34915a3`. The lab holds S7 fixed while upgrading the fixture packages; it does not test upgrading S7 itself across serialized package versions.

## Package scenarios

The executable cases live in `scenarios.R`; `results.md` records installation, namespace loading, smoke tests, and `R CMD check`. Each starts with a downstream package that works with the old upstream version, then tests both an upstream-only upgrade and a rebuilt downstream package.

| Transition                           | Coverage                                                                                                   |
| ------------------------------------ | ---------------------------------------------------------------------------------------------------------- |
| Rename a generic                     | Imported and external registrations; calls through both names; method lookup through the alias             |
| Retire a generic                     | Existing downstream methods still dispatch through `old`                                                   |
| Move a generic                       | Compare NAMESPACE re-export, binding copy, and deprecated wrapper                                          |
| Rename a generic export              | `new_label` while preserving the generic's original identity and shared methods                            |
| Recommend a replacement class        | Direct and external parents and method signatures; distinct replacement type; explicit union methods       |
| Recommend a class in another package | Original package and class identity retained; warning names the other package                              |
| Retire a class                       | Subclass construction, property defaults, unions, inheritance, method dispatch, explicit constructor calls |
| Preserve saved instances             | An instance stored in the downstream namespace before deprecation, then saved and read as RDS              |
| Preserve a custom constructor        | Lexical defaults, validation, direct and external subclasses, local and external property defaults         |
| Deprecate a direct property type     | Compare the installed constructor's old default with one generated after rebuilding                        |
| Rename or move class identity        | Plain aliases allow rebuilding, but stale subclasses and saved instances retain old metadata               |
| Rename a property                    | Default, both constructor arguments, reads, changed writes, validation, and installed subclasses           |
| Retire a property                    | Type, default, validator, silent construction, nullable values, and installed subclasses                   |
| Inspect an object                    | `print()` and `str()` stay silent; `props()` reads deprecated properties and signals                       |
| Change signaling policy              | Base warnings and both lifecycle methods, including each property operation and silent exceptions          |

The original breaking-change cases remain as comparisons. An `ERROR` can be intentional, for example when an export is removed, a stale subclass needs rebuilding, or `lifecycle(stop)` stops a deprecated call. The property-signal cases catch and assert each expected condition, so their successful stop-mode tests report `OK`. Other stop-mode cases deliberately end with an uncaught deprecation error. The failing `class-deprecated-property-signals-warn` case is a regression to fix, described below; its assertion is retained. Read the scenario descriptions alongside the recorded results.

## Supported transitions

### Deprecating a class without changing its type

Keep the original class definition and change its defining function:

```r
Bar := new_class(properties = list(size = class_double))
Foo := deprecated_class(
  properties = list(size = class_double),
  replacement = Bar,
  when = "2.0.0"
)
```

`Foo()` warns and constructs a Foo. Existing instances and subclasses still inherit from Foo, and Foo's methods continue to apply. They do not become Bar instances, and Bar does not acquire Foo's methods. A method that supports both classes can use `Foo | Bar` as its signature.

The direct and external package scenarios verify both stale and rebuilt subclass dispatch. The saved-instance case also verifies an RDS round trip. A separate case recommends a class with a different schema, confirming that the recommendation does not adapt construction. A cross-package recommendation retains the old home and names the new package in the warning.

The custom-constructor cases preserve the original lexical default and validator, including rejection of invalid subclass values. Generated subclass constructors and external property defaults stay silent under all three signaling policies. Local property defaults defined after the deprecated class are also silent.

Omitting `replacement` retires the class without recommending another. Even stop mode leaves class contexts available: subclass definitions and generated constructors, method signatures, unions, and inheritance checks. Explicit constructor calls signal. This allows continued extension during deprecation; removing the class eventually still requires checking these downstream definitions.

### Generic aliases and package moves

`deprecated_generic(new = ...)` forwards calls and registrations to the replacement. Unlike classes, both names share their methods. Imported and external registrations work for stale and rebuilt downstream packages. `old =` retains the existing generic when there is no replacement.

For a move, the wrapper resolves the foreign generic in its owning namespace:

```r
# evoA, version 2
gen := deprecated_generic(new = evoACore::gen, when = "2.0.0")
```

Calls through either package reach the same method table, including downstream registrations. The target must remain available under its original name in its owning namespace. A NAMESPACE re-export also works when the old home can remain silent. Copying a foreign generic into an ordinary binding still duplicates its method table during installation; that comparison deliberately fails.

`new_label` applies to generics. It lets an export rename recommend the new spelling while retaining the original generic object. `deprecated_class()` instead accepts a class definition and an optional `replacement`; it has no `new`, `old`, or `new_label` arguments. A plain export alias, `Bar <- Foo`, preserves the class object and adds no deprecation signal. A warning alias that constructs another class is outside the new class helper's contract.

### Preserving property behavior

Retiring a stored property requires carrying over its `class`, `default`, and `validator`. Renaming a property requires putting validation on the replacement. The lab tests invalid constructor values and writes through both names, and verifies that a rejected write leaves the previous value intact. Supplying a validator on the deprecated alias itself is rejected by #734's input-validation tests.

The property-signal cases check reads, `props()`, changed writes (including a retired property whose old value is NULL), and conflicting old/new constructor arguments under all three policies. Base warnings and stop-mode errors match the expected behavior. Lifecycle warnings have the `props()` attribution and throttling defect below. Equal-value arguments and writes stay silent. Retired-property construction remains silent even with an explicit argument. `print()` and `str()` omit deprecated properties without invoking their getters, including in stop mode.

For lifecycle warning assertions, these cases set `lifecycle_verbosity = "warning"` so repeated direct uses each signal. The other lifecycle scenarios use the default warning policy.

## Compatibility boundaries

### Lifecycle warnings through props(): fix needed in #734

With lifecycle 1.0.5, a direct call to `props()` attributes a deprecated-property warning to the base package. A second call can be silent even with `lifecycle_verbosity = "warning"`:

```r
options(lifecycle_verbosity = "warning")
Basket := new_class(
  properties = list(
    deprecated_property(
      "count",
      class = class_double,
      default = 1,
      when = "2.0.0",
      method = "lifecycle(warn)"
    )
  )
)
x <- Basket()
props(x) # warns, attributing the use to the base package
props(x) # silent, despite the warning option
x@count # a direct read still warns
```

`props()` reads properties through `lapply()`. The caller lookup in `user_frame()` stops at that base frame, so lifecycle treats the access as an indirect use in base rather than a direct user call. This both gives the wrong attribution and prevents the warning option from making every direct call warn.

#734 should preserve the actual caller through these internal iteration frames and test both warning attribution and repeated direct calls. The lab's `class-deprecated-property-signals-warn` assertion fails at `props(x)` after an earlier direct read; both stale and rebuilt packages reproduce it, and the fixture's `R CMD check` fails. Fresh-process probes confirm the incorrect attribution for both renamed and retired properties, while direct reads, changed writes, and renamed constructor arguments warn as expected. This is a defect, not an intentional unsupported scenario. Base warnings and `lifecycle(stop)` still signal through `props()`.

### Installed direct property defaults: adjustment needed in #734

A downstream class defined before deprecation can retain a generated default that calls the exported constructor directly:

```r
# evoA, version 1
Foo := new_class(
  properties = list(size = new_property(class_double, default = 7))
)

# evoB, installed against evoA version 1
Box := new_class(properties = list(item = evoA::Foo))

# evoA, version 2
Foo := deprecated_class(
  properties = list(size = new_property(class_double, default = 7)),
  when = "2.0.0",
  method = "lifecycle(stop)"
)
```

Without rebuilding evoB, `Box()` errors because its installed default still calls `evoA::Foo()`. Base and lifecycle warning policies warn at the same point. Rebuilding evoB generates `S7::as_class(evoA::Foo)()` and restores silent construction. A default generated from `new_external_class()` already uses the silent path and does not need rebuilding for this transition.

The `class-deprecated-direct-property-default` scenarios reproduce all three policies. #734 should qualify the statement that property-class use does not warn and add this installed-package regression case. The evolution vignette now explains the boundary and recommends external references. No runtime change is needed if rebuilding these direct references is an accepted requirement; guaranteeing silence for existing compiled defaults would require a broader design change.

An explicitly written default or custom constructor that calls `Foo()` remains an ordinary deprecated call. Silencing every indirect call would hide uses that the helper is meant to report.

### Class identity changes and saved objects

A plain `Foo <- Bar` alias after defining Bar changes which type the old export names. External class references follow the alias, but a stale subclass retains Foo in its dispatch vector. Dispatch can fail until the subclass package is rebuilt. With a direct parent and method signature, the stale method can still match the old subclass while failing on a newly constructed Bar. Moving a class to another package also changes its identity.

Saved Foo instances do not become Bar instances. Recommending Bar while preserving Foo avoids breaking their old methods; it does not migrate them. The helper deliberately leaves representation conversion and rebuilding for structural changes to package authors.

### Installed property definitions

Property deprecation changes the property definition even when the class name stays the same. In the rename case, a stale subclass retains a stored `count` property while the new parent stores `size`; construction fails validation until rebuilding. In the retirement case, the stale subclass still works but reads and writes remain silent until rebuilding adopts the deprecation. A runtime parent reference does not refresh the subclass's stored property metadata.

### Signaling and adaptation limits

- Equal-value writes and constructor arguments can stay silent even under `lifecycle(stop)`. Stop mode does not prohibit every spelling of the old property name. Remove the property after migration if that is the desired end state.
- Computed properties and custom setters need deprecation signals added to their existing implementation. The property helper does not wrap an existing getter or setter.
- Generic argument deprecation belongs in the generic's function. Changes to dispatch arguments or formals need an explicit adapter or a separate old generic; `deprecated_generic(new = ...)` uses the replacement's signature unchanged.
- Warnings in a plain test script do not themselves fail `R CMD check`; warning-as-error tests can fail, and namespace-load warnings can produce a NOTE. Errors fail when exercised.

## Validation

All 59 package scenarios completed with `--check` against the source tree pinned above, on R 4.6.1 with lifecycle 1.0.5. Every version-1 baseline passed, and every upstream upgrade installed successfully. One new scenario fails its intended contract: lifecycle warning attribution and throttling through `props()`. The other recorded errors match deliberate breaking cases, stop-mode calls, and the documented rebuilding boundaries. Some fixture checks also report unused-Imports notes.

The full S7 package suite passed 1,503 assertions, including 116 deprecation assertions, with no failures, skips, or warnings. The combined source with this branch's documentation passed `pkgdown::check_pkgdown()` and rendered the evolution vignette. Pandoc emitted notices about deprecated command-line options; vignette execution succeeded. The existing package suite does not cover the newly exposed `props()` warning defect.
