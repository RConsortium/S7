# S7 (development version)

* New `:=` operator creates and names an object in one step, so `Foo := new_class()` is equivalent to `Foo <- new_class(name = "Foo")` (#658).
* Errors thrown by S7 now report the function where they occurred, making it easier to track down the source of a problem (#646).
* `class_POSIXct` uses the `tzone` attribute (not `tz`), and allows it to be absent (#401).
* Base type wrappers like `class_integer` now define their constructor and validator in the S7 namespace. (#553).
* Method dispatch on `class_missing` now correctly handles missing arguments forwarded through a wrapper functions (#595).
* `convert()` now errors when upcasting to an abstract class, rather than producing an instance of that abstract class (#680, #686).
* `convert()` no longer automatically converts between sibling classes (classes that merely share a common ancestor); the default downcast now applies only when `to` is genuinely a descendant of `from`'s class (#509).
* `convert()` now falls back to the corresponding `as.*()` function (e.g. `as.character()`) when converting to a base type like `class_character` and no method or inheritance-based default applies, so `convert(1, class_character)` works out of the box (#472).
* `convert()` accepts a single unnamed list of property overrides when downcasting, as a shortcut for individual name-value pairs (#497).
* `convert()` no longer errors when `from` is a base or S3 object and `to` is an S7 class that inherits from `from`'s class. The base/S3 value is now passed as `.data` to the `to` constructor (#537).
* `method<-` now works for double-dispatch operators (e.g. `+`, `==`, `%*%`) with plain S3 or S4 classes, even when neither operand is an S7 object (#544).
* `method<-` no longer embeds a copy of a generic owned by another package in your package namespace. Instead it returns a sentinel value that the new `S7_on_build()` removes from the namespace at build time; call `S7_on_build()` at the top level of `zzz.R` (see `vignette("packages")`) (#364).
* `method<-` now accepts `NULL` to unregister an existing method, e.g. `method(foo, class_character) <- NULL` (#613).
* `convert()` is now idempotent when `from` is already an instance of `to`, returning it unchanged. When `from` inherits from `to` but is more specific, dispatch is now restricted to classes more specific than `to`, so an inherited downcasting method can no longer be selected in place of an upcast (#429).
* `method<-` now gives a clear error when assigning a primitive function (e.g. `log`) as a method (#608).
* `method<-` and `method()` now accept a length-1 list as `signature` for single-dispatch generics, matching the list-of-classes form required for multi-dispatch (#555).
* `new_object()` now names its first argument `_parent` to minimise the chance of a clash with a property (#423). It also accepts a single unnamed named list as a shortcut for splicing property values, making it easier to programmatically construct an object from a list of properties (#497).
* `method<-` can now register methods on S3 and S4 generics with base types (e.g. `class_character`), S3 classes (`new_S3_class()`, `class_factor`, etc.), S7 unions (expanded to one registration per class), `class_any` (registered as the `default` method), and `NULL` (registered as the `NULL` method) (#455).
* `method<-` no longer emits an "Overwriting method" message when re-registering an identical method, eliminating spurious messages from `devtools::load_all()` (#474).
* `new_class()` now errors if a child class overrides a parent property with a type that doesn't extend the parent's type, since such a class could never be instantiated. Narrowing the type is still allowed, as are dynamic (getter) properties (#352).
* `new_class()` now allows properties named `names`, `dim`, `dimnames`, `class`, `comment`, `tsp`, and `row.names`. But property names beginning with `_` are now reserved for internal use (#579).
* `new_class()` experimentally allows `class_environment` as a parent again, so you can build S7 objects that share R's reference semantics for environments. This support is provisional: because environments are mutated in place, some operations behave differently than for value-typed S7 objects, and the API may change. `S7_data()` and `S7_data<-()` error on environment-based objects, since they would otherwise destroy the object's S7 attributes in place (#590).
* `new_class()`'s default constructor now respects properties overridden in a subclass: the subclass's default is used (#467) and its setter is run during construction (#585). Values for overridden properties are passed to both the parent constructor and the new object, so a subclass can override a parent property whose default is mandatory.
* `new_external_class()` creates a delayed reference to an S7 class in another package (or your own package, but not yet defined). It is useful for registering methods on classes from suggested packages (#573) and for creating self-referential or mutually recursive classes (#250).
* `new_object()` now gives an informative error when `.parent` is a class specification rather than an instance of the parent class (#409).
* `new_object()` no longer materialises ALTREP parent values (e.g. `seq_len()`), so constructing an S7 object that wraps a large compact integer sequence is now O(1) in memory instead of O(n) (@kschaubroeck, #607).
* `new_object()` no longer re-runs property validators for properties inherited unchanged from an already-validated parent class, so constructing an instance of a deeply nested class hierarchy validates each property exactly once (#539).
* `new_property()` now runs the property class's own validator when checking a value, not just the structural class check, so a property restricted to an S3 class (e.g. `class_factor`) now enforces constraints that aren't visible in `class()` (#401).
* `new_property()` now warns when `default` is a complex value like a named vector, because such values are inlined into the constructor and can cause `R CMD check` failures. Wrap them in `quote()` instead. This warning will become an error in a future release (#541).
* `new_property()` now accepts a `setter` that takes `self`, `name`, and `value` making it easy to reuse the same definition for multiple properties (#552).
* `new_S3_class()` objects now work with `inherits()` (and other functions that use `nameOfClass()`) in R 4.3 and later (@lawremi, #521).
* `print(<S7_class>)` now shows property defaults inline (`= "value"`) and annotates read-only properties (`[read-only]`) (#439).
* `prop()` and `prop<-()` errors from getters and setters (including custom) now report a synthetic `<Class>@<prop>` call, making it easier to see which property triggered the error (#416, #536, #638).
* `prop()` no longer leaves an object in a broken state when a custom getter signals an error (#520, #640, #638).
* `prop<-()` no longer fails when assigning a call or symbol to a property (#511, #633, #638).
* New `prop_info()` returns a data frame summarising the properties of an S7 object or class, with one row per property and columns for name, default, class, getter, setter, and validator (#551).
* New `S7_classes()`, `S7_generics()`, and `S7_methods()` introspection helpers. `S7_classes()` and `S7_generics()` list the S7 classes / generics defined in a given environment/package (#335). `S7_methods()` list methods methods registered on a generic or all methods associated with a class (across generics in attached packages) (#435).
* `S7_dispatch()` now gives a clear error when called from a function that is not an S7 generic, e.g. `unclass(generic)()`, instead of failing with a confusing message (#684).
* `S7_class()` now returns a class specification for any R object, not just S7 objects. It returns the matching `class_*` for base types, a `new_S3_class()` wrapper for S3 objects, and the S4 class for S4 objects, so the result can be passed directly to `method()` or other S7 dispatch helpers (#559).
* `S7_class_desc()` is a new exported helper that formats a class specification as a short human-readable string (#594).
* `S7_data()` now preserves the S3 class when the S7 class inherits from an S3 class, so e.g. `S7_data()` on a data.frame subclass now returns a data.frame (#380).
* `S7_data<-()` now preserves attributes (like `names` or `dim`) from the replacement data instead of carrying over the originals, so resizing the underlying data works correctly (#478).
* `S7_error_method_not_found` now has a correct class vector without a duplicate `"error"` entry (@jjjermiah, #604).
* `S7_inherits()` and `check_is_S7()` now accept any class specification (S7 class, S7 union, S3 class, S4 class, or base type wrapper like `class_integer`), not just S7 classes (#556).
* `S7_on_load()` is the new name for `methods_register()`, giving it a nicer symmetry with `S7_on_build()`; `methods_register()` remains available for backward compatibility (#615). It no longer accumulates duplicate registration hooks when a package is loaded repeatedly (#316).
* New `S7_on_unload()`, to be called from `.onUnload()`, unregisters active methods and removes hooks added by `S7_on_load()` (#316).
* `set_props()` now names its first argument `_object` to minimise the chances of a clash with a property (#423). It also accepts a single unnamed named list as a shortcut for splicing property values, making it easier to set properties programmatically (#497).
* `str()` on S7 objects that inherit from data.frame (or other S3 classes whose underlying data has a `dim` attribute incompatible with the bare base type) no longer errors (#494).
* `super()` now works with S3 and S4 objects, not just S7 objects (#500).
* `validate()` now signals validation errors with class `S7_error_validation_failed`, so they can be caught with `tryCatch()` (#602, #605).

# S7 0.2.2

* Internal changes to support R-devel (4.6) (#592, #593, #598, #600).

# S7 0.2.1

* `props<-()` and `set_props()` gain `check`/`.check` arguments, letting you
  set properties without calling `validate()` (#574, #575).

* Internal changes to support R-devel (4.6) (#577).

# S7 0.2.0

## New features

* The default object constructor returned by `new_class()` has been updated.
  It now accepts lazy (promise) property defaults and includes dynamic properties
  with a `setter` in the constructor. Additionally, all custom property setters
  are now consistently invoked by the default constructor. If you're using S7 in
  an R package, you'll need to re-document to ensure that your documentation
  matches the updated usage (#438, #445).

* The call context of a dispatched method (as visible in `sys.calls()` and
  `traceback()`) no longer includes the inlined method and generic, resulting in
  more compact and readable tracebacks. The dispatched method call now contains
  only the method name, which serves as a hint for retrieving the method. For
  example: `method(my_generic, class_double)`(x=10, ...). (#486)

* New `nameOfClass()` method exported for S7 base classes, to enable usage like
  `inherits("foo", S7::class_character)` (#432, #458)

* Added support for more base/S3 classes (#434): `class_POSIXlt`,
  `class_POSIXt`, `class_formula`, `class_call`, `class_language`,
  and `class_name`.

* S7 provides a new automatic backward compatibility mechanism to provide
  a version of `@` that works in R before version 4.3 (#326).

## Bug fixes and minor improvements

* `new_class()` now automatically infers the package name when called from
  within an R package (#459).

* Improved error message when custom validators return invalid values (#454, #457).

* Fixed S3 methods registration across packages (#422).

* `convert()` now provides a default method to transform a parent class instance
  into a subclass, enabling class construction from a prototype (#444).

* A custom property `getter()` no longer infinitely recurses when accessing
  itself (reported in #403, fixed in #406).

* `method()`generates an informative message with class
  `S7_error_method_not_found` when dispatch fails (#387).

* `method<-()` can create multimethods that dispatch on `NULL`.

* In `new_class()`, properties can either be named by naming the element
  of the list or by supplying the `name` argument to `new_property()` (#371).

* The `Ops` generic now falls back to base Ops behaviour when one of the
  arguments is not an S7 object (#320). This means that you get the somewhat
  inconsistent base behaviour, but means that S7 doesn't introduce a new axis
  of inconsistency.

* `prop()` (#395) and `prop<-`/`@<-` (#396) have been optimized and
  rewritten in C.

* `super()` now works with Ops methods (#357).

* `validate()` is now always called after a custom property setter was invoked
  (reported in #393, fixed in #396).

# S7 0.1.1

* Classes get a more informative print method (#346).

* Correctly register S3 methods for S7 objects with a package (#333).

* External methods are now registered using an attribute of the S3 methods
  table rather than an element of that environment. This prevents a warning
  being generated during the "code/documentation mismatches" check in
  `R CMD check` (#342).

* `class_missing` and `class_any` can now be unioned with `|` (#337).

* `new_object()` no longer accepts `NULL` as `.parent`.

* `new_object()` now correctly runs the validator from abstract parent classes
  (#329).

* `new_object()` works better when custom property setters modify other
  properties.

* `new_property()` gains a `validator` argument that allows you to specify
  a per-property validator (#275).

* `new_property()` clarifies that it's the user's responsibility to return
  the correct class; it is _not_ automatically validated.

* Properties with a custom setter are now validated _after_ the setter has
  run and are validated when the object is constructed or when you call
  `validate()`, not just when you modify them after construction.

* `S7_inherits()` now accepts `class = NULL` to test if an object is any
  sort of S7 object (#347).

# S7 0.1.0

## May-July 2023

* `new_external_generic()` is only needed when you want a soft dependency
  on another package.

* `methods_register()` now also registers S3 and S4 methods (#306).

## Jan-May 2023

* Subclasses of abstract class can have readonly properties (#269).

* During construction, validation is now only performed once for each
  element of the class hierarchy (#248).

* Implemented a better filtering strategy for the S4 class hierarchy so
  you can now correctly dispatch on virtual classes (#252).

* New `set_props()` to make a modified copy of an object (#229).

* `R CMD check` now passes on R 3.5 and greater (for tidyverse
  compatibility).

* Dispatching on an evaluated argument no longer causes a crash (#254).

* Improve method dispatch failure message (#231).

* Can use `|` to create unions from S7 classes (#224).

* Can no longer subclass an environment via `class_environment` because we
  need to think the consequences of this behaviour through more fully (#253).

## Rest of 2022

* Add `[.S7_object`, `[<-.S7_object`, `[[.S7_object`, and `[[<-.S7_object`
  methods to avoid "object of type 'S4' is not subsettable" error
  (@jamieRowen, #236).

* Combining S7 classes with `c()` now gives an error (#230)

* Base classes now show as `class_x` instead of `"x"` in method print (#232)

## Mar 2022

* Exported `class_factor`, `class_Date`, `class_POSIXct`, and
  `class_data.frame`.

* New `S7_inherits()` and `check_is_S7()` (#193)

* `new_class()` can create abstract classes (#199).

* `method_call()` is now `S7_dispatch()` (#200).

* Can now register methods for double-dispatch base Ops (currently only
  works if both classes are S7, or the first argument is S7 and the second
  doesn't have a method for the Ops generic) (#128).

* All built-in wrappers around base types use `class_`. You can no longer
  refer to a base type with a string or a constructor function (#170).

* `convert()` allows you to convert an object into another class (#136).

* `super()` replaces `next_method()` (#110).

## Feb 2022

* `class_any` and `class_missing` make it possible to dispatch on absent
  arguments and arguments of any class (#67).

* New `method_explain()` to explain dispatch (#194).

* Minor property improvements: use same syntax for naming short-hand and
  full property specifications; input type automatically validated for
  custom setters. A property with a getter but no setter is read-only (#168).

* When creating an object, unspecified properties are initialized with their
  default value (#67). DISCUSS: to achieve this, the constructor arguments
  default to `class_missing`.

* Add `$.S7_object` and `$<-.S7_object` methods to avoid "object of type 'S4'
  is not subsettable" error (#204).

* Dispatch now disambiguates between S4 and S3/S7, and, optionally, between
  S7 classes in different packages (#48, #163).

* `new_generic()` now requires `dispatch_args` (#180). This means that
  `new_generic()` will typically be called without names. Either
  `new_generic("foo", "x")` for a "standard" generic, or
  `new_generic("foo", "x", function(x, y) call_method())` for
  a non-standard method.

* `new_external_generic()` now requires `dispatch_args` so we can eagerly
  check the signature.

* Revamp website. README now shows brief example and more info in
  `vignette("S7")`. Initial design docs and minutes are now articles so
  they appear on the website.

## Jan 2022

* New `props<-` for setting multiple properties simultaneously and validating
  afterwards (#149).
* Validation now happens recursively, and validates types before validating
  the object (#149)
* Classes (base types, S3, S4, and S7) are handled consistently wherever they
  are used. Strings now only refer to base types. New explicit `new_S3_class()` for
  referring to S3 classes (#134). S4 unions are converted to S7 unions (#150).
* Base numeric, atomic, and vector "types" are now represented as class unions
  (#147).
* Different evaluation mechanism for method dispatch, and greater restrictions
  on dispatch args (#141)
* `x@.data` -> `S7_data()`; probably to be replaced by casting.
* In generic, `signature` -> `dispatch_args`.
* Polished `str()` and `print()` methods
* `new_class()` has properties as 3rd argument (instead of constructor).
