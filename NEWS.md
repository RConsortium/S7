# S7 1.0.0

## Breaking changes

* `convert()` now errors when upcasting to an abstract class. Use a concrete
  target class instead (#680, #686).

* `convert()` now restricts default downcasts to descendants of the source
  class. Converting between sibling classes requires an explicit `convert()`
  method, rather than relying on a shared ancestor (#509).

* `method<-()` now leaves a temporary placeholder, rather than a copy of an
  external generic, in your package namespace. Add `S7_on_build()` at the top
  level of `zzz.R`, after all method registrations, to remove these placeholders
  when the package is built (#364).

* `new_class()` changes the default constructor for subclasses of classes with
  custom constructors. The subclass constructor now takes `...`, followed by
  named arguments for its new properties. It forwards `...` to the parent
  instead of exposing the parent's properties as individual arguments. Update
  calls to use the parent constructor's arguments, and re-document affected
  constructors (#609, #317).

* `new_class()` now calls constructors of S7 parents from other packages at run time when the subclass is defined in a package, so subclasses use the installed parent constructor without needing `new_external_class()`. Such subclass constructors now accept parent arguments through `...` (#763).

* `new_class()` now reserves property names beginning with `_` for internal use.
  Rename any properties that use this prefix (#579).

* `new_object()` and `set_props()` now name their first arguments `_parent` and
  `_object`, respectively. Update calls that name these arguments, or pass the
  first argument positionally (#423).

## New features

### Classes and properties

* New `:=` operator creates and names an object in one step:
  `Foo := new_class()` is equivalent to `Foo <- new_class(name = "Foo")`. S7's
  `:=` takes precedence over rlang and data.table regardless of attachment
  order, without emitting masking messages (#658, #697).

* `new_class()` now supports S4 classes as parents, mapping S4 slots to S7
  properties and registering the new class with S4 automatically. Conversely,
  `S4_register()` registers an S7 class with S4, and `S4_contains()` supplies a
  class name for `methods::setClass(contains = )`, exposing stored S7 properties
  as slots for S4 subclasses. This includes integration with S4 initialization,
  validity checking, and S4/internal generic registration. See
  `vignette("compatibility")` for details (#456).

* `new_class()` now allows properties named `names`, `dim`, `dimnames`, `class`,
  `comment`, `tsp`, and `row.names`. Property names beginning with `_` are
  reserved for internal use (#579).

* `new_class()` experimentally supports `class_environment` as a parent,
  allowing S7 objects with reference semantics. Environments are modified in
  place, so some operations differ from those on value-typed objects, and the
  API may change. `S7_data()` and `S7_data<-()` error on these objects because
  they would otherwise remove the S7 attributes in place (#590).

* `new_external_class()` creates a delayed reference to an S7 class in another
  package, or a class in your own package that is not yet defined. This supports
  method registration for suggested packages (#573), self-referential and
  mutually recursive classes (#250).

* `new_object()` now accepts a named list of property values, passed as a single
  unnamed argument through `...` (#497). Its first argument is now named
  `_parent` to avoid clashes with property names (#423).

* `new_property()` accepts a `setter` with arguments `self`, `name`, and
  `value`, so the same setter can be reused for multiple properties (#552).

* `new_S3_class()` gains a `default` argument for supplying a quoted property
  default independently of its constructor (#755).

* `new_S3_class()` gains an optional `attributes` argument declaring the complete set of attributes to preserve when constructing S7 subclasses. The bundled concrete S3 wrappers declare their attributes, so `new_object()` strips foreign attributes from their subclasses (#760).

* `set_props()` now accepts a named list of property values, passed as a single
  unnamed argument through `...` (#497). Its first argument is now named
  `_object` to avoid clashes with property names (#423).

### Generics and methods

* `method()` and `method<-()` accept a length-one list as `signature` for
  single-dispatch generics, matching the list-of-classes form used for multiple
  dispatch (#555).

* `method<-()` can register methods on S3 and S4 generics for base types, S3
  classes, S7 unions, `class_any`, and `NULL`. Unions expand to one registration
  per class; `class_any` and `NULL` register as the `default` and `NULL`
  methods, respectively (#455).

* `method<-()` supports double-dispatch operators such as `+`, `==`, and `%*%`
  with plain S3 or S4 classes, even when neither operand is an S7 object (#544).
  It also supports unary `+`, `-`, and `!` methods (#531).

* `method<-()` accepts `NULL` to unregister an existing method, e.g.
  `method(foo, class_character) <- NULL` (#613).

* `super()` now works with S3 and S4 objects, not just S7 objects (#500).

### Conversion

* `convert()` falls back to the corresponding `as.*()` function when converting
  to a base type and no method or inheritance-based default applies. For
  example, `convert(1, class_character)` uses `as.character()` (#472).

* `convert()` now accepts a named list of property overrides when downcasting,
  passed as a single unnamed argument through `...` (#497).

* New `convert_lazy()` is a non-strict variant of `convert()` that returns
  `from` unchanged if it already inherits from `to`, preserving the more
  specific class and any extra properties instead of stripping them (#428).

### Introspection and debugging

* `print()` for S7 classes now shows property defaults inline and marks
  read-only properties with `[read-only]` (#439).

* New `prop_info()` returns a data frame describing an object's or class's
  properties, with one row per property and columns for name, default, class,
  getter, setter, and validator (#551).

* `S7_class()` returns a class specification for any R object, not just S7
  objects: a `class_*` wrapper for base types, a `new_S3_class()` wrapper for S3
  objects, or an S4 class for S4 objects. The result can be passed directly to
  `method()` and other S7 dispatch helpers (#559).

* New `S7_class_desc()` formats a class specification as a short, human-readable
  string (#594).

* New `S7_classes()` and `S7_generics()` list the S7 classes and generics
  defined in an environment or package (#335).

* New `S7_generic_call()`, `S7_generic_fun()`, and `S7_user_frame()` provide
  access to a method's generic call, generic function, and caller frame (#596).

* `S7_inherits()` and `check_is_S7()` accept any class specification, including
  S7 unions, S3 and S4 classes, and base type wrappers (#556).

* New `S7_methods()` lists methods registered on a generic, or methods
  associated with a class across generics in attached packages (#435).

* `trace()` and `untrace()` work with S7 generics and methods, enabling standard
  debugging workflows. For example,
  `trace("myclass", browser, where = my_generic@methods)` sets a breakpoint in
  the method for `myclass` (#584).

### Package development

* New `vignette("evolution")` explains how to evolve S7 classes and generics 
  without breaking downstream packages, and what downstream authors can do to 
  smooth transitions (#143).

* New `deprecated_class()`, `deprecated_generic()`, and `deprecated_property()`
  help package authors deprecate APIs while keeping existing code working. A
  deprecated class warns on construction while preserving its methods and
  subclasses; an optional `new` is recommended in the warning. A
  deprecated generic can warn or forward calls and method registrations to a
  replacement supplied as `new`. Deprecated properties can forward reads and
  writes to a replacement property. Warnings can use base R or lifecycle (#727,
  #730).

* New `S7_on_build()` removes the temporary placeholders returned by
  `method<-()` for generics owned by other packages, avoiding embedded copies of
  those generics in your namespace. Call it at the top level of `zzz.R`, after
  all method registrations, not inside `.onLoad()`. See `vignette("packages")`
  for details (#364).

* `S7_on_load()` is the new name for `methods_register()`, which remains
  available for backward compatibility. Call it from `.onLoad()` to register
  methods (#615).

* New `S7_on_unload()` unregisters active methods and removes hooks added by
  `S7_on_load()`. Call it from `.onUnload()` (#316).

## Performance

* `new_object()` is faster thanks to cached class names and dispatch vectors and
  faster class type detection. In benchmarks, construction is 1.4x faster for a
  direct subclass of `S7_object` and 2.4x faster for a class with 10 ancestors.
  `S7_inherits()` is 1.5x faster and `super()` is 2.1x faster (#723).

* `new_object()` stores a shared internal class reference instead of copying the
  class on each construction. Objects serialized together also share this
  reference. Constructors created by older versions of S7 continue to work
  (#742).

* `new_object()` preserves ALTREP parent values, so wrapping a large compact
  integer sequence uses O(1) rather than O(n) memory (@kschaubroeck, #607).

* `new_object()` avoids re-running validators for properties inherited unchanged
  from an already-validated parent, so each property is validated only once when
  constructing deeply nested subclasses (#539).

* `new_object()` and `validate()` are 25-30% faster through direct access to
  class metadata. Avoiding duplicate checks for base type properties also makes
  construction 1.4x faster for classes with 10 such properties and 1.6x faster
  for classes with 50. On R 4.3 and later, S7 uses base R's `@` directly (#723).

## Bug fixes and minor improvements

### Classes and construction

* Base type wrappers such as `class_integer` now define their constructors and
  validators in the S7 namespace (#553).

* `class_POSIXct` uses the `tzone` attribute, rather than `tz`, and allows it to
  be absent (#401).

* `new_class()` now rejects property overrides whose type does not extend the
  parent's property type, since such classes cannot be instantiated (#352,
  #708).

* `new_class()` generates compact property defaults for bundled concrete S3
  wrappers, keeping constructor bodies out of generated documentation (#755).

* `new_class()` generates a working default constructor when the parent has a
  custom constructor. It forwards `...` to the parent, allowing the parent to
  match and evaluate its own argument defaults. Properties added by the subclass
  follow `...` as named arguments (#609, #317).

* `new_class()` uses the subclass's default and setter for overridden
  properties. Override values are passed to both the parent constructor and the
  new object, allowing subclasses to override parent properties with mandatory
  defaults (#467, #585).

* `new_external_class()` accepts exported aliases, so references continue to
  work when an old name points to a renamed or moved class (#727).

* `new_object()` allows an abstract class's constructor to run while
  constructing the parent part of a subclass. This supports subclasses of
  abstract classes in other packages referenced through `new_external_class()`
  (#717).

* `new_object()` gives an informative error when `_parent` is a class
  specification rather than an instance of the parent class (#409).

* `new_object()` now strips foreign attributes, including properties of sibling classes, when constructing classes rooted in `S7_object` (#760).

* `new_S3_class()` objects work with `inherits()` and other functions that use
  `nameOfClass()` on R 4.3 and later (@lawremi, #521).

* `S7_class()` reads class metadata from the internal `_S7_class` attribute,
  previously named `S7_class`, avoiding collisions with user-defined properties.
  Objects created by older versions of S7, including saved objects and objects
  in installed packages, remain supported. Use `S7_class()` rather than
  accessing the attribute directly (#677).

### Conversion and underlying data

* `convert()` returns `from` unchanged when it already has the target class.
  When upcasting a more specific subclass, dispatch is restricted to classes
  more specific than `to`, so an inherited downcasting method is not selected in
  place of an upcast (#429).

* `convert()` now errors when upcasting to an abstract class instead of creating
  an instance of that class (#680, #686).

* `convert()` only applies the default downcast when `to` is a descendant of the
  source class, rather than converting between sibling classes (#509).

* `convert()` supports conversion from base and S3 objects to S7 subclasses of
  their class, passing the source value as `.data` to the target constructor
  (#537).

* `S7_data()` preserves the S3 class when the S7 class inherits from an S3
  class. For example, extracting data from an S7 subclass of data.frame returns
  a data.frame (#380).

* `S7_data<-()` preserves attributes such as `names` and `dim` from the
  replacement data, rather than retaining the originals, so resizing the
  underlying data works correctly (#478).

### Generics and method registration

* Dispatch on `class_missing` correctly handles missing arguments forwarded
  through wrapper functions (#595).

* Operator methods propagate missing-method errors raised inside their bodies
  instead of silently falling back to base behavior (#490).

* `method<-()` gives a clear error when a primitive function, such as `log`, is
  assigned as a method (#608).

* `method<-()` silently accepts re-registration of an identical method, avoiding
  spurious "Overwriting method" messages from `devtools::load_all()` (#474).

* `method<-()` checks method signatures for consistency with their generics only
  in development contexts: during `pkgload::load_all()`, when `R CMD check`
  checks a package involved in the registration, or when registering a method
  outside a package. Users of installed packages no longer receive these
  warnings or errors after an upstream generic changes. Registrations inside
  testthat tests are treated as an end-user context, keeping local tests
  consistent with `R CMD check` (#726, #728).

* `method<-()` warns and skips registration of incompatible methods during
  `pkgload::load_all()`, rather than stopping with an error. This lets a package
  remain sourceable while its methods are updated to match a changed generic
  (#726).

* `S7_on_load()` avoids accumulating duplicate registration hooks when a package
  is loaded repeatedly (#316).

* `S7_on_load()` warns and skips registration when an upstream generic has been
  renamed or removed, allowing the downstream package to load. It also resolves
  generics through package exports, so generics can move between packages and be
  re-exported without breaking installed downstream packages (#729).

### Properties and validation

* `new_property()` runs the property class's own validator when checking values,
  in addition to checking their class. Properties restricted to an S3 class such
  as `class_factor` now enforce constraints that are not visible in `class()`
  (#401).

* `new_property()` warns when `default` is a complex value, such as a named
  vector, that would be inlined into the constructor and could cause
  `R CMD check` failures. Wrap these defaults in `quote()`. This warning will
  become an error in a future release (#541).

* `prop()` keeps objects usable after a custom getter signals an error (#520,
  #640, #638).

* `prop<-()` supports assigning calls and symbols to properties (#511, #633,
  #638).

### Errors and printing

* Errors from S7 report the function where they occurred (#646).

* `S7_error_method_not_found` has a correct class vector without a duplicate
  `"error"` entry (@jjjermiah, #604).

* `print()` and `str()` omit properties created by `deprecated_property()`
  (#754).

* `prop()` and `prop<-()` report errors from getters and setters with a
  synthetic `<Class>@<prop>` call, identifying the property that triggered the
  error (#416, #536, #638).

* `S7_dispatch()` gives a clear error when called from a function that is not an
  S7 generic, such as `unclass(generic)()` (#684).

* `str()` works for S7 objects that inherit from data.frame and other S3 classes
  whose `dim` attribute is incompatible with the bare underlying type (#494).

* `validate()` signals validation errors with class
  `S7_error_validation_failed`, allowing them to be caught with `tryCatch()`
  (#602, #605).

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
