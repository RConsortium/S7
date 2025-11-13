# Changelog

## S7 (development version)

- `props<-()` and
  [`set_props()`](https://rconsortium.github.io/S7/reference/props.md)
  gain `check`/`.check` arguments, letting you set properties without
  calling
  [`validate()`](https://rconsortium.github.io/S7/reference/validate.md)
  ([\#574](https://github.com/RConsortium/S7/issues/574),
  [\#575](https://github.com/RConsortium/S7/issues/575)).

- Internal changes to support R-devel (4.6)
  ([\#577](https://github.com/RConsortium/S7/issues/577)).

## S7 0.2.0

CRAN release: 2024-11-07

### New features

- The default object constructor returned by
  [`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md)
  has been updated. It now accepts lazy (promise) property defaults and
  includes dynamic properties with a `setter` in the constructor.
  Additionally, all custom property setters are now consistently invoked
  by the default constructor. If you’re using S7 in an R package, you’ll
  need to re-document to ensure that your documentation matches the
  updated usage ([\#438](https://github.com/RConsortium/S7/issues/438),
  [\#445](https://github.com/RConsortium/S7/issues/445)).

- The call context of a dispatched method (as visible in
  [`sys.calls()`](https://rdrr.io/r/base/sys.parent.html) and
  [`traceback()`](https://rdrr.io/r/base/traceback.html)) no longer
  includes the inlined method and generic, resulting in more compact and
  readable tracebacks. The dispatched method call now contains only the
  method name, which serves as a hint for retrieving the method. For
  example: `method(my_generic, class_double)`(x=10, …).
  ([\#486](https://github.com/RConsortium/S7/issues/486))

- New [`nameOfClass()`](https://rdrr.io/r/base/class.html) method
  exported for S7 base classes, to enable usage like
  `inherits("foo", S7::class_character)`
  ([\#432](https://github.com/RConsortium/S7/issues/432),
  [\#458](https://github.com/RConsortium/S7/issues/458))

- Added support for more base/S3 classes
  ([\#434](https://github.com/RConsortium/S7/issues/434)):
  `class_POSIXlt`, `class_POSIXt`, `class_formula`, `class_call`,
  `class_language`, and `class_name`.

- S7 provides a new automatic backward compatibility mechanism to
  provide a version of `@` that works in R before version 4.3
  ([\#326](https://github.com/RConsortium/S7/issues/326)).

### Bug fixes and minor improvements

- [`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md)
  now automatically infers the package name when called from within an R
  package ([\#459](https://github.com/RConsortium/S7/issues/459)).

- Improved error message when custom validators return invalid values
  ([\#454](https://github.com/RConsortium/S7/issues/454),
  [\#457](https://github.com/RConsortium/S7/issues/457)).

- Fixed S3 methods registration across packages
  ([\#422](https://github.com/RConsortium/S7/issues/422)).

- [`convert()`](https://rconsortium.github.io/S7/reference/convert.md)
  now provides a default method to transform a parent class instance
  into a subclass, enabling class construction from a prototype
  ([\#444](https://github.com/RConsortium/S7/issues/444)).

- A custom property `getter()` no longer infinitely recurses when
  accessing itself (reported in
  [\#403](https://github.com/RConsortium/S7/issues/403), fixed in
  [\#406](https://github.com/RConsortium/S7/issues/406)).

- [`method()`](https://rconsortium.github.io/S7/reference/method.md)generates
  an informative message with class `S7_error_method_not_found` when
  dispatch fails
  ([\#387](https://github.com/RConsortium/S7/issues/387)).

- `method<-()` can create multimethods that dispatch on `NULL`.

- In
  [`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md),
  properties can either be named by naming the element of the list or by
  supplying the `name` argument to
  [`new_property()`](https://rconsortium.github.io/S7/reference/new_property.md)
  ([\#371](https://github.com/RConsortium/S7/issues/371)).

- The `Ops` generic now falls back to base Ops behaviour when one of the
  arguments is not an S7 object
  ([\#320](https://github.com/RConsortium/S7/issues/320)). This means
  that you get the somewhat inconsistent base behaviour, but means that
  S7 doesn’t introduce a new axis of inconsistency.

- [`prop()`](https://rconsortium.github.io/S7/reference/prop.md)
  ([\#395](https://github.com/RConsortium/S7/issues/395)) and
  `prop<-`/`@<-` ([\#396](https://github.com/RConsortium/S7/issues/396))
  have been optimized and rewritten in C.

- [`super()`](https://rconsortium.github.io/S7/reference/super.md) now
  works with Ops methods
  ([\#357](https://github.com/RConsortium/S7/issues/357)).

- [`validate()`](https://rconsortium.github.io/S7/reference/validate.md)
  is now always called after a custom property setter was invoked
  (reported in [\#393](https://github.com/RConsortium/S7/issues/393),
  fixed in [\#396](https://github.com/RConsortium/S7/issues/396)).

## S7 0.1.1

CRAN release: 2023-09-17

- Classes get a more informative print method
  ([\#346](https://github.com/RConsortium/S7/issues/346)).

- Correctly register S3 methods for S7 objects with a package
  ([\#333](https://github.com/RConsortium/S7/issues/333)).

- External methods are now registered using an attribute of the S3
  methods table rather than an element of that environment. This
  prevents a warning being generated during the “code/documentation
  mismatches” check in `R CMD check`
  ([\#342](https://github.com/RConsortium/S7/issues/342)).

- `class_missing` and `class_any` can now be unioned with `|`
  ([\#337](https://github.com/RConsortium/S7/issues/337)).

- [`new_object()`](https://rconsortium.github.io/S7/reference/new_class.md)
  no longer accepts `NULL` as `.parent`.

- [`new_object()`](https://rconsortium.github.io/S7/reference/new_class.md)
  now correctly runs the validator from abstract parent classes
  ([\#329](https://github.com/RConsortium/S7/issues/329)).

- [`new_object()`](https://rconsortium.github.io/S7/reference/new_class.md)
  works better when custom property setters modify other properties.

- [`new_property()`](https://rconsortium.github.io/S7/reference/new_property.md)
  gains a `validator` argument that allows you to specify a per-property
  validator ([\#275](https://github.com/RConsortium/S7/issues/275)).

- [`new_property()`](https://rconsortium.github.io/S7/reference/new_property.md)
  clarifies that it’s the user’s responsibility to return the correct
  class; it is *not* automatically validated.

- Properties with a custom setter are now validated *after* the setter
  has run and are validated when the object is constructed or when you
  call
  [`validate()`](https://rconsortium.github.io/S7/reference/validate.md),
  not just when you modify them after construction.

- [`S7_inherits()`](https://rconsortium.github.io/S7/reference/S7_inherits.md)
  now accepts `class = NULL` to test if an object is any sort of S7
  object ([\#347](https://github.com/RConsortium/S7/issues/347)).

## S7 0.1.0

CRAN release: 2023-08-24

### May-July 2023

- [`new_external_generic()`](https://rconsortium.github.io/S7/reference/new_external_generic.md)
  is only needed when you want a soft dependency on another package.

- [`methods_register()`](https://rconsortium.github.io/S7/reference/methods_register.md)
  now also registers S3 and S4 methods
  ([\#306](https://github.com/RConsortium/S7/issues/306)).

### Jan-May 2023

- Subclasses of abstract class can have readonly properties
  ([\#269](https://github.com/RConsortium/S7/issues/269)).

- During construction, validation is now only performed once for each
  element of the class hierarchy
  ([\#248](https://github.com/RConsortium/S7/issues/248)).

- Implemented a better filtering strategy for the S4 class hierarchy so
  you can now correctly dispatch on virtual classes
  ([\#252](https://github.com/RConsortium/S7/issues/252)).

- New
  [`set_props()`](https://rconsortium.github.io/S7/reference/props.md)
  to make a modified copy of an object
  ([\#229](https://github.com/RConsortium/S7/issues/229)).

- `R CMD check` now passes on R 3.5 and greater (for tidyverse
  compatibility).

- Dispatching on an evaluated argument no longer causes a crash
  ([\#254](https://github.com/RConsortium/S7/issues/254)).

- Improve method dispatch failure message
  ([\#231](https://github.com/RConsortium/S7/issues/231)).

- Can use `|` to create unions from S7 classes
  ([\#224](https://github.com/RConsortium/S7/issues/224)).

- Can no longer subclass an environment via `class_environment` because
  we need to think the consequences of this behaviour through more fully
  ([\#253](https://github.com/RConsortium/S7/issues/253)).

### Rest of 2022

- Add `[.S7_object`, `[<-.S7_object`, `[[.S7_object`, and
  `[[<-.S7_object` methods to avoid “object of type ‘S4’ is not
  subsettable” error ([@jamieRowen](https://github.com/jamieRowen),
  [\#236](https://github.com/RConsortium/S7/issues/236)).

- Combining S7 classes with [`c()`](https://rdrr.io/r/base/c.html) now
  gives an error ([\#230](https://github.com/RConsortium/S7/issues/230))

- Base classes now show as `class_x` instead of `"x"` in method print
  ([\#232](https://github.com/RConsortium/S7/issues/232))

### Mar 2022

- Exported `class_factor`, `class_Date`, `class_POSIXct`, and
  `class_data.frame`.

- New
  [`S7_inherits()`](https://rconsortium.github.io/S7/reference/S7_inherits.md)
  and
  [`check_is_S7()`](https://rconsortium.github.io/S7/reference/S7_inherits.md)
  ([\#193](https://github.com/RConsortium/S7/issues/193))

- [`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md)
  can create abstract classes
  ([\#199](https://github.com/RConsortium/S7/issues/199)).

- `method_call()` is now
  [`S7_dispatch()`](https://rconsortium.github.io/S7/reference/new_generic.md)
  ([\#200](https://github.com/RConsortium/S7/issues/200)).

- Can now register methods for double-dispatch base Ops (currently only
  works if both classes are S7, or the first argument is S7 and the
  second doesn’t have a method for the Ops generic)
  ([\#128](https://github.com/RConsortium/S7/issues/128)).

- All built-in wrappers around base types use `class_`. You can no
  longer refer to a base type with a string or a constructor function
  ([\#170](https://github.com/RConsortium/S7/issues/170)).

- [`convert()`](https://rconsortium.github.io/S7/reference/convert.md)
  allows you to convert an object into another class
  ([\#136](https://github.com/RConsortium/S7/issues/136)).

- [`super()`](https://rconsortium.github.io/S7/reference/super.md)
  replaces `next_method()`
  ([\#110](https://github.com/RConsortium/S7/issues/110)).

### Feb 2022

- `class_any` and `class_missing` make it possible to dispatch on absent
  arguments and arguments of any class
  ([\#67](https://github.com/RConsortium/S7/issues/67)).

- New
  [`method_explain()`](https://rconsortium.github.io/S7/reference/method_explain.md)
  to explain dispatch
  ([\#194](https://github.com/RConsortium/S7/issues/194)).

- Minor property improvements: use same syntax for naming short-hand and
  full property specifications; input type automatically validated for
  custom setters. A property with a getter but no setter is read-only
  ([\#168](https://github.com/RConsortium/S7/issues/168)).

- When creating an object, unspecified properties are initialized with
  their default value
  ([\#67](https://github.com/RConsortium/S7/issues/67)). DISCUSS: to
  achieve this, the constructor arguments default to `class_missing`.

- Add `$.S7_object` and `$<-.S7_object` methods to avoid “object of type
  ‘S4’ is not subsettable” error
  ([\#204](https://github.com/RConsortium/S7/issues/204)).

- Dispatch now disambiguates between S4 and S3/S7, and, optionally,
  between S7 classes in different packages
  ([\#48](https://github.com/RConsortium/S7/issues/48),
  [\#163](https://github.com/RConsortium/S7/issues/163)).

- [`new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.md)
  now requires `dispatch_args`
  ([\#180](https://github.com/RConsortium/S7/issues/180)). This means
  that
  [`new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.md)
  will typically be called without names. Either
  `new_generic("foo", "x")` for a “standard” generic, or
  `new_generic("foo", "x", function(x, y) call_method())` for a
  non-standard method.

- [`new_external_generic()`](https://rconsortium.github.io/S7/reference/new_external_generic.md)
  now requires `dispatch_args` so we can eagerly check the signature.

- Revamp website. README now shows brief example and more info in
  [`vignette("S7")`](https://rconsortium.github.io/S7/articles/S7.md).
  Initial design docs and minutes are now articles so they appear on the
  website.

### Jan 2022

- New `props<-` for setting multiple properties simultaneously and
  validating afterwards
  ([\#149](https://github.com/RConsortium/S7/issues/149)).
- Validation now happens recursively, and validates types before
  validating the object
  ([\#149](https://github.com/RConsortium/S7/issues/149))
- Classes (base types, S3, S4, and S7) are handled consistently wherever
  they are used. Strings now only refer to base types. New explicit
  [`new_S3_class()`](https://rconsortium.github.io/S7/reference/new_S3_class.md)
  for referring to S3 classes
  ([\#134](https://github.com/RConsortium/S7/issues/134)). S4 unions are
  converted to S7 unions
  ([\#150](https://github.com/RConsortium/S7/issues/150)).
- Base numeric, atomic, and vector “types” are now represented as class
  unions ([\#147](https://github.com/RConsortium/S7/issues/147)).
- Different evaluation mechanism for method dispatch, and greater
  restrictions on dispatch args
  ([\#141](https://github.com/RConsortium/S7/issues/141))
- `x@.data` -\>
  [`S7_data()`](https://rconsortium.github.io/S7/reference/S7_data.md);
  probably to be replaced by casting.
- In generic, `signature` -\> `dispatch_args`.
- Polished [`str()`](https://rdrr.io/r/utils/str.html) and
  [`print()`](https://rdrr.io/r/base/print.html) methods
- [`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md)
  has properties as 3rd argument (instead of constructor).
