# R7 0.0.0.9000

* `method_call()` is now `R7_dispatch()`

## Mar 2022

* `convert()` allows you to convert an object into another class (#136).

* `super()` replaces `next_method()` (#110).

## Feb 2022

* `any_class` and `missing_any` make it possible to dispatch on absent
  arguments and arguments of any class (#67).

* New `method_explain()` to explain dispatch (#194).

* Minor property improvements: use same syntax for naming short-hand and
  full property specifications; input type automatically validated for 
  custom setters. A property with a getter but no setter is read-only (#168).

* When creating an object, unspecified properties are initialized with their 
  default value (#67). DISCUSS: to achieve this, the constructor arguments
  default to `missing_class`.

* Add `$.R7_object` and `$<-.R7_object` methods to avoid "object of type 'S4'
  is not subsettable" error (#204).
  
* Dispatch now disambiguates between S4 and S3/R7, and, optionally, between
  R7 classes in different packages (#48, #163).

* `new_generic()` now requires `dispatch_args` (#180). This means that 
  `new_generic()` will typically be called without names. Either 
  `new_generic("foo", "x")` for a "standard" generic, or 
  `new_generic("foo", "x", function(x, y) call_method())` for 
  a non-standard method.

* `new_external_generic()` now requires `dispatch_args` so we can eagerly 
  check the signature.

* Revamp website. README now shows brief example and more info in 
  `vignette("R7")`. Initial design docs and minutes are now articles so
  they appear on the website.

## Jan 2022

* New `props<-` for setting multiple properties simultaneously and validating
  afterwards (#149).
* Validation now happens recursively, and validates types before validating 
  the object (#149)
* Classes (base types, S3, S4, and R7) are handled consistently wherever they
  are used. Strings now only refer to base types. New explicit `new_S3_class()` for 
  referring to S3 classes (#134). S4 unions are converted to R7 unions (#150).
* Base numeric, atomic, and vector "types" are now represented as class unions
  (#147).
* Different evaluation mechanism for method dispatch, and greater restrictions 
  on dispatch args (#141)
* `x@.data` -> `R7_data()`; probably to be replaced by casting.
* In generic, `signature` -> `dispatch_args`.
* Polished `str()` and `print()` methods
* `new_class()` has properties as 3rd argument (instead of constructor).
