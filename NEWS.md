# R7 0.0.0.9000

## Feb 2022

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
  are used. Strings now only refer to base types. New explicit `S3_class()` for 
  referring to S3 classes (#134). S4 unions are converted to R7 unions (#150).
* Base numeric, atomic, and vector "types" are now represented as class unions
  (#147).
* Different evaluation mechanism for method dispatch, and greater restrictions 
  on dispatch args (#141)
* `x@.data` -> `R7_data()`; probably to be replaced by casting.
* In generic, `signature` -> `dispatch_args`.
* Polished `str()` and `print()` methods
* `new_class()` has properties as 3rd argument (instead of constructor).
