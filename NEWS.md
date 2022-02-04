# R7 0.0.0.9000

## Jan 2020

* Classes (base types, S3, S4, and R7) are handled consistently wherever they
  are used. Strings now only refer to base types. New explicit `s3_class()` for 
  referring to S3 classes (#134).
* Different evaluation mechanism for method dispatch, and greater restrictions 
  on dispatch args (#141)
* `x@.data` -> `r7_data()`; probably to be replaced by casting.
* In generic, `signature` -> `dispatch_args`.
* `new_class()` has properties as 3rd argument (instead of constructor).
