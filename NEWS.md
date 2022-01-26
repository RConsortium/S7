# R7 0.0.0.9000

## Jan 2020

* Different evaluation mechanism for method dispatch, and greater restrictions 
  on dispatch args (#141)
* `x@.data` -> `r7_data()`; probably to be replaced by casting.
* In generic, `signature` -> `dispatch_args`.
* `new_class()` has properties as 3rd argument (instead of constructor).
