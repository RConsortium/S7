# Validate an S7 object

`validate()` ensures that an S7 object is valid by calling the
`validator` provided in
[`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md).
This is done automatically when constructing new objects and when
modifying properties.

`valid_eventually()` disables validation, modifies the object, then
revalidates. This is useful when a sequence of operations would
otherwise lead an object to be temporarily invalid, or when repeated
property modification causes a performance bottleneck because the
validator is relatively expensive.

`valid_implicitly()` does the same but does not validate the object at
the end. It should only be used rarely, and in performance critical code
where you are certain a sequence of operations cannot produce an invalid
object.

## Usage

``` r
validate(object, recursive = TRUE, properties = TRUE)

valid_eventually(object, fun)

valid_implicitly(object, fun)
```

## Arguments

- object:

  An S7 object

- recursive:

  If `TRUE`, calls validator of parent classes recursively.

- properties:

  If `TRUE`, the default, checks property types before executing the
  validator.

- fun:

  A function to call on the object before validation.

## Value

Either `object` invisibly if valid, otherwise an error.

## Examples

``` r
# A range class might validate that the start is less than the end
Range <- new_class("Range",
  properties = list(start = class_double, end = class_double),
  validator = function(self) {
    if (self@start >= self@end) "start must be smaller than end"
  }
)
# You can't construct an invalid object:
try(Range(1, 1))
#> Error : <Range> object is invalid:
#> - start must be smaller than end

# And you can't create an invalid object with @<-
r <- Range(1, 2)
try(r@end <- 1)
#> Error : <Range> object is invalid:
#> - start must be smaller than end

# But what if you want to move a range to the right?
rightwards <- function(r, x) {
  r@start <- r@start + x
  r@end <- r@end + x
  r
}
# This function doesn't work because it creates a temporarily invalid state
try(rightwards(r, 10))
#> Error : <Range> object is invalid:
#> - start must be smaller than end

# This is the perfect use case for valid_eventually():
rightwards <- function(r, x) {
  valid_eventually(r, function(object) {
    object@start <- object@start + x
    object@end <- object@end + x
    object
  })
}
rightwards(r, 10)
#> <Range>
#>  @ start: num 11
#>  @ end  : num 12

# Alternatively, you can set multiple properties at once using props<-,
# which validates once at the end
rightwards <- function(r, x) {
  props(r) <- list(start = r@start + x, end = r@end + x)
  r
}
rightwards(r, 20)
#> <Range>
#>  @ start: num 21
#>  @ end  : num 22
```
