# Define a new S7 class

A class specifies the properties (data) that each of its objects will
possess. The class, and its parent, determines which method will be used
when an object is passed to a generic.

Learn more in
[`vignette("classes-objects")`](https://rconsortium.github.io/S7/articles/classes-objects.md)

## Usage

``` r
new_class(
  name,
  parent = S7_object,
  package = topNamespaceName(parent.frame()),
  properties = list(),
  abstract = FALSE,
  constructor = NULL,
  validator = NULL
)

new_object(.parent, ...)
```

## Arguments

- name:

  The name of the class, as a string. The result of calling
  `new_class()` should always be assigned to a variable with this name,
  i.e. `Foo <- new_class("Foo")`.

- parent:

  The parent class to inherit behavior from. There are three options:

  - An S7 class, like
    [S7_object](https://rconsortium.github.io/S7/reference/S7_object.md).

  - An S3 class wrapped by
    [`new_S3_class()`](https://rconsortium.github.io/S7/reference/new_S3_class.md).

  - A base type, like
    [class_logical](https://rconsortium.github.io/S7/reference/base_classes.md),
    [class_integer](https://rconsortium.github.io/S7/reference/base_classes.md),
    etc.

- package:

  Package name. This is automatically resolved if the class is defined
  in a package, and `NULL` otherwise.

  Note, if the class is intended for external use, the constructor
  should be exported. Learn more in
  [`vignette("packages")`](https://rconsortium.github.io/S7/articles/packages.md).

- properties:

  A named list specifying the properties (data) that belong to each
  instance of the class. Each element of the list can either be a type
  specification (processed by
  [`as_class()`](https://rconsortium.github.io/S7/reference/as_class.md))
  or a full property specification created
  [`new_property()`](https://rconsortium.github.io/S7/reference/new_property.md).

- abstract:

  Is this an abstract class? An abstract class can not be instantiated.

- constructor:

  The constructor function. In most cases, you can rely on the default
  constructor, which will generate a function with one argument for each
  property.

  A custom constructor should call `new_object()` to create the S7
  object. The first argument, `.data`, should be an instance of the
  parent class (if used). The subsequent arguments are used to set the
  properties.

- validator:

  A function taking a single argument, `self`, the object to validate.

  The job of a validator is to determine whether the object is valid,
  i.e. if the current property values form an allowed combination. The
  types of the properties are always automatically validated so the job
  of the validator is to verify that the *values* of individual
  properties are ok (i.e. maybe a property should have length 1, or
  should always be positive), or that the *combination* of values of
  multiple properties is ok. It is called after construction and
  whenever any property is set.

  The validator should return `NULL` if the object is valid. If not, it
  should return a character vector where each element describes a single
  problem, using `@prop_name` to describe where the problem lies.

  See
  [`validate()`](https://rconsortium.github.io/S7/reference/validate.md)
  for more details, examples, and how to temporarily suppress validation
  when needed.

- .parent, ...:

  Parent object and named properties used to construct the object.

## Value

A object constructor, a function that can be used to create objects of
the given class.

## Examples

``` r
# Create an class that represents a range using a numeric start and end
Range <- new_class("Range",
  properties = list(
    start = class_numeric,
    end = class_numeric
  )
)
r <- Range(start = 10, end = 20)
r
#> <Range>
#>  @ start: num 10
#>  @ end  : num 20
# get and set properties with @
r@start
#> [1] 10
r@end <- 40
r@end
#> [1] 40

# S7 automatically ensures that properties are of the declared types:
try(Range(start = "hello", end = 20))
#> Error : <Range> object properties are invalid:
#> - @start must be <integer> or <double>, not <character>

# But we might also want to use a validator to ensure that start and end
# are length 1, and that start is < end
Range <- new_class("Range",
  properties = list(
    start = class_numeric,
    end = class_numeric
  ),
  validator = function(self) {
    if (length(self@start) != 1) {
      "@start must be a single number"
    } else if (length(self@end) != 1) {
      "@end must be a single number"
    } else if (self@end < self@start) {
      "@end must be great than or equal to @start"
    }
  }
)
try(Range(start = c(10, 15), end = 20))
#> Error : <Range> object is invalid:
#> - @start must be a single number
try(Range(start = 20, end = 10))
#> Error : <Range> object is invalid:
#> - @end must be great than or equal to @start

r <- Range(start = 10, end = 20)
try(r@start <- 25)
#> Error : <Range> object is invalid:
#> - @end must be great than or equal to @start
```
