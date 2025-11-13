# Get/set underlying "base" data

When an S7 class inherits from an existing base type, it can be useful
to work with the underlying object, i.e. the S7 object stripped of class
and properties.

## Usage

``` r
S7_data(object)

S7_data(object, check = TRUE) <- value
```

## Arguments

- object:

  An object from a S7 class

- check:

  If `TRUE`, check that `value` is of the correct type and run
  [`validate()`](https://rconsortium.github.io/S7/reference/validate.md)
  on the object before returning.

- value:

  Object used to replace the underlying data.

## Value

`S7_data()` returns the data stored in the base object; `S7_data<-()` is
called for its side-effects and returns `object` invisibly.

## Examples

``` r
Text <- new_class("Text", parent = class_character)
y <- Text(c(foo = "bar"))
y
#> <Text> Named chr "bar"
#>  - attr(*, "names")= chr "foo"
S7_data(y)
#>   foo 
#> "bar" 

S7_data(y) <- c("a", "b")
y
#> <Text> Named chr [1:2] "a" "b"
#>  - attr(*, "names")= chr [1:2] "foo" NA
```
