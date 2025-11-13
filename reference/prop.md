# Get/set a property

- `prop(x, "name")` / `prop@name` get the value of the a property,
  erroring if it the property doesn't exist.

- `prop(x, "name") <- value` / `prop@name <- value` set the value of a
  property.

## Usage

``` r
prop(object, name)

prop(object, name, check = TRUE) <- value

object@name
```

## Arguments

- object:

  An object from a S7 class

- name:

  The name of the parameter as a character. Partial matching is not
  performed.

- check:

  If `TRUE`, check that `value` is of the correct type and run
  [`validate()`](https://rconsortium.github.io/S7/reference/validate.md)
  on the object before returning.

- value:

  A new value for the property. The object is automatically checked for
  validity after the replacement is done.

## Value

`prop()` and `@` return the value of the property. `prop<-()` and `@<-`
are called for their side-effects and return the modified object,
invisibly.

## Examples

``` r
Horse <- new_class("Horse", properties = list(
  name = class_character,
  colour = class_character,
  height = class_numeric
))
lexington <- Horse(colour = "bay", height = 15, name = "Lex")
lexington@colour
#> [1] "bay"
prop(lexington, "colour")
#> [1] "bay"

lexington@height <- 14
prop(lexington, "height") <- 15
```
