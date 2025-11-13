# Get/set multiple properties

- `props(x)` returns all properties.

- `props(x) <- list(name1 = val1, name2 = val2)` modifies an existing
  object by setting multiple properties simultaneously.

- `set_props(x, name1 = val1, name2 = val2)` creates a copy of an
  existing object with new values for the specified properties.

## Usage

``` r
props(object, names = prop_names(object))

props(object, check = TRUE) <- value

set_props(object, ..., .check = TRUE)
```

## Arguments

- object:

  An object from a S7 class

- names:

  A character vector of property names to retrieve. Default is all
  properties.

- check, .check:

  If `TRUE`, run
  [`validate()`](https://rconsortium.github.io/S7/reference/validate.md)
  on the object before returning.

- value:

  A named list of values. The object is checked for validity only after
  all replacements are performed.

- ...:

  Name-value pairs given property to modify and new value.

## Value

A named list of property values.

## Examples

``` r
Horse <- new_class("Horse", properties = list(
  name = class_character,
  colour = class_character,
  height = class_numeric
))
lexington <- Horse(colour = "bay", height = 15, name = "Lex")

props(lexington)
#> $name
#> [1] "Lex"
#> 
#> $colour
#> [1] "bay"
#> 
#> $height
#> [1] 15
#> 
props(lexington) <- list(height = 14, name = "Lexington")
lexington
#> <Horse>
#>  @ name  : chr "Lexington"
#>  @ colour: chr "bay"
#>  @ height: num 14
```
