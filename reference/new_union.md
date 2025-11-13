# Define a class union

A class union represents a list of possible classes. You can create it
with `new_union(a, b, c)` or `a | b | c`. Unions can be used in two
places:

- To allow a property to be one of a set of classes,
  `new_property(class_integer | Range)`. The default `default` value for
  the property will be the constructor of the first object in the union.
  This means if you want to create an "optional" property (i.e. one that
  can be `NULL` or of a specified type), you'll need to write (e.g.)
  `NULL | class_integer`.

- As a convenient short-hand to define methods for multiple classes.
  `method(foo, X | Y) <- f` is short-hand for
  `method(foo, X) <- f; method(foo, Y) <- foo`

S7 includes built-in unions for "numeric" (integer and double vectors),
"atomic" (logical, numeric, complex, character, and raw vectors) and
"vector" (atomic vectors, lists, and expressions).

## Usage

``` r
new_union(...)
```

## Arguments

- ...:

  The classes to include in the union. See
  [`as_class()`](https://rconsortium.github.io/S7/reference/as_class.md)
  for details.

## Value

An S7 union, i.e. a list with class `S7_union`.

## Examples

``` r
logical_or_character <- new_union(class_logical, class_character)
logical_or_character
#> <S7_union>: <logical> or <character>
# or with shortcut syntax
logical_or_character <- class_logical | class_character

Foo <- new_class("Foo", properties = list(x = logical_or_character))
Foo(x = TRUE)
#> <Foo>
#>  @ x: logi TRUE
Foo(x = letters[1:5])
#> <Foo>
#>  @ x: chr [1:5] "a" "b" "c" "d" "e"
try(Foo(1:3))
#> Error : <Foo> object properties are invalid:
#> - @x must be <logical> or <character>, not <integer>

bar <- new_generic("bar", "x")
# Use built-in union
method(bar, class_atomic) <- function(x) "Hi!"
bar
#> <S7_generic> bar(x, ...) with 6 methods:
#> 1: method(bar, class_integer)
#> 2: method(bar, class_complex)
#> 3: method(bar, class_double)
#> 4: method(bar, class_character)
#> 5: method(bar, class_logical)
#> 6: method(bar, class_raw)
bar(TRUE)
#> [1] "Hi!"
bar(letters)
#> [1] "Hi!"
try(bar(NULL))
#> Error : Can't find method for `bar(<NULL>)`.
```
