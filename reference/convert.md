# Convert an object from one type to another

`convert(from, to)` is a built-in generic for converting an object from
one type to another. It is special in three ways:

- It uses double-dispatch, because conversion depends on both `from` and
  `to`.

- It uses non-standard dispatch because `to` is a class, not an object.

- It doesn't use inheritance for the `to` argument. To understand why,
  imagine you have written methods to objects of various types to
  `classParent`. If you then create a new `classChild` that inherits
  from `classParent`, you can't expect the methods written for
  `classParent` to work because those methods will return `classParent`
  objects, not `classChild` objects.

`convert()` provides three default implementations:

1.  When `from` inherits from `to`, it strips any properties that `from`
    possesses that `to` does not (upcasting).

2.  When `to` inherits from `from`, it creates a new object of class
    `to`, copying over existing properties from `from` and initializing
    new properties of `to` (downcasting).

3.  When `to` is a base type (e.g.
    [class_integer](https://rconsortium.github.io/S7/reference/base_classes.md)
    or
    [class_character](https://rconsortium.github.io/S7/reference/base_classes.md))
    and neither of the above apply, it calls the corresponding `as.*()`
    function (e.g. [`as.integer()`](https://rdrr.io/r/base/integer.html)
    or [`as.character()`](https://rdrr.io/r/base/character.html)). This
    mirrors the convention that `as.*()` coercion sits below
    `convert()`, so you can rely on it as a fallback but still override
    it with a more specific method.

If you are converting an object solely for the purposes of accessing a
method on a superclass, you probably want
[`super()`](https://rconsortium.github.io/S7/reference/super.md)
instead. See its docs for more details.

### S3 & S4

`convert()` plays a similar role to the convention of defining
`as.foo()` functions/generics in S3, and to `as()`/`setAs()` in S4.

## Usage

``` r
convert(from, to, ...)
```

## Arguments

- from:

  An S7 object to convert.

- to:

  An S7 class specification, passed to
  [`as_class()`](https://rconsortium.github.io/S7/reference/as_class.md).

- ...:

  Other arguments passed to custom `convert()` methods. For downcasting,
  these can be used to override existing properties or set new ones.

## Value

Either `from` coerced to class `to`, or an error if the coercion is not
possible.

## Examples

``` r
Foo1 <- new_class("Foo1", properties = list(x = class_integer))
Foo2 <- new_class("Foo2", Foo1, properties = list(y = class_double))

# Upcasting: S7 provides a default implementation for coercing an object
# to one of its parent classes:
convert(Foo2(x = 1L, y = 2), to = Foo1)
#> <Foo1>
#>  @ x: int 1

# Downcasting: S7 also provides a default implementation for coercing an
# object to one of its child classes:
convert(Foo1(x = 1L), to = Foo2)
#> <Foo2>
#>  @ x: int 1
#>  @ y: num(0) 
convert(Foo1(x = 1L), to = Foo2, y = 2.5)  # Set new property
#> <Foo2>
#>  @ x: int 1
#>  @ y: num 2.5
convert(Foo1(x = 1L), to = Foo2, x = 2L, y = 2.5)  # Override existing and set new
#> <Foo2>
#>  @ x: int 2
#>  @ y: num 2.5

# Converting to a base type falls back to the corresponding `as.*()`:
convert(1.5, to = class_character)
#> [1] "1.5"
convert(c("1", "2"), to = class_integer)
#> [1] 1 2

# For all other cases, you'll need to provide your own.
try(convert(Foo1(x = 1L), to = class_integer))
#> Error in as.integer(from, ...) : 
#>   cannot coerce type 'object' to vector of type 'integer'

method(convert, list(Foo1, class_integer)) <- function(from, to) {
  from@x
}
convert(Foo1(x = 1L), to = class_integer)
#> [1] 1

# Note that conversion does not respect inheritance so if we define a
# convert method for integer to foo1
method(convert, list(class_integer, Foo1)) <- function(from, to) {
  Foo1(x = from)
}
convert(1L, to = Foo1)
#> <Foo1>
#>  @ x: int 1

# Converting to Foo2 will still error
try(convert(1L, to = Foo2))
#> Error in convert(1L, to = Foo2) : 
#>   Can't find method with dispatch classes:
#> - from: <integer>
#> - to  : <Foo2>
# This is probably not surprising because foo2 also needs some value
# for `@y`, but it definitely makes dispatch for convert() special
```
