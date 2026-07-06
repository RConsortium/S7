# Non-strict conversion

`convert_lazy()` is a non-strict variant of
[`convert()`](https://rconsortium.github.io/S7/reference/convert.md)
that guarantees that the result inherits from `to`, without forcing
`from` to become an exact instance of `to`, i.e. it never upcasts.

## Usage

``` r
convert_lazy(from, to, ...)
```

## Arguments

- from:

  An S7 object to convert.

- to:

  An S7 class specification, passed to
  [`as_class()`](https://rconsortium.github.io/S7/reference/as_class.md).

- ...:

  Other arguments passed to custom
  [`convert()`](https://rconsortium.github.io/S7/reference/convert.md)
  methods. For downcasting, these can be used to override existing
  properties or set new ones. As a convenience, you can supply a single
  unnamed list instead of individual name-value pairs, which makes it
  easy to override properties programmatically.

## Value

`from`, unchanged, if it already inherits from `to`; otherwise the
result of `convert(from, to, ...)`.

## See also

[`convert()`](https://rconsortium.github.io/S7/reference/convert.md) for
the strict variant that always returns an exact instance of `to`.

## Examples

``` r
Foo1 := new_class(properties = list(x = class_integer))
Foo2 := new_class(Foo1, properties = list(y = class_double))

# `convert()` upcasts by stripping the extra properties of `from`:
convert(Foo2(x = 1L, y = 2), to = Foo1)
#> <Foo1>
#>  @ x: int 1

# `convert_lazy()` never upcasts: because the object already inherits from
# Foo1, it's returned unchanged, keeping `y`:
convert_lazy(Foo2(x = 1L, y = 2), to = Foo1)
#> <Foo2>
#>  @ x: int 1
#>  @ y: num 2

# When `from` doesn't inherit from `to`, `convert_lazy()` falls back to
# `convert()`, so it can still downcast or coerce to a base type:
convert_lazy(Foo1(x = 1L), to = Foo2, y = 2.5)
#> <Foo2>
#>  @ x: int(0) 
#>  @ y: num -1
convert_lazy(1.5, to = class_character)
#> [1] "1.5"
```
