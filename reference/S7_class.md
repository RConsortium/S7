# Retrieve the S7 class of an object

Given an S7 object, find it's class.

## Usage

``` r
S7_class(object)
```

## Arguments

- object:

  The S7 object

## Value

An [S7 class](https://rconsortium.github.io/S7/reference/new_class.md).

## Examples

``` r
Foo <- new_class("Foo")
S7_class(Foo())
#> <Foo> class
#> @ parent     : <S7_object>
#> @ constructor: function() {...}
#> @ validator  : <NULL>
#> @ properties :
```
