# Base S7 class

The base class from which all S7 classes eventually inherit from.

## Usage

``` r
S7_object()
```

## Value

The base S7 object.

## Representation version

Newly constructed S7 objects carry an internal `_S7_version` attribute.
Classes returned by
[`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md)
carry `S7_version`. Both start at `1L` and track changes to the stored
representation independently of the S7 package version, so future
backward compatibility code can distinguish object formats. Objects
created before versioning was introduced have no version attribute and
remain supported.

## Examples

``` r

S7_object
#> <S7_object> class
#> @ parent     : <NULL>
#> @ constructor: function() {...}
#> @ validator  : function(self) {...}
#> @ properties :
```
