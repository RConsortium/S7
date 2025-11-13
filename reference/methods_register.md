# Register methods in a package

When using S7 in a package you should always call `methods_register()`
when your package is loaded. This ensures that methods are registered as
needed when you implement methods for generics (S3, S4, and S7) in other
packages. (This is not strictly necessary if you only register methods
for generics in your package, but it's better to include it and not need
it than forget to include it and hit weird errors.)

## Usage

``` r
methods_register()
```

## Value

Nothing; called for its side-effects.

## Examples

``` r
.onLoad <- function(...) {
  S7::methods_register()
}
```
