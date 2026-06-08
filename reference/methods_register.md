# Package hooks for S7 methods

When using S7 in a package, add two hooks to your `zzz.R`:

- Call `methods_register()` from `.onLoad()`. This is S7's way of
  registering methods, rather than using `NAMESPACE` directives like S3
  and S4 do. It ensures that methods for generics (S3, S4, and S7)
  defined in other packages are registered as needed when your package
  is loaded. This is only strictly necessary if you register methods for
  generics in other packages, but there's no harm in always including it
  and it ensures you won't forget later.

- Call `S7_on_build()` at the top level (i.e. *not* inside `.onLoad()`)
  after all method registration is complete. This avoids embedding
  copies of external generics in your package when you use `method<-`.

See
[`vignette("packages")`](https://rconsortium.github.io/S7/articles/packages.md)
for more details.

## Usage

``` r
methods_register()

S7_on_build()
```

## Value

Nothing; both functions are called for their side-effects.

## Examples

``` r
# In zzz.R:
.onLoad <- function(...) {
  S7::methods_register()
}
S7::S7_on_build()
```
