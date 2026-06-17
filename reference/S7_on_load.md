# Package hooks for S7 methods

When using S7 in a package, add three hooks to your `zzz.R`:

- Call `S7_on_load()` from `.onLoad()`. This is S7's way of registering
  methods, rather than using `NAMESPACE` directives like S3 and S4 do.
  It ensures that methods for generics (S3, S4, and S7) defined in other
  packages are registered as needed when your package is loaded. This is
  only strictly necessary if you register methods for generics in other
  packages, but there's no harm in always including it and it ensures
  you won't forget later.

- Call `S7_on_unload()` from `.onUnload()`. This cleans up after
  `S7_on_load()`: it unregisters methods that your package registered
  for S7 generics in other packages, if they are still active, and
  removes any hooks that `S7_on_load()` added.

- Call `S7_on_build()` at the top level (i.e. *not* inside `.onLoad()`)
  after all method registration is complete. This avoids embedding
  copies of external generics in your package when you use `method<-`.

`S7_on_load()` was previously known as `methods_register()`. This
function is retained for backward compatibility but new code should use
`S7_on_load()`.

See
[`vignette("packages")`](https://rconsortium.github.io/S7/articles/packages.md)
for more details.

## Usage

``` r
S7_on_load()

methods_register()

S7_on_unload()

S7_on_build()
```

## Value

Nothing; these functions are called for their side-effects.

## Examples

``` r
# In zzz.R:
.onLoad <- function(...) {
  S7::S7_on_load()
}
.onUnload <- function(...) {
  S7::S7_on_unload()
}
S7::S7_on_build()
```
