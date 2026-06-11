# Use an environment as the base type of an S7 class

![\\Experimental\\](figures/lifecycle-experimental.svg)

`class_environment` is the
[base](https://rconsortium.github.io/S7/reference/base_classes.md)
wrapper for environments. Unlike all other R objects, environments have
reference semantics, i.e., they are modified in place. It's not clear
what all the implications of this are for S7, so we are marking the use
of `class_environment` as experimental.

Its use is subject to the following caveats:

- [`S7_data()`](https://rconsortium.github.io/S7/reference/S7_data.md)
  and `S7_data<-()` error, because swapping the underlying data would
  destroy the existing attributes.

- The default
  [`convert()`](https://rconsortium.github.io/S7/reference/convert.md)
  method errors when upcasting to an environment because stripping the
  subclass's properties would mutate `from` in place.

## Usage

``` r
class_environment
```

## Examples

``` r
Counter := new_class(class_environment)
counter <- Counter()
counter$n <- 0L

# Reference semantics: `copy` and `counter` are the same object, so
# mutating one is visible through the other.
copy <- counter
copy$n <- 10L
counter$n
#> [1] 10
```
