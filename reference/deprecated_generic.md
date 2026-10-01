# Deprecate a generic

To deprecate a generic, change
[`new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.md)
to `deprecated_generic()` in its definition and supply `when`. Keep its
existing `dispatch_args`, `fun`, and method registrations, and keep
exporting it under the same name. Calls still dispatch to its methods,
but now warn.

If you rename a generic or move it to another package, supply the
replacement as `new` instead of `dispatch_args` and `fun`:

- Calling the old name warns, then calls the replacement.

- Registering a method on the old name registers it on the replacement,
  without a warning. Both names use the same methods.

## Usage

``` r
deprecated_generic(
  name,
  dispatch_args,
  fun = NULL,
  when,
  new = NULL,
  method = c("base", "lifecycle(warn)", "lifecycle(stop)"),
  new_label = NULL
)
```

## Arguments

- name:

  The old name of the generic, as a string. As with
  [`new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.md),
  the result should be assigned to a variable with this name, most
  easily with
  [:=](https://rconsortium.github.io/S7/reference/named-bind.md).

- dispatch_args, fun:

  As in
  [`new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.md).
  Use these to define a deprecated generic without a replacement. Cannot
  be supplied with `new`.

- when:

  The package version when the deprecation began, e.g. `"1.2.0"`.

- new:

  The replacement: an S7 generic, usually the renamed generic, or a
  generic that now lives in another package.

- method:

  How to signal the deprecation:

  - `"base"` (the default): a
    [`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html)-style
    warning.

  - `"lifecycle(warn)"`:
    [`lifecycle::deprecate_warn()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html),
    a warning that's only displayed once every eight hours.

  - `"lifecycle(stop)"`:
    [`lifecycle::deprecate_stop()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html),
    an error.

  The lifecycle options require the lifecycle package to be installed,
  and to be a dependency of your package.

- new_label:

  How to name the replacement in the warning, e.g. `"Bar()"` or
  `"pkg::Bar()"`. Requires `new`. Defaults to the name recorded when the
  replacement was created. Use this when you export it under a different
  name. It changes the message, not the object used as the replacement.

## Value

A function with class `S7_deprecated_generic`.

## Moving a generic to another package

If `pkg::gen()` moves to `pkgcore::gen()`, define the old name in `pkg`
as:

    gen := deprecated_generic(new = pkgcore::gen, when = "2.0.0")

Methods registered through either name apply to calls through both
names, including methods supplied by other packages.

## Changing arguments

When `new` is supplied, the deprecated generic has the same arguments as
the replacement and forwards them unchanged. Changing or deprecating
arguments needs code in the generic itself; this helper only deprecates
the generic's name.

## See also

[`deprecated_class()`](https://rconsortium.github.io/S7/reference/deprecated_class.md)
and
[`deprecated_property()`](https://rconsortium.github.io/S7/reference/deprecated_property.md)
to deprecate other parts of your API.

## Examples

``` r
# Deprecate a generic by changing its definition:
shout := deprecated_generic("x", when = "2.0.0")
method(shout, class_character) <- \(x) toupper(x)
shout("hi")
#> Warning: `shout()` was deprecated in version 2.0.0.
#> [1] "HI"

# A generic renamed from summarise() to summarize():
summarize := new_generic("x")
method(summarize, class_double) <- function(x) mean(x)
summarise := deprecated_generic(new = summarize, when = "1.1.0")
# Calling the old name warns, then delegates:
summarise(c(1, 2, 3))
#> Warning: `summarise()` was deprecated in version 1.1.0.
#> Please use `summarize()` instead.
#> [1] 2

# Registering a method on the old name registers it on the new generic:
method(summarise, class_character) <- function(x) unique(x)
summarize(c("a", "b", "a"))
#> [1] "a" "b"
```
