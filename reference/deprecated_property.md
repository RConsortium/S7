# Deprecate a property

Add `deprecated_property()` to a class's `properties` to warn users when
they use an old property name. Supply `new` to forward reads and writes
to a replacement property, or omit it to keep storing the property's
value.

The default [`print()`](https://rdrr.io/r/base/print.html) and
[`str()`](https://rdrr.io/r/utils/str.html) methods omit deprecated
properties. Direct access and
[`props()`](https://rconsortium.github.io/S7/reference/props.md) still
read them and signal deprecation.

## Usage

``` r
deprecated_property(
  old,
  new = NULL,
  when,
  method = c("base", "lifecycle(warn)", "lifecycle(stop)"),
  class = class_any,
  default = NULL,
  validator = NULL
)
```

## Arguments

- old:

  The name of the deprecated property, as a string. Because the name is
  part of the property itself, the `properties` list entry doesn't need
  to be named.

- new:

  The name of the replacement property, as a string. If `NULL`, the
  property is deprecated without a replacement.

- when:

  The package version when the deprecation began, e.g. `"1.2.0"`.

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

- class, default:

  The property `class` and `default`, as in
  [`new_property()`](https://rconsortium.github.io/S7/reference/new_property.md).
  When `new` is supplied, `default` defaults to the value of the
  replacement property so that construction only signals deprecation
  when the deprecated argument differs from it.

- validator:

  A property validator, as in
  [`new_property()`](https://rconsortium.github.io/S7/reference/new_property.md).
  Only allowed when `new` is `NULL`. When renaming a property, put the
  validator on the replacement instead.

## Value

An [S7
property](https://rconsortium.github.io/S7/reference/new_property.md).

## When properties warn

Reading a deprecated property signals a warning. Setting it to a
different value also warns, but assignments of the current value can be
silent.

With a replacement, the old constructor argument defaults to the value
of the new argument. For example, `Basket(size = 2)` and
`Basket(size = 2, count = 2)` are both silent, while
`Basket(size = 2, count = 3)` warns. S7 cannot distinguish explicitly
supplying the same value from using that default.

Without a replacement, construction is silent: S7 initializes the
property for every object, whether its value came from an argument or a
default.

`method = "lifecycle(stop)"` turns the deprecation warnings described
above into errors. The cases described as silent remain silent.

## Preserving validation

When deprecating a stored property without a replacement, keep its
existing `class`, `default`, and `validator`. For a renamed property,
put validation on the replacement. To deprecate a computed property or
one with a custom setter, add a deprecation signal to its existing
getter and setter instead of using this helper.

## Downstream packages

Downstream packages can store copies of classes, generics, or properties
when they are built, instead of looking them up dynamically when they
are used. Those packages may not emit deprecation warnings until they
are rebuilt against the updated version of your package and reinstalled.
Code that looks them up dynamically can start warning as soon as your
package is updated.

## See also

[`deprecated_generic()`](https://rconsortium.github.io/S7/reference/deprecated_generic.md)
and
[`deprecated_class()`](https://rconsortium.github.io/S7/reference/deprecated_class.md)
to deprecate other parts of your API.

## Examples

``` r
# A property renamed from count to size:
Basket := new_class(properties = list(
  size = class_double,
  deprecated_property("count", new = "size", when = "1.5.0")
))

# Using the new name is silent, using the old name warns:
basket <- Basket(size = 3)
basket@count
#> Warning: `<Basket>@count` was deprecated in version 1.5.0.
#> Please use `<Basket>@size` instead.
#> [1] 3
```
