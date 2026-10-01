# Deprecate a class

To deprecate a class, change
[`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md)
to `deprecated_class()` in its definition and supply `when`. Keep its
existing parent, properties, constructor, and validator, and keep
exporting it under the same name.

Calling the constructor warns, then constructs an instance of the
deprecated class. Using it in method signatures, `parent`, property
classes, or
[`new_external_class()`](https://rconsortium.github.io/S7/reference/new_external_class.md)
references does not warn. Property defaults generated before deprecation
can still signal; see "Installed property defaults" below.

Supply `replacement` to recommend another class in the warning. This
changes the message only: `Dog()` still creates a `Dog`, even if the
warning recommends `Pet()`. Methods for `Dog` remain methods for `Dog`.
To register a method for both classes, use `Dog | Pet` as its signature.

Omit `replacement` to deprecate the class without recommending another.

## Usage

``` r
deprecated_class(
  name,
  ...,
  when,
  replacement = NULL,
  method = c("base", "lifecycle(warn)", "lifecycle(stop)")
)
```

## Arguments

- name:

  The name of the class, as a string. Assign the result to a variable
  with this name, most easily with
  [:=](https://rconsortium.github.io/S7/reference/named-bind.md).

- ...:

  Named arguments passed to
  [`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md),
  such as `parent`, `properties`, `constructor`, and `validator`.

- when:

  The package version when the deprecation began, e.g. `"1.2.0"`.

- replacement:

  An S7 class to recommend in the warning, or `NULL` to give no
  recommendation. It does not affect construction or dispatch.

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

## Value

An S7 class with the additional class `S7_deprecated_class`.

## Installed subclasses

Adding deprecation to an otherwise unchanged class does not require
rebuilding packages that define subclasses. Existing instances and
subclasses continue matching methods for the deprecated class.

The helper does not convert existing or saved objects to `replacement`,
or update installed subclasses if you also change the class definition.
Changes to the parent, properties, constructor, or validator need the
same compatibility considerations as changes to a non-deprecated class.

## Installed property defaults

A downstream package installed before deprecation can retain a property
default that calls the constructor directly. For example:

    Box := new_class(properties = list(item = upstream::Foo))

After `upstream` deprecates `Foo`, `Box()` can warn, or error with
`method = "lifecycle(stop)"`. Rebuild and reinstall the downstream
package against the updated `upstream` to generate a silent default.
Defaults generated from
[`new_external_class()`](https://rconsortium.github.io/S7/reference/new_external_class.md)
references stay silent without rebuilding.

Explicitly written constructor calls, including calls in a property's
`default` or a custom constructor, still signal after rebuilding.

## See also

[`deprecated_generic()`](https://rconsortium.github.io/S7/reference/deprecated_generic.md)
and
[`deprecated_property()`](https://rconsortium.github.io/S7/reference/deprecated_property.md)
to deprecate other parts of your API.

## Examples

``` r
# Recommend Pet() while keeping Dog() working:
Pet := new_class(properties = list(name = class_character))
Dog := deprecated_class(
  properties = list(name = class_character),
  replacement = Pet,
  when = "2.0.0"
)

Dog(name = "Fido") # warns and creates a Dog
#> Warning: `Dog()` was deprecated in version 2.0.0.
#> Please use `Pet()` instead.
#> <Dog>
#>  @ name: chr "Fido"
Pet(name = "Fido") # creates a Pet without warning
#> <Pet>
#>  @ name: chr "Fido"

# Existing methods and subclasses still use Dog:
speak := new_generic("x")
method(speak, Dog) <- function(x) "Woof!"
Puppy := new_class(parent = Dog)
speak(Puppy(name = "Rex"))
#> [1] "Woof!"

# A method can support both classes:
method(speak, Dog | Pet) <- function(x) x@name
#> Overwriting method speak(<Dog>)
speak(Pet(name = "Rex"))
#> [1] "Rex"

# A class deprecated without a replacement:
Cat := deprecated_class(
  properties = list(lives = class_double),
  when = "3.0.0"
)
Cat(lives = 9)
#> Warning: `Cat()` was deprecated in version 3.0.0.
#> <Cat>
#>  @ lives: num 9
```
