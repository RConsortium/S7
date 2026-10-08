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

Supply `new` to recommend another class in the warning. By default, this
changes the message only: `Dog()` still creates a `Dog`, even if the
warning recommends `Pet()`. Methods for `Dog` remain methods for `Dog`.
To register a method for both classes, use `Dog | Pet` as its signature.

Omit `new` to deprecate the class without recommending another.

## Usage

``` r
deprecated_class(
  name,
  ...,
  when,
  new = NULL,
  method = c("base", "lifecycle(warn)", "lifecycle(stop)"),
  alias = FALSE
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
  such as `parent`, `properties`, `constructor`, and `validator`. Cannot
  be supplied with `alias = TRUE`.

- when:

  The package version when the deprecation began, e.g. `"1.2.0"`.

- new:

  An S7 class to recommend in the warning, or `NULL` to give no
  recommendation. With `alias = TRUE`, this is also the class used for
  construction and class contexts such as method signatures.

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

- alias:

  If `TRUE`, make the deprecated name an alias for `new`. Requires `new`
  and no arguments in `...`. Defaults to `FALSE`, which preserves the
  deprecated class's definition and identity.

## Value

An S7 class with the additional class `S7_deprecated_class`.

## Aliasing a class

Set `alias = TRUE` to make the deprecated name an alias for `new`, as
[`deprecated_generic()`](https://rconsortium.github.io/S7/reference/deprecated_generic.md)
does for generics. Calling the old name warns, then calls the
replacement's constructor. Method signatures, parents, property types,
unions, and
[`S7_inherits()`](https://rconsortium.github.io/S7/reference/S7_inherits.md)
resolve the alias to `new` without warning. Both names refer to the same
class for these operations.

For example,
`Dog := deprecated_class(new = Pet, when = "2.0.0", alias = TRUE)` makes
`Dog()` construct a `Pet`. A method registered or removed through either
name affects the same method registration. The alias uses the
replacement's definition, so do not supply class arguments such as
`properties` or `constructor` in `...`.

Existing objects and subclasses retain their original class identity.
They do not become instances or subclasses of `new`. Previously
registered methods are not transferred to `new` either. Rebuild packages
that captured the old class in their subclasses, property types, or
method signatures. Use the default `alias = FALSE` when the old class
must remain usable by saved objects or installed subclasses during the
transition.

If `new` belongs to another package, the alias resolves its current
definition at run time. The replacement must remain exported under its
S7 class name.

## Installed subclasses

With `alias = FALSE`, adding deprecation to an otherwise unchanged class
does not require rebuilding packages that define subclasses. Existing
instances and subclasses continue matching methods for the deprecated
class.

The helper does not convert existing or saved objects to `new`, or
update installed subclasses if you also change the class definition.
Changes to the parent, properties, constructor, or validator need the
same compatibility considerations as changes to a non-deprecated class.

## Installed property defaults

With `alias = FALSE`, a downstream package installed before deprecation
can retain a property default that calls the constructor directly. For
example:

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
  new = Pet,
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

# An alias uses the replacement's constructor and class identity:
Hound := deprecated_class(new = Pet, when = "3.0.0", alias = TRUE)
Hound(name = "Rex") # warns and creates a Pet
#> Warning: `Hound()` was deprecated in version 3.0.0.
#> Please use `Pet()` instead.
#> <Pet>
#>  @ name: chr "Rex"
greet := new_generic("x")
method(greet, Hound) <- function(x) paste("Hello", x@name)
greet(Pet(name = "Rex"))
#> [1] "Hello Rex"
S7_inherits(Pet(name = "Rex"), Hound)
#> [1] TRUE

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
