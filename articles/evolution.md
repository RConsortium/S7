# Evolving S7 classes and generics

Sooner or later every successful package needs to change its interface:
a generic gains an argument, a property turns out to be misnamed, or a
class needs to be split in two. When other packages depend on yours,
these changes are no longer private decisions, and you need to think
through how your changes affect dependencies. This vignette works
through the ways an S7 interface can change and the consequences for
downstream packages.

Throughout this vignette, we’ll imagine two packages:

- **yourPackage** is *upstream*: it defines S7 classes and generics.
  You’re the author of yourPackage.
- **depPackage** is *downstream*: it depends on yourPackage, using its
  generics (by calling them or registering methods) and its classes (by
  subclassing them or creating instances). Someone else is the author of
  depPackage.

This vignette addresses the case where both packages are on CRAN.
Releasing two CRAN packages at the same time is painful and best
avoided, so every breaking change needs a path where depPackage can
release a version that works with both old and new yourPackage. In
practice, you may have many dependencies; depPackage stands in for any
one of them.

This vignette shows you how to use the tools that S7 provides, and
describes the changes that can’t be made without breaking depPackage.

The vignette is divided into changes to generics and changes to classes.
Each starts by explaining when things break, then goes into the details
of each possible change and how to make it with the least disruption to
depPackage. We finish with some practices that make your package easier
to evolve, and a table summarising every change.

``` r

library(S7)
```

## Changing a generic

Most changes to a generic cause problems with method registration.
depPackage’s `method<-` calls run when depPackage is installed, and the
registrations are replayed each time it’s loaded. Failures at either
install- or load-time are problematic, because they make it impossible
to create a version of depPackage that works with both versions of
yourPackage.

For this reason, S7 only checks method compatibility in development:

- when you’ve loaded the package with `devtools::load_all()`,
- when you’re running `R CMD check` on the package that owns the method,
  the generic, or one of the classes, or
- when you’re working outside a package (e.g. interactively, in a
  script, or in a vignette).

In development, S7 checks that:

- The method has the same dispatch arguments as the generic.
- If the generic lacks `...`, the method’s formals match the generic’s
  exactly.
- The method has all the non-dispatch arguments that the generic has.
- The method’s default values match the generic’s.

The first two are errors (or warnings under `load_all()`, so you can see
them all at once), which fail `R CMD check` of depPackage. The last two
are warnings, which show up as a single NOTE.

Users skip these checks entirely: an incompatible method still installs
and loads, and any problem only shows up when the method is called. This
gives the depPackage developer the ability to create a version that
works with both old and new yourPackage. So in the sections below, a
change that “breaks methods” means that depPackage fails `R CMD check`,
and its methods error or misbehave when users call them.

The following sections cover each way a generic can change:

- [Changing dispatch](#changing-dispatch)
- [Adding an argument](#adding-an-argument)
- [Removing an argument](#removing-an-argument)
- [Changing a default](#changing-a-default)
- [Renaming a generic](#renaming-a-generic)
- [Removing a generic](#removing-a-generic)
- [Moving a generic to another
  package](#moving-a-generic-to-another-package)

### Changing dispatch

Changing a generic’s dispatch arguments (i.e. their number, names, or
order) breaks every method in depPackage: no method can match both the
old and new dispatch arguments. There is no workaround: a generic’s
dispatch arguments are its identity. So if you change dispatch, you are
creating a new generic, and that means you should be explicit about it:
create a new generic with a new name and deprecate the old one. We
recommend avoiding this if at all possible, but if you have to, here’s
the process:

1.  You create a new generic, with a new name and the new dispatch
    arguments.
2.  You deprecate the old generic with
    [`deprecated_generic()`](https://rconsortium.github.io/S7/reference/deprecated_generic.md),
    and release.
3.  Each dependency registers its methods on the new generic, and
    releases whenever convenient.
4.  You remove the old generic in a later release.

### Adding an argument

If your generic has `...`, adding a non-dispatch argument is one of the
mildest changes you can make. Existing methods keep working, but every
registration produces a warning until they’re updated:

``` r

# yourPackage, version 1
gen := new_generic("x")

# depPackage's method, written against version 1
BClass := new_class()
method(gen, BClass) <- function(x, ...) "old"

# yourPackage, version 2 adds `verbose`
gen := new_generic("x", fun = function(x, verbose = FALSE, ...) S7_dispatch())
method(gen, BClass) <- function(x, ...) "old"
#> Warning: gen(<BClass>) doesn't have argument `verbose`
```

depPackage’s users see nothing, but the warning surfaces under
`load_all()` and as a single NOTE in `R CMD check` of depPackage. The
fix is pleasingly asymmetric: because methods may have arguments the
generic lacks, depPackage can add the argument **before** yourPackage
releases:

``` r

# yourPackage, version 1 again
gen := new_generic("x")

# depPackage adds `verbose` ahead of yourPackage's release: no warning under version 1...
method(gen, BClass) <- function(x, verbose = FALSE, ...) "new"

# ...and none under version 2 either
gen := new_generic("x", fun = function(x, verbose = FALSE, ...) S7_dispatch())
method(gen, BClass) <- function(x, verbose = FALSE, ...) "new"
```

So the process is:

1.  You tell dependency authors about the upcoming argument (or send
    PRs).
2.  Each dependency adds the argument to its methods, and can release
    whenever convenient.
3.  You release.

If your generic doesn’t have `...`, adding an argument is an error for
every downstream method, and there is no version of the method that
satisfies both the old and the new generic. Either way, there is no fix
on depPackage’s side, so a generic without `...` is a frozen interface:
only add one deliberately, and treat any change to it like a rename.

### Removing an argument

Removing a non-dispatch argument is similarly mild. Methods may have
extra arguments, so a method that still lists the removed argument
registers without complaint against both versions. depPackage can remove
the argument from its methods at leisure.

Callers are the bigger concern: `gen(x, removed_arg = 1)` will still be
accepted by the generic (via `...`) and will reach methods that may or
may not expect it. So before removing an argument, deprecate it: keep it
in the generic, and warn when it’s supplied:

``` r

gen := new_generic("x", fun = function(x, verbose = FALSE, ...) {
  if (!missing(verbose)) {
    .Deprecated(msg = "The `verbose` argument is deprecated and will be removed in a future release.")
  }
  S7_dispatch()
})
method(gen, BClass) <- function(x, verbose = FALSE, ...) "result"
. <- gen(BClass(), verbose = TRUE)
#> Warning in gen(BClass(), verbose = TRUE): The `verbose` argument is deprecated
#> and will be removed in a future release.
```

If you use lifecycle, the equivalent is:

``` r

gen := new_generic("x", fun = function(x, verbose = deprecated(), ...) {
  if (lifecycle::is_present(verbose)) {
    lifecycle::deprecate_warn("1.0.0", "gen(verbose)")
  }
  S7_dispatch()
})
method(gen, BClass) <- function(x, verbose = deprecated(), ...) "result"
. <- gen(BClass(), verbose = TRUE)
#> Warning: The `verbose` argument of `gen()` is deprecated as of <NA> 1.0.0.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

So the process is:

1.  You deprecate the argument in the generic, and release.
2.  Each dependency stops supplying the argument (and can remove it from
    its methods), and releases whenever convenient.
3.  You remove the argument in a later release.

### Changing a default

Changing a default changes behavior for every caller, because S7
dispatch never uses the defaults provided by the method. For this reason
S7 warns whenever the generic and method drift out of sync:

``` r

gen := new_generic("x", fun = function(x, drop = TRUE, ...) S7_dispatch())
method(gen, BClass) <- function(x, drop = FALSE, ...) drop
#> Warning: In gen(<BClass>), default value of `drop` is not the same as the generic
#> - Generic: TRUE
#> - Method:  FALSE
gen(BClass())
#> [1] TRUE
```

There’s no way around this warning, as there’s no spelling of the method
that agrees with both versions of yourPackage. Fortunately, as with
adding an argument, this only costs depPackage one check NOTE. Our
advice is to keep non-dispatch arguments to a minimum, and change
defaults rarely, if ever.

If you must, the process is:

1.  You tell dependency authors about the upcoming change.
2.  You release.
3.  Each dependency updates the default in its methods, and releases
    whenever convenient.

### Renaming a generic

If you simply rename a generic, depPackage will fail to install if it
uses `importFrom(yourPackage, gen)`. The fix is to keep the old name as
a deprecated alias for the new generic:

``` r

# yourPackage, version 1
gen1 := new_generic("x")

# yourPackage, version 2
gen2 := new_generic("x")
gen1 := deprecated_generic(new = gen2, when = "2.0.0")
```

[`deprecated_generic()`](https://rconsortium.github.io/S7/reference/deprecated_generic.md)
returns an object that still counts as the generic, so method
registration continues to work, but calls through the old name warn that
it’s time to move on.

``` r

method(gen1, BClass) <- function(x, ...) "result"
gen2(BClass())
#> [1] "result"
gen1(BClass())
#> Warning in gen1(BClass()): `gen1()` was deprecated in version 2.0.0.
#> Please use `gen2()` instead.
#> [1] "result"
```

By default,
[`deprecated_generic()`](https://rconsortium.github.io/S7/reference/deprecated_generic.md)
generates a base warning, but you can choose to use lifecycle instead:

``` r

gen1 := deprecated_generic(
  new = gen2, 
  when = "2.0.0", 
  method = "lifecycle(warn)"
)
method(gen1, BClass) <- function(x, ...) "result"
gen1(BClass())
#> Warning: `gen1()` was deprecated in <NA> 2.0.0.
#> ℹ Please use `gen2()` instead.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> [1] "result"
```

You can later remove `gen1` once dependencies have had time to move on.

So the process is:

1.  You create the new generic, turn the old one into a deprecated alias
    with `deprecated_generic(new = )`, and release.
2.  Each dependency switches to the new name, and releases whenever
    convenient.
3.  You remove the old name in a later release.

### Removing a generic

To retire a generic without replacement, use
[`deprecated_generic()`](https://rconsortium.github.io/S7/reference/deprecated_generic.md)
without the `new` argument:

``` r

# yourPackage, version 1
gen := new_generic("x")

# yourPackage, version 2
gen := deprecated_generic("x", when = "2.0.0")
method(gen, BClass) <- function(x, ...) "result"
gen(BClass())
#> Warning in gen(BClass()): `gen()` was deprecated in version 2.0.0.
#> [1] "result"
```

Existing method registration will continue to work (with a warning),
giving dependencies time to move away from the generic before you remove
it in a future release.

So the process is:

1.  You replace the generic with
    [`deprecated_generic()`](https://rconsortium.github.io/S7/reference/deprecated_generic.md),
    and release.
2.  Each dependency stops using the generic, and releases whenever
    convenient.
3.  You remove the generic in a later release.

### Moving a generic to another package

Splitting generics out into a lower-dependency package (say
yourPackageCore) works, as long as yourPackage re-exports them:

``` r

# yourPackage, version 2
#' @importFrom yourPackageCore gen
#' @export
yourPackageCore::gen
```

depPackage’s `importFrom(yourPackage, gen)` still resolves, and method
registration follows the generic object to its new home package
automatically.

You must re-export using the `NAMESPACE`, not assignment:

``` r

# DO NOT DO THIS
#' @export
gen <- yourPackageCore::gen
```

This captures a copy of the generic when yourPackage is installed. The
result is a split brain: depPackage’s registrations follow the generic’s
home package and land on yourPackageCore’s original, while calls through
`yourPackage::gen()` dispatch using the duplicate’s empty table and
fail.

Should the re-export signal deprecation? Since a re-export costs nothing
to maintain, keeping it forever is a fine choice. If you do want to
retire the old home, export a deprecated alias:

``` r

# In yourPackage, version 2
gen := deprecated_generic(new = yourPackageCore::gen, when = "2.0.0")
```

So the process is:

1.  You release yourPackageCore with the generic.
2.  You re-export the generic from yourPackage via `NAMESPACE`, and
    release.
3.  Optionally, you later replace the re-export with a deprecated alias,
    each dependency switches to yourPackageCore, and you eventually
    remove the alias.

## Changing a class

Classes can evolve without breaking their subclasses in other packages
because S7 never inlines a parent constructor across package boundaries.
To see why this matters, compare the two cases. When classes live in the
same package, the subclass’s constructor inlines the parent’s arguments,
so you can see every property it takes:

``` r

class1 := new_class(package = "foo", properties = list(a = class_any))
class2 := new_class(class1, package = "foo", properties = list(b = class_any))
class2@constructor
#> function (a = NULL, b = NULL) 
#> S7::new_object(class1(a = a), b = b)
#> <environment: 0x560cfd656028>
```

When the parent lives in another package, you refer to it with
[`new_external_class()`](https://rconsortium.github.io/S7/reference/new_external_class.md):

``` r

class1 := new_external_class(package = "foo")
class2 := new_class(class1, properties = list(b = class_any))
```

Now the constructor of `class2` can’t inline the parent’s arguments,
because the parent’s properties (and hence its constructor) might change
in a future version of foo. Instead, it takes `...` and passes them on
to `foo::class1()` at run time, followed by its own property, `b`. This
is the key idea that allows classes to evolve over time without
immediately breaking their dependencies.

The following sections cover each way a class can change:

- [Adding a property](#adding-a-property)
- [Removing a property](#removing-a-property)
- [Renaming a property](#renaming-a-property)
- [Changing a property’s type or
  validator](#changing-a-propertys-type-or-validator)
- [Renaming a class](#renaming-a-class)
- [Removing a class](#removing-a-class)
- [Moving a class](#moving-a-class)

### Adding a property

Adding a property is usually harmless, but there are two ways it can
still bite:

- **Name clash.** If depPackage has a subclass and that subclass uses a
  property with the same name, depPackage will fail to install:

      Error: <MyFoo>@y must narrow <yourPackage::Foo>@y.

  The easiest resolution to this problem is for you to pick a different
  name. If that doesn’t work, you’ll need to ask the depPackage
  maintainer to rename their property using the process described below.

- **Positional construction.** `yourPackage::Foo(1, 2)` changes meaning
  if a property is inserted before an existing one. In general, it’s
  best to add new properties at the end so positional calls keep their
  meaning, but inserting is usually OK because most callers use named
  arguments.

So the process is:

1.  You check whether any dependency has a subclass with a property of
    the same name.
2.  If so, you pick a different name (or ask the dependency to rename
    its property first).
3.  You release.

### Removing a property

Removing a property will cause run-time errors: `Foo(count = 2)` will
fail with `unused argument` and `x@count` with `Property not found`. To
give dependencies time to adapt, deprecate the property with
[`deprecated_property()`](https://rconsortium.github.io/S7/reference/deprecated_property.md):

``` r

# yourPackage, version 1
validate_count <- function(value) {
  if (any(value < 0)) "must be non-negative"
}
Foo := new_class(properties = list(
  count = new_property(class_double, default = 1, validator = validate_count)
))

# yourPackage, version 2
Foo := new_class(properties = list(
  deprecated_property(
    "count", when = "2.0.0", class = class_double, default = 1,
    validator = validate_count
  )
))

foo <- Foo(count = 3)
foo@count
#> Warning: `<Foo>@count` was deprecated in version 2.0.0.
#> [1] 3
foo@count <- 4
#> Warning: `<Foo>@count` was deprecated in version 2.0.0.
print(foo)
#> <Foo>
```

So the process is:

1.  You replace the property with
    [`deprecated_property()`](https://rconsortium.github.io/S7/reference/deprecated_property.md),
    and release.
2.  Each dependency stops using the property, and releases whenever
    convenient.
3.  You remove the property in a later release.

### Renaming a property

A rename is just an add and a remove, supplying the name of the `new`
property to
[`deprecated_property()`](https://rconsortium.github.io/S7/reference/deprecated_property.md):

``` r

Foo := new_class(properties = list(
  size = new_property(class_double, default = 1),
  deprecated_property("count", new = "size", when = "2.0.0")
))

foo <- Foo(size = 3) # silent
foo <- Foo(count = 3) # warns, then sets size
#> Warning: `<Foo>@count` was deprecated in version 2.0.0.
#> Please use `<Foo>@size` instead.
foo@count
#> Warning: `<Foo>@count` was deprecated in version 2.0.0.
#> Please use `<Foo>@size` instead.
#> [1] 3
foo@count <- 4
#> Warning: `<Foo>@count` was deprecated in version 2.0.0.
#> Please use `<Foo>@size` instead.
foo@size
#> [1] 4
```

So the process is:

1.  You add the new property, replace the old one with
    `deprecated_property(new = )`, and release.
2.  Each dependency switches to the new name, and releases whenever
    convenient.
3.  You remove the old property in a later release.

### Changing a property’s type or validator

Widening a property’s type (say, `class_integer` to `class_numeric`) is
safe for depPackage: everything that validated before still validates.
Narrowing causes run-time errors: any code that stored a now-invalid
value fails when it runs:

    Error: <yourPackage::Foo> object properties are invalid:
    - @size must be <integer>, not <double>

The same applies to tightening a validator. There’s no automatic fix
here, but you can give dependencies a deprecation period by generating a
warning in the validator:

``` r

# yourPackage, version 1
Foo := new_class(properties = list(size = class_double))

# yourPackage, version 2: warn instead of failing
Foo := new_class(
  properties = list(size = class_double),
  validator = function(self) {
    if (self@size != trunc(self@size)) {
      warning("`size` should be a whole number; this will become an error in a future release.")
    }
    NULL
  }
)
foo <- Foo(size = 3.5)
#> Warning in validator(object): `size` should be a whole number; this will become
#> an error in a future release.
foo
#> <Foo>
#>  @ size: num 3.5
```

If you use lifecycle,
[`lifecycle::deprecate_warn()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)
is a good choice here: it rate-limits to one warning every 8 hours, so
dependencies that construct many objects aren’t flooded.

So the process is:

1.  You make the validator warn about values that will become invalid,
    and release.
2.  Each dependency fixes the code that triggers the warning, and
    releases whenever convenient.
3.  You narrow the type or tighten the validator in a later release.

### Renaming a class

To rename a class, create the new class, then redefine the old one with
[`deprecated_class()`](https://rconsortium.github.io/S7/reference/deprecated_class.md),
passing the replacement to `new` and recording the version in `when`.

``` r

# yourPackage, version 1
Foo := new_class(properties = list(size = class_double))
saved <- Foo(size = 1)
MyFoo := new_class(parent = Foo)
measure := new_generic("x")
method(measure, Foo) <- function(x) x@size

# yourPackage, version 2: export both classes
Bar := new_class(properties = list(size = class_double))
Foo := deprecated_class(
  properties = list(size = class_double),
  new = Bar,
  when = "2.0.0"
)
foo <- Foo(size = 2) # warns and still constructs a Foo
#> Warning in Foo(size = 2): `Foo()` was deprecated in version 2.0.0.
#> Please use `Bar()` instead.
measure(foo)
#> [1] 2
measure(MyFoo(size = 3)) # existing subclasses still work, without warning
#> [1] 3
measure(saved)
#> [1] 1
S7_inherits(saved, Foo)
#> [1] TRUE
S7_inherits(saved, Bar)
#> [1] FALSE
```

Methods for `Foo` do not automatically apply to `Bar`. When a method can
handle both classes, register it on their union (this replaces the
existing `Foo` method with an identical one, hence the message):

``` r

method(measure, Foo | Bar) <- function(x) x@size
#> Overwriting method measure(<Foo>)
measure(Bar(size = 4))
#> [1] 4
measure(saved)
#> [1] 1
```

If the replacement needs a different representation, keep separate
methods and provide an explicit conversion for users who want it.

So the process is:

1.  You create the new class, replace the old one with
    `deprecated_class(new = )`, register shared methods on the union,
    and release.
2.  Each dependency switches to the new class, and releases whenever
    convenient.
3.  You remove the old class in a later release.

### Removing a class

To remove a class without a replacement, redefine it with
[`deprecated_class()`](https://rconsortium.github.io/S7/reference/deprecated_class.md),
keeping its existing properties and omitting `new`:

``` r

Retired := deprecated_class(
  properties = list(size = class_double),
  when = "2.0.0"
)
Retired(size = 3)
#> Warning in Retired(size = 3): `Retired()` was deprecated in version 2.0.0.
#> <Retired>
#>  @ size: num 3
```

Subclasses, method signatures, property types, and unions in depPackage
can continue using the class, but constructors will warn.

So the process is:

1.  You replace the class with
    [`deprecated_class()`](https://rconsortium.github.io/S7/reference/deprecated_class.md),
    and release.
2.  Each dependency stops using the class, and releases whenever
    convenient.
3.  You remove the class in a later release.

### Moving a class

You can use the same technique as renaming to move a class between
packages, because changing the class’s `package` also changes its
identity. To recommend a class in another package while preserving
existing uses, use
[`deprecated_class()`](https://rconsortium.github.io/S7/reference/deprecated_class.md):

``` r

# In yourPackage
Foo := deprecated_class(
  properties = list(size = class_double),
  new = yourPackageCore::Foo,
  when = "2.0.0"
)
```

So the process is:

1.  You release yourPackageCore with the class.
2.  You replace the class in yourPackage with
    `deprecated_class(new = )`, and release.
3.  Each dependency switches to yourPackageCore’s class, and releases
    whenever convenient.
4.  You remove the old class in a later release.

## Designing for evolution

Most of the pain described above can be avoided by a few decisions made
when you first design your interface. Most of these recap advice from
earlier sections:

- **Treat names as permanent.** While there are ways to “rename” classes
  and generics, it’s best to think of a rename as creating a new class
  or generic and deprecating the old one. The same goes for a generic’s
  dispatch arguments and a class’s package.

- **Give every generic `...`** (the default), so it can gain arguments
  later (see [Adding an argument](#adding-an-argument)).

- **Keep generics as small as possible.** Every argument in the
  generic’s signature (even those that don’t take part in dispatch) is
  part of its contract, and any changes need careful thought. If an
  argument is only meaningful for some methods, leave it out of the
  generic and let those methods take it via `...`.

- **Call constructors by name**, not position, so adding a property
  never changes the meaning of a call (see [Adding a
  property](#adding-a-property)).

## Summary

| Change | What breaks in depPackage | What to do |
|----|----|----|
| Change dispatch arguments | Every method: check error, calls fail | Create a new generic and deprecate the old one |
| Add an argument (generic has `...`) | Check NOTE only | depPackage adds the argument first, then you release |
| Add an argument (no `...`) | Every method: check error, calls fail | Treat as a rename |
| Remove an argument | Callers that supply it | Deprecate the argument, then remove |
| Change a default | Check NOTE only | Avoid |
| Rename a generic | Install, via `importFrom()` | `deprecated_generic(new = )` |
| Remove a generic | Install, via `importFrom()` | [`deprecated_generic()`](https://rconsortium.github.io/S7/reference/deprecated_generic.md) |
| Move a generic | Nothing | Re-export via `NAMESPACE` |
| Add a property | Install, if a subclass has a clashing property | Pick a different name |
| Remove a property | Construction and access, at run time | [`deprecated_property()`](https://rconsortium.github.io/S7/reference/deprecated_property.md) |
| Rename a property | Construction and access, at run time | `deprecated_property(new = )` |
| Narrow a property’s type or validator | Objects with now-invalid values, at run time | Warn in the validator first |
| Rename a class | Nothing immediately | `deprecated_class(new = )` |
| Remove a class | Nothing immediately | [`deprecated_class()`](https://rconsortium.github.io/S7/reference/deprecated_class.md) |
| Move a class | Nothing immediately | `deprecated_class(new = )` |
