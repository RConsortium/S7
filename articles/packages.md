# Using S7 in a package

This vignette outlines the most important things you need to know about
using S7 in a package. S7 is new, so few people have used it in a
package yet; this vignette is likely incomplete, and we’d love your help
to make it better. Please [let us
know](https://github.com/RConsortium/S7/issues/new) if you have
questions that this vignette doesn’t answer.

``` r

library(S7)
```

## Getting started

First, add `S7` to the `Imports` field of your `DESCRIPTION`. We then
recommend importing all S7 functions into your package `NAMESPACE` with
`import(S7)`, or, if you’re using roxygen2, `@import S7`.

Next, create a `.onLoad()` in `zzz.R` that calls
[`methods_register()`](https://rconsortium.github.io/S7/reference/methods_register.md):

``` r

.onLoad <- function(...) {
  S7::methods_register()
}
```

This is S7’s way of registering methods, rather than using export
directives in your `NAMESPACE` like S3 and S4 do. This is only strictly
necessary if you are registering methods for generics in other packages,
but there’s no harm in adding it and it ensures that you won’t forget
later.

## Classes

If you want users to create instances of your class, you will need to
export the class constructor. That means you will also need to document
it, and since the constructor is a function, you have to document the
arguments, which will be the properties of the class (unless you have
customized the constructor). If you export a class (i.e. its
constructor), you must also set the `package` argument, ensuring that
classes with the same name are disambiguated across packages.

NB: if your package creates a hierarchy of classes, subclasses must be
defined *after* the parent classes. That means if you define the classes
in separate files, you will need to use the `DESCRIPTION` Collate field
(or the equivalent roxygen2 `@include` tag) to ensure the files are
loaded in the correct order.

## Generics and methods

You should document generics like regular functions (since they are!).
If you expect others to create their own methods for your generic, you
may want to include a section describing the properties that you expect
all methods to have. If you want to list all methods for a generic, you
can use the [doclisting](https://doclisting.r-lib.org) package.

If you use roxygen2, you can document S7 generics and methods by
following the advice in
[`vignette("rd-S7", package = "roxygen2")`](https://roxygen2.r-lib.org/articles/rd-S7.html).

Note that methods can only be defined after both the class and generic
have been defined. If generics/methods/classes live in different files,
you will need to use the `DESCRIPTION` Collate field (or the equivalent
roxygen2 `@include` tag) to ensure the files are loaded in the correct
order.

## Backward compatibility

### S3

S7 objects are S3 objects, so it’s possible to manually register an S7
method for an S3 generic without using `method<-`. The main thing to
note is that the S3 class name of an S7 object is `{package}::{class}`,
which means you’ll need to wrap the method name in `` ` ``:

``` r

Foo <- new_class("Foo", package = "S7")
`length.S7::Foo` <- function(x) 10
length(Foo())
#> [1] 10
```

### Older versions of R

If you are using S7 in a package *and* you want your package to work in
versions of R before 4.3.0, you need to know that in these versions of R
`@` only works with S4 objects. There are two workarounds. The easiest
but least convenient workaround is to just use
[`prop()`](https://rconsortium.github.io/S7/reference/prop.md) instead
of `@`. Otherwise, you can conditionally make an S7-aware `@` available
to your package with this custom `NAMESPACE` directive:

``` r

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL
```

`@` will work for users of your package because S7 automatically
attaches an environment containing the needed definition when it’s
loaded.
