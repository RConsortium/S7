# Design specification

This document presents a broad overview of the system.

## Objects

We define an object in this system as any R object with:

-   A class object attribute, a reference to the **class object**, and retrieved with `classObject()`.
-   For S3 compatibility, a class attribute, a character vector of class names.
-   Additional attributes storing **properties** defined by the class, accessible with `@`/`prop()`.

## Classes

In accordance with requirement \#2, classes are first class objects.
A class object is a function which can be called to construct an object of that class.
It has the following components:

-   **Name**, a human-meaningful descriptor for the class.
    This is used for print method and error messages; it does not identify the class.

-   **Parent**, the class object of the parent class (Single inheritance; requirement \#6).

-   **Constructor**, an user-facing function used to create new objects of this class.
    It always ends with a call to `newObject()` to initialize the class.
    This the function (wrapped appropriately) that represents the class.

-   **Validator**, a function that takes the object and returns `NULL` if the object is valid, otherwise a character vector of error messages (like the methods package).

-   **Properties**, a list of property objects that define object data.

Each component corresponds to an argument in `defineClass()`:

``` {.r}
defineClass(
  name, 
  parent = object, 
  constructor = function(...) newObject(...), 
  validity = function(object) NULL,
  properties = list()
)
```

For convenience:

-   `parent` can be a string.
    This string will be used to look for a class object in the calling frame.

-   `properties` can be a named character vector, which will be converted to a formal list of property objects.
    For example, `c(name = "character", age = "integer")` is shorthand for `list(defineProperty("name", "character"), defineProperty("age", "integer"))`.

For example:

``` {.r}
Range <- defineClass("Range", 
  Vector, 
  constructor = function(start, end) {
    stopifnot(is.numeric(start), is.numeric(end), end >= start)
    newObject(start = start, end = end)
  },
  validity = function(object) {
    if (end < start) {
      "end must be greater than or equal to start"
    }
  }, 
  properties = c(start = "numeric", end = "numeric")
)

Range(start = 1, end = 10)
```

### Initialization

Initializing an instance of a class with `newObject()`:

1.  Inspects the enclosing scope to find the "active" class.
2.  Creates the prototype, by either by calling the parent constructor or by creating a base type and adding `class` and `classObject` attributes to it.
3.  Validates properties then adds to prototype.
4.  Validates the complete object.

Steps 2 and 3 are similar to calling `structure()`, except that property values will be initialized and validated through the property system.

### Validation

Objects will be validated on construction and every time a property is modified.
To temporarily opt-out of validation (e.g. when you need to transition through a temporarily invalid state) the system will provide `eventuallyValid()`:

``` {.r}
eventuallyValid <- function(object, fun) {
  object$internal_validation_flag <- FALSE
  out <- fun(object)
  out$internal_validation_flag <- TRUE
  validate(out)
}
```

For example, if you wanted to move a Range object to the right, you could write:

``` {.r}
move_right <- function(x, y) {
  eventuallyValid(x, function(x) {
    x@start <- x@start + y
    x@end <- x@end + y
    x
  })
}
```

This ensures that the validation will not trigger if `x@start + y` is greater than `x@end`.

The system also provides `implicitlyValid()` for expert use only.
This is similar to `eventuallyValid()` but does not check for validity at the end.
This can be used in performance critical areas where you can ascertain that a sequence of operations can never make an valid object invalid[^1].

[^1]: This is generally hard.
    For example, in the `move_right()` example above, you might think that that if `x@start < x@end` is true at the beginning, then `x@start + y < x@end + y` will still be true at the end, and you don't technically need to re-validate the object.
    But that's actually not true: if you assume `x@start == 1` and `x@end == 2`, then `x@start + y == x@end + y` (i.e. they're equal!), as soon as `abs(y) > 2e16`, i.e. for very many values of `y`.

## Properties

A property is an encapsulated component of the object state that is publicly accessible via a simple syntax.
The motivation for properties is that R users expect transparency; they want to get to the actual data inside an object and directly manipulate it to get their work done, without worrying about bespoke APIs.
Properties support typical usage while protecting encapsulation and hiding implementation details.

Compared to S3 attributes, properties are considerably stricter: a class defines the names and types of its properties.
Compared to S4 slots, properties enable an object to change its internals without breaking existing usage because it can provides a custom accessor that redirects to the new representation.
There will be built-in support for emitting deprecation messages.

Every property definition has a:

-   A **name**, used to label output for humans.
-   An optional **class**
-   A **default value** that is (itself defaulting to the value class prototype)
-   An optional **accessor** function that overrides getting and setting, much like an active binding (by default, the value is stored as attribute, like S3/S4).

Property objects are created by `defineProperty()`:

``` {.r}
newProperty(
  name, 
  class = NULL, 
  default = NULL, 
  accessor = NULL
)
```

While it would be tempting to support public vs. private scoping on properties, it is not obvious how to support that, because no code is more privileged than any another.
Nor is it clear whether the package should define the boundary, when packages sometimes define methods on classes defined in other packages and want to take advantage of the internal representation.
The encapsulation afforded by properties is a good compromise.

Properties of an object can be accessed using `@`/`@<-` or `prop()`/`prop<-`.
Setting the properties of an object always triggers validation.

## Generic

A generic is a function that provides an interface with implementation provided by methods.
For introspection purposes, it knows its name and the names of the arguments in its signature (the arguments considered during dispatch).

Calling `newGeneric()` defines a new generic.
It has the signature:

``` {.r}
newGeneric(name, FUN, signature)
```

The `signature` would default to the first argument, i.e. `formals(FUN)[1]`.
The body of `FUN` would resemble S3 and S4 generics.
It might just call `UseMethod()`.

## Methods

### Creation

Methods are defined by calling `method<-(generic, signature, method)`:

``` {.r}
method(generic, signature) <- function(x, ...) {} 
```

-   `generic` is either a function or a string representing the name of a function which is looked up in the calling frame.

-   `signature` is a single class object, list of class objects, or a character vector.
    If a character vector is supplied, class objects are searched for in the calling frame.

`method<-` performs validation ensuring that the method is compatible with the generic (i.e. all arguments before `...` have the same names in the same order; if the generic doesn't have `...` all arguments must be same).

Documentation will discuss the risks of defining a method when you don't own either the generic or the class.

`method<-` is designed to work at run-time (not just package build-time) so that methods can be defined when suggested packages are loaded later:

``` {.r}
whenLoaded("pkg", {
  method(mean, pkg::A) <- function() 10
  method(sum, pkg::A) <- function() 5
})
```

### Dispatch

Dispatch will be nested, meaning that if there are multiple arguments in the generic signature, it will dispatch on the first argument, then the second.
Nested dispatch is likely easier to predict and understand compared to treating all arguments with equal precendence.
Nested dispatch is also easier to implement efficiently, because classes would effectively inherit methods, and we could implement that inheritance using environments.

For example, a `plot()` generic dispatching on `x` could be implemented like this:

``` {.r}
plot <- function(x) {
  methods(classObject(x))$plot(x)
}
```

For multiple dispatch, we could apply the builder pattern.
While a `publish()` that publishes an object `x` to a destination `y`, dispatching on both arguments, could be:

``` {.r}
publish <- function(x, y) {
  methods(classObject(x))$publish(x, y)
}
```

where `class(x)$publish` returns the pregenerated

``` {.r}
function(x, y) {
  methods(classObject(y))$publish.plot(x, y)
}
```

assuming `x` is a "plot" object.

Alternatively, the generics could just call `UseMethod()`, which would gain support for nested dispatch.

# Compatibility

## S3

Since the class attribute has the same semantics as S3, S3 dispatch should be fully compatible.
The new generics should also be able to handle legacy S3 objects.

## S4

### Generics

In principle, we could modify the methods package so that S4 generics can operate on the new class definitions.
Since the new generics will fallback to S3 dispatch, they should support S4 objects just as S3 generics support them now.

### Classes

# Documentation

The primary challenge is that the availability of methods and classes depends on which packages are installed, so we will need to generate index pages that list the methods for a generic or the methods with a particular class in their signature.
The documentation blurbs could be lazily scraped at runtime to build an index, similar to how the help search index is created now.
We would then generate help pages from the index.

The index of installed methods could provide additional benefits, including introspection.
A generic could prompt the user to load a package if it fails to find an applicable method, while there is an installed, unloaded method available.
This casd would arise when a method is defined outside of the packages that define the class and generic.
