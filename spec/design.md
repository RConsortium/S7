# Data Structures
## Object

We define an object in this system as any R object with:
- A class attribute, a character vector of class names, following S3
  semantics, with a direct reference to the class object
- Potentially attributes storing class-dependent property values

## Class

In accordance with requirement #2, classes are first class objects,
with the following components:
- Name
- Parent class object
  - Single inheritance (requirement #6)
- Property list
- Method list
  - We are able to assign methods to a class, because we assume single
    and nested (requirement #5) dispatch.
- Initializer

The class object inherits from function and acts as a constructor of
instances of the class. The class definer implements the constructor,
which takes a custom set of arguments and ultimately delegates to
`Class$new()`, which accepts any parent instance and property values
and returns an instance of the class so initialized. 

For example, a constructor a `Range` class might look like:
```{R}
function(start, end) {
    stopifnot(is.numeric(start), is.numeric(end), end >= start)
    Range$new(start=start, end=end)
}
```

Initializing an instance of a class will:
1. Create the prototype, by either adding the class label to the
   parent instance or constructing an empty S4SXP,
1. Merge any given property values, and
1. Validate and return the result.

The first two steps would be very similar to calling `structure()`,
except it would initialize property values through the property system
(see below). We could allow the user to override the first two steps,
as one would in the methods package by defining a method on
`initialize()`.

### Property

A property is an encapsulated component of the object state that is
publicly accessible via a simple syntax. The motivation for
properties is that R users expect transparency; they want to get to
the actual data inside an object and directly manipulate it to get
their work done, without worrying about bespoke APIs. Properties
enable them to work that way without actually violating encapsulation
and exposing implementation details.

Every property definition has a:
- name
- value class
- default value (itself defaulting to the value class prototype) and, 
- optional function that implements the getting and setting, much like
  an active binding (by default, stores value as attribute, like S4 slot). 

The advantage of a property over an S4 slot is that the developer can,
at a later date, change the representation of the object and specify a
getter/setter that redirects to the new representation. There will be
built-in support for emitting deprecation messages.

While it would be tempting to support public vs. private scoping on
properties, it is not obvious how to support that, because no code is
more privileged than any another. Nor is it clear whether the
package should define the boundary, when packages sometimes define
methods on classes defined in other packages and want to take
advantage of the internal representation. The encapsulation afforded
by properties is a good compromise.

Properties are defined in the class structure. The `prop()` and
`prop<-()` functions implement getting and setting, respectively, in a
way that is compatible with S4. The `prop()` function could be
implemented as:
```{R}
     function(x, name) {
         properties(class(x))[[name]](x)
     }
```

   In practice, we would implement this in native code for efficiency.

## Generic

A generic is a function that dispatches to methods. For introspection
purposes, it knows its name and the names of the arguments in its
signature (the arguments considered during dispatch).

# Dispatch
Dispatch will be nested, meaning that if there are multiple
arguments in the generic signature, it will dispatch on the first
argument, then the second. Nested dispatch is likely easier to
predict and understand compared to treating all arguments with equal
precendence. Nested dispatch is also easier to implement
efficiently, because classes would effectively inherit methods, and
we could implement that inheritance using environments.
	
For example, a `plot()` generic dispatching on `x` could be
implemented like this:
```{R}
plot <- function(x) {
    methods(class(x))$plot(x)
}
```
  
For multiple dispatch, we could apply the builder pattern. 
While a `publish()` that publishes an object `x` to a destination
`y`, dispatching on both arguments, could be:
```{R}
publish <- function(x, y) {
    methods(class(x))$publish(x, y)
}
```
where `class(x)$publish` returns the pregenerated
```{R}
function(x, y) {
    methods(class(y))$publish.plot(x, y)
}
```
assuming `x` is a "plot" object.

Alernatively, the generics could just call `UseMethod()`, whiich would
gain support for nested dispatch.

# API

## Programmatic

### Objects
 - `classObject(x)`: Get the class object for object `x`.
 - `prop(x, name)`, `prop<-(x, name, value)`: Get and set a property value

### Classes
Calling `defineClass()` defines a new class. Its signature:
```{R}
defineClass <- function(name, parent = object, constructor, init = structure,
                        validity = function(object) { },
                        slots = NULL)
```

Example:
```{R}
Child <- defineClass("Child", Parent, function(object, ...) {
    structure(object, ...)
}, validity = function(object) {
    if (object@slot2 >= object@slot1)
        "slot2 must be less than slot1"
}, slots = c(slot1 = "integer", slot2 = "integer"))
```

### Generics

Calling `defineGeneric()` defines a new generic, with signature:
```{R}
function(name, FUN, signature) { }
```
The `signature` would default to all of the arguments in
`formals(FUN)`. The body of `FUN` would resemble S3 and S4
generics. It might just call `UseMethod()`.

### Methods

Calling `defineMethod()` defines a new method, with the signature:
```{R}
function(generic, signature, FUN) { }
```

The `generic` is the actual generic function object (not its name), and
the `signature` is a list of class objects.

## Sugar

### Objects
	- `@()` and `@<-()` get and set property values, like S4 slots
	
### Classes

One crazy syntax idea for defining a class:
```{R}
MyClass <- Class(slot1 := integer,
                 slot2 := character = "default") :=
    SuperClass |> valid_when(cond1, cond2, ...)
```

### Methods

Assign the method into the generic, as a member of its dispatch table:

```{R}
generic$Class <- function(x) { }
```

In the above, `Class` would be looked up by name in the parent
frame. A safer and slightly more verbose syntax, which passes the
actual class object:
```{R}
generic[[Class]] <- function(x) { }
```

For multiple (nested) dispatch:
```{R}
generic[[Class1]][[Class2]] <- function(x) { }
```

Methods are looked up in the same way (incorporating inheritance).

# Compatibility

## S3

Since the class attribute has the same semantics as S3, S3 dispatch
should be fully compatible. The new generics should also be able to
handle legacy S3 objects.

## S4

### Generics

In principle, we could modify the methods package so that S4 generics
can operate on the new class definitions. Since the new generics will
fallback to S3 dispatch, they should support S4 objects just as S3
generics support them now.

### Classes

# Documentation

The primary challenge is that the availability of methods and classes
depends on which packages are installed, so we will need to generate
index pages that list the methods for a generic or the methods with a
particular class in their signature. The documentation blurbs could be
lazily scraped at runtime to build an index, similar to how the help
search index is created now. We would then generate help pages from
the index.

The index of installed methods could provide additional benefits,
including introspection. A generic could prompt the user to load a
package if it fails to find an applicable method, while there is an
installed, unloaded method available. This casd would arise when a
method is defined outside of the packages that define the class and
generic.
