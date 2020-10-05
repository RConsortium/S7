# Data Structures
## Object

We define an object in this system as any R object with:

- A class attribute, a character vector of class names (for S3 compatibility)
- A direct reference to the **class object**, retrieved with `classObject()`.
- Additional attributes storing class-dependent **properties**, accessible
  with `@`.
- Classes, generics, and methods are created with `newClass()`, `newGeneric()`, and `newMethod()` respectively, and only have impact when assigned.

## Class

In accordance with requirement #2, classes are first class objects,
with the following components:
- Name
- Parent class object
  - Single inheritance (requirement #6)
- Property list
- Validator
- Constructor

The class object inherits from function and acts as a constructor of
instances of the class. The class definer implements the constructor,
which takes a custom set of arguments and calls `newObject()` to create
the object.

For example, a constructor a `Range` class might look like:

```{R}
function(start, end) {
    stopifnot(is.numeric(start), is.numeric(end), end >= start)
    newObject(start = start, end = end)
}
```

Initializing an instance of a class will:
1. Create the prototype, by either adding the class label to the
   parent instance or constructing an empty S4SXP,
1. Merge any given property values, and
1. Validate and return the result.

The first two steps would be very similar to calling `structure()`,
except it would initialize property values through the property system
(see below). 

The second step, merging property values, is also useful for updating
an object (immutably, so returning a new object). This corresponds to
the `methods::initialize()`, which also runs the validator. We would
separate the (re)initialization from validation. This would allow the
developer to update the object in controlled situations without
triggering potentially expensive or redundant validation. If we
implement initialization through a generic, the developer can
override it for a particular class.

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
         properties(classObject(x))[[name]]
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
    methods(classObject(x))$plot(x)
}
```
  
For multiple dispatch, we could apply the builder pattern. 
While a `publish()` that publishes an object `x` to a destination
`y`, dispatching on both arguments, could be:
```{R}
publish <- function(x, y) {
    methods(classObject(x))$publish(x, y)
}
```
where `class(x)$publish` returns the pregenerated
```{R}
function(x, y) {
    methods(classObject(y))$publish.plot(x, y)
}
```
assuming `x` is a "plot" object.

Alternatively, the generics could just call `UseMethod()`, which would
gain support for nested dispatch.

# API

## Programmatic

### Objects

 - `classObject(x)`: Get the class object for object `x`.
 - `prop(x, name)`, `prop<-(x, name, value)`: Get and set a property value
 - `x@name` and `x@name <- value`: Shortcut to get and set property values


### Classes

Calling `newClass()` creates a new class. Its signature:

```{R}
defineClass(
  name, 
  parent = object, 
  constructor, 
  init = structure,
  validity = function(object) { },
  properties = list()
)
```

where:
 - `name` is the name of the class
 - `parent` is the class object for the parent class
 - `constructor` is an arbitrary function that typically ends with a
   call to `newObject()`
 - `validity` a function that takes the object and returns a vector of
   error messages, or `NULL` (like the methods package)
 - `properties` is either:
   - a character vector mapping property names to types 
     (converted into Property objects that store their values in attributes) 
   - or a list of Property objects

The return value is `constructor`, except it is an instance of class
`Class`, which defines properties that describe the rest of the class.

Example:

```{R}
Range <- defineClass("Range", 
  Vector, 
  function(start, end) {
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

### Properties

Calling `newProperty()` define a new property. It has the
signature:

```{R}
newProperty(name, class, default, accessor)
```

where:
 - `name` is the name of the property,
 - `class` is the class object for the property value,
 - `default` is the default value of the property on new instances,
 - `accessor` is a function that gets and sets the property value,
   such as through attributes on the object, and behaves similar to an
   active binding function.

### Generics

Calling `newGeneric()` defines a new generic. It has the signature:

```{R}
newGeneric(name, FUN, signature)
```

The `signature` would default to the first argument, i.e. `formals(FUN)[1]`. 
The body of `FUN` would resemble S3 and S4 generics. It might just call `UseMethod()`.

### Methods

Calling `newMethod()` defines a new method, with the signature:

```{R}
newMethod(generic, signature, FUN)
```

The `generic` is the actual generic function object (not its name), and
the `signature` is a list of class objects.

## Sugar

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
generic[[classObject]] <- function(x) { }
```

For multiple (nested) dispatch:
```{R}
generic[[classObject1]][[classObject2]] <- function(x) { }
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
