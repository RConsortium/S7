This page is for brainstorming on the technical requirements for solving our [problem](https://github.com/RConsortium/OOP-WG/wiki/Problem-Statement). Once we align on the requirements, we can start the design process.

## List of requirements
1. The system should be as compatible as possible with existing systems, particularly S3
1. Classes should be first class objects, extrinsic from instances
1. It should be convenient to systematically validate an instance
1. Double dispatch, and possibly general multiple dispatch, should be supported
1. Inheritance should be as simple as possible, ideally single (other features might compensate for the lack of multiple inheritance)
1. Syntax should be intuitive and idiomatic, and should not rely on side effects nor loose conventions
1. Namespace management should not be any more complicated than S3 currently
1. Performance should be competitive with existing solutions
1. The design should be simple to implement

## Compatibility

Ideally the new system will be an extension of S3, because S3 is already at the bottom of the stack and many of the other systems have some compatibility with S3.

## Classes as first class objects

It is important for classes to be defined extrinsically from instances, because it makes the data contract more obvious to both developers (reading the code) and users (interacting with objects). S4 represents classes as proper objects; however, typically developers will only refer to the classes by name (string literal) when interacting with the S4 API. Interacting directly with objects instead would likely simplify the API (syntax) and its implementation.

Class objects need to be namespaced so that it's method dispatch still occurs correctly even when multiple packages implement a class with the same (string) name.

## Generics as extended function objects

Generic functions should be represented by a special class of function object that tracks its own method table. This puts generic functions at the same level as classes, which is the essence of functional OOP, and will likely enable more natural syntax in method registration.
 
## Systematic validation

Class contracts are often more complicated than a simple list of typed fields. Having a formally supported convention around validating objects is important so that code can make the necessary assumptions about data structures and their content. Otherwise, developers trying to be defensive will resort to ad hoc checks scattered throughout the code.
 
## Multiple dispatch

The system will at least need to support double dispatch, so that code can be polymorphic on the interaction of two arguments. There are many obvious examples: arithmetic, serialization (object, target), coercion, etc. It is less likely that we will require dispatch beyond two arguments, and any advantages are probably outweighed by the increase in complexity. In many cases of triple dispatch or higher in the wild, developers are abusing dispatch to implement type checks, instead of polymorphism. Almost always, we can deconstruct multiple dispatch into a series of double dispatch calls. General multiple dispatch makes software harder to understand and is more difficult to implement.

## Inheritance

Inheritance lets us define a data taxonomy, and it is often natural to organize a set of data structures into multiple, orthogonal taxonomies. Multiple inheritance enables that; however, it also adds a lot of complexity, both to the software relying on it and the underlying system. It is often possible and beneficial (in the long term) to rely on techniques like composition and delegation instead. We should consider how single inheritance languages like Java have been so successful, although they are not directly comparable to our functional system. 

## Syntax

Ideally the entire API could be free of side effects and odd conventions, like making the `.` character significant in method names. Direct manipulation of class and generic objects should enable this.

## Namespaces

The system should support exporting generics and classes. If classes are objects, they can be treated like any other object when exporting and importing symbols. If generics are objects, then it should be simple to export all methods on a generic.  It is not clear whether selective method export is important. One use case would be to hide a method with an internal class in its signature to avoid unnecessary documentation. Perhaps `R CMD check` could ignore methods for unexported classes. There should be no need for explicit method registration.  

