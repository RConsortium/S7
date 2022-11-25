Open Generic Registration
================

- <a href="#preamble" id="toc-preamble">Preamble</a>
- <a href="#summary" id="toc-summary">Summary</a>
- <a href="#motivation" id="toc-motivation">Motivation</a>
  - <a href="#s3-case-study" id="toc-s3-case-study"><code>S3</code> Case
    Study</a>
    - <a href="#open-generics-with-s3" id="toc-open-generics-with-s3">Open
      generics with <code>S3</code></a>
    - <a href="#challenges" id="toc-challenges">Challenges</a>
  - <a href="#s4-case-study" id="toc-s4-case-study"><code>S4</code> Case
    Study</a>
    - <a href="#open-generics-with-s4" id="toc-open-generics-with-s4">Open
      Generics with <code>S4</code></a>
  - <a href="#julia-case-study" id="toc-julia-case-study"><code>julia</code>
    Case Study</a>
    - <a href="#generics-packages" id="toc-generics-packages">Generics
      Packages</a>
    - <a href="#weak-dependencies-glue-code"
      id="toc-weak-dependencies-glue-code">Weak Dependencies &amp; Glue
      Code</a>
- <a href="#open-registration" id="toc-open-registration">Open
  Registration</a>
  - <a href="#considerations" id="toc-considerations">Considerations</a>
    - <a href="#registration-behavior"
      id="toc-registration-behavior">Registration Behavior</a>
    - <a href="#signature-ambiguity" id="toc-signature-ambiguity">Signature
      Ambiguity</a>
    - <a href="#compatibility-constraints"
      id="toc-compatibility-constraints">Compatibility Constraints</a>
    - <a href="#namespaced-calls" id="toc-namespaced-calls">Namespaced
      calls</a>
    - <a href="#internal-calls" id="toc-internal-calls">Internal Calls</a>
    - <a href="#communication" id="toc-communication">Communication</a>
    - <a href="#documentation" id="toc-documentation">Documentation</a>

# Preamble

This proposal follows the discussion in
[\#240](https://github.com/RConsortium/OOP-WG/issues/240), a
feature-incomplete
[proof-of-concept](https://github.com/dgkf/OOP-WG/tree/mutual_generics),
and discussion at the Nov 14th Meeting.

# Summary

When multiple packages provide generics of the same name, the
coordination of their methods can cause a maintenance overhead as
compatibility between generics must be opted into. This is true of both
the S3 and S4 dispatch systems.

This proposal explores a “open” generic registration alternative which
allows a more permissive mechanism of method registration. While “open”
refers to one permissive mechanism for providing a generic, this
proposal also explores ways to articulate contracts of interactions
between packages’ generics, *proactively* consenting to mechanisms of
extension.

# Motivation

The motivating example for this exploration is to allow packages to
provide functional stand-alone generics, while also registering their
methods alongside generics which share the same name from other
packages.

## `S3` Case Study

### Open generics with `S3`

To provide a generic which functions in isolation, the generic and its
method must both be exported by a package.

<div class="code-with-filename">

**pkgA/R/fn.R**

``` R
fn <- function(x, ...) UseMethod()
fn.character <- function(x, ...) "fn(<character>)"
```

</div>

<div class="code-with-filename">

**pkgA/NAMESPACE**

``` R
export(fn)
export(fn.character)
```

</div>

If another package provides a generic of the same name, `pkgB::fn`, it
will mask `pkgA::fn`. To continue to work after loading `pkgB`, `pkgA`
must export its methods as well for explicit use with `pkgB::fn`.

<div class="code-with-filename">

**pkgA/NAMESPACE**

``` R
export(fn)
export(fn.character)
S3method(pkgB::fn,character)
```

</div>

This isn’t quite “open” because compatibility with each package has to
be opted into for each compatible interface.

### Challenges

This point of friction seems to have resulted in communities adopting
one of three different mechanisms. These methods are considered to be
either *defensive* (preempting or working-around conflict) or
*retroactive* (requiring changes to code after conflicts are observed)
resolutions.

#### Package- and/or Type-specific Naming

One solution is to avoid name conflicts by adding a prefix (`pkg_fn()`)
or embed some type hints into the function name `fnCharacter()`.

#### Combinatorial Registration

Continuing to register methods against all packages which export the
same generics begins to run into a combinatorial problem of explicitly
whitelisting interactions.

```mermaid
graph LR

A["
<strong>pkgA</strong>
<code>
export(fn)
export(fn.character)
S3method(pkgB::fn,character)
S3method(pkgC::fn,character) </code>
"]

B["
<strong>pkgB</strong>
<code>
export(fn)
export(fn.numeric)
S3method(pkgA::fn,numeric)
S3method(pkgC::fn,numeric) </code>
"]

C["
<strong>pkgC</strong>
<code>
export(fn)
export(fn.list)
S3method(pkgA::fn,list)
S3method(pkgB::fn,list) </code>
"]

  A --- B
  A --- C
  B --- C
```

In practice, we don’t often see this level of package interaction.
However, it’s difficult to project whether that is due to lack of
interest or whether it’s discouraged due to the the complexity it
currently requires.

#### `generics`-style Packages

The best solution to this problem appears to be the concerted
endorsement of a `generics`-style package – a package whose sole purpose
is to export generic functions with minimal claim over the behavior of
methods.

This mechanism effectively opens up the generics for communal adoption,
and forfeits some of the control over the evolution of a generic’s API.

## `S4` Case Study

### Open Generics with `S4`

Using `S4` we have to get a bit craftier. Any S4 generic will mask
existing generics from other packages. To provide methods that work
alone or in unison with other packages we have to engineer our own
method registration scheme.

> Full disclosure, this is the best I could come up with in a couple
> hours of tinkering. I’m sure if there was a real interest in extending
> S4 in this way a more thoughtful solution could be found.

<div class="code-with-filename">

**pkgA/R/fn.R**

``` R
fn <- function(x, ...) "pkgA::fn(<default>)"
fn_numeric <- function(x, ...) "pkgA::fn(<numeric>)"

.onLoad <- function(libname, pkgname) {
  if (is.null(getGeneric("fn"))) {
    makeStandardGeneric("fn", fn)
    namespaceExport(getNamespace(packageName()), "fn")
  }

  setMethod("fn", signature = "numeric", fn_numeric)
}
```

</div>

<div class="code-with-filename">

**pkgB/R/fn.R**

``` R
fn <- function(x, ...) standardGeneric()
fn_integer <- function(x, ...) "pkgA::fn(<integer>)"

.onLoad <- function(libname, pkgname) {
  if (is.null(getGeneric("fn"))) {
    makeStandardGeneric("fn", fn)
    namespaceExport(getNamespace(packageName()), "fn")
  }

  setMethod("fn", signature = "integer", fn_integer)
}
```

</div>

#### Challenges

This approach works well when all packages handle their method
registration in this way, but methods will still be masked by any new
generic.

## `julia` Case Study

`julia` provides an interesting parallel world that prides itself in the
emergent collaboration that stems from the language’s multiple dispatch
system.

> *I really like Stefan Karpinski’s talk from JuliaCon 2019 on this
> topic*  
> [“The Unreasonable Effectiveness of Multiple
> Dispatch”](https://www.youtube.com/watch?v=kc9HwsxE1OY&t=1173s)
>
> Which is a retrospective on the practical impact of `julia`’s multiple
> dispatch in addressing [*the expression
> problem*](https://en.wikipedia.org/wiki/Expression_problem),
> summarized succinctly as the ability to
>
> 1.  define **new types** to which **existing operations** apply
> 2.  define **new operations** which apply to **existing types**

Open registration of generics is similarly about making it easier to
provide implementations of existing operations for new types. Where the
expression problem applies to a *language as a whole* and assumes you
*know* about the existing operations, this proposal attempts to make
extension more expressive in an *ecosystem* with *incomplete
information* by removing the burden of knowledge from developers and
users.

### Generics Packages

Just like `R`, `julia` has required adminstrative overhead to coalesce
around sets of types and generics that should be served to the community
in a lightweight form factor so that domain-specific components can be
easily reused. The amount of organization behind these packages is a
testament to the `julia` community’s mindfulness around sharing code,
but the necessity for them may still be considered as indication that
languages haven’t quite cracked good extensible interfaces across
ecosystems.

Below is a small collection of examples in familiar fields

`JuliaData`

- [`DataAPI.jl`](https://github.com/JuliaData/DataAPI.jl)

  > This package provides a namespace for data-related generic function
  > definitions …

- [`Tables.jl`](https://github.com/JuliaData/Tables.jl)

  > The Tables.jl package provides simple, yet powerful interface
  > functions …

- [`TableOperations.jl`](https://github.com/JuliaData/TableOperations.jl)

  > common table operations (`select`, `filter`, `transform`, `map`,
  > etc.)

`JuliaStats`

- [`StatsAPI.jl`](https://github.com/JuliaStats/StatsAPI.jl)

  > This package provides a namespace for statistics-related generic
  > function definitions to solve the optional dependency problem

`JuliaAI`

- [`ScientificTypesBase.jl`](https://github.com/JuliaAI/ScientificTypesBase.jl)

  > A light-weight, dependency-free, Julia interface defining a
  > collection of types (without instances) for implementing conventions
  > about the scientific interpretation of data.

`BioJulia`

- [`BioCore.jl`](https://github.com/BioJulia/BioCore.jl)

  > core definitions that are common to all or many packages in the
  > BioJulia ecosystem.

- [`BioGenerics.jl`](https://github.com/BioJulia/BioGenerics.jl)

  > BioGenerics provides generic methods and modules used in many of the
  > other BioJulia packages.

### Weak Dependencies & Glue Code

Although not exactly the same problem, the `julia` community is still
exploring solutions to the problem of extending packages without hard
dependencies. This could be seen as a downstream repercussion of
strictly defining ownership of generics to modules.

This is related in that the goal is to minimize the overhead of taking
dependencies on the package that owns a given generic or type, but
different in that there is no mechanism proposed for two packages
contributing to the same generic without clear ownership.

- [“Weak dependencies”](https://github.com/JuliaLang/julia/pull/47040)
  evaluate code only if dependencies are available at compile-time.

- [“Glue code”](https://github.com/JuliaLang/julia/issues/43119) (akin
  to “Conditional Dependencies”) evaluates code if dependencies are
  available on-load.

# Open Registration

To address the challenges of creating compatible packages which extend
each other’s features with incomplete knowledge of the package
landscape, this proposal introduces the concept of “open” method
registration. Where “closed” generics are concrete entities, whose
compatibility must be unidirectionally opted into, “open” generics are
an abstract interface for extension.

In its most permissive form, open generics could try to reconcile
differences between multiple packages’ generics to attempt to
differentiate intended methods based on criteria such as the dispatched
argument names, order and types. If a more controlled ecosystem is
preferred, generics provided by packages might come with compatibility
requirements to prevent over-eager reconciliation of methods with
unintended name conflict. In any case, the goal is that this
compatibility can be negotiated between packages without the need for
dependencies, without coordination among package authors, and in more
permissive cases, without knowledge of other packages which contribute
the same generic.

Exactly how this open forum for method registration might operate
prompts a number of design choices. Below are a number of key behavioral
considerations.

## Considerations

### Registration Behavior

Provided two packages, which each provide a generic and method for
`foo`, both packages may be loaded either independently or in either
order to still provide a functional `foo` generic.

> This example is considered simple because there is no ambiguity
> between the type signatures. (*this simple case is implemented in
> [`@dgkf`’s fork](https://github.com/dgkf/OOP-WG)*)

<div class="code-with-filename">

**pkgA/R/foo.R**

``` R
#' @export
foo <- R7::new_generic("foo", "x")
R7::method(foo, R7::class_numeric) <- function(x) "pkgA::foo(<numeric>)"

.onLoad <- function(libpath, pkgname) R7::methods_register(libpath, pkgname)
```

</div>

<div class="code-with-filename">

**pkgB/R/foo.R**

``` R
#' @export
foo <- R7::new_generic("foo", "x")
R7::method(foo, R7::class_double) <- function(x) "pkgB::foo(<double>)"

.onLoad <- function(libpath, pkgname) R7::methods_register(libpath, pkgname)
```

</div>

The `R7` registration creates or appends to the methods table of a
generic `foo` automatically.

<div class="code-with-filename">

**loading pkgA alone**

``` R
library(pkgA)
foo
# <R7_generic> foo(x, ...) with 2 methods:
# 1: method(foo, class_integer) [pkgA]
# 2: method(foo, class_double) [pkgA]

foo(1)
# [1] "pkgA::foo(<numeric>)"
```

</div>

<div class="code-with-filename">

**loading pkgB alone**

``` R
library(pkgB)
foo
# <R7_generic> foo(x, ...) with 1 methods:
# 2: method(foo, class_double) [pkgB]

foo(1)
# [1] "pkgB::foo(<double>)"
```

</div>

When both packages are loaded, new methods are added to the methods
table, giving precedence to packages loaded later. The precedence of
methods is meant to mirror the masking mechanics of symbols when loading
packages, such that methods of the same signature will mask existing
methods on load.

Just like when symbols are masked, package startup messages can be
provided to indicate whether methods are being masked by newly loaded
packages.

<div class="code-with-filename">

**loading pkgA, then pkgB**

``` R
library(pkgA)
library(pkgB)
# method foo(<double>) from 'pkgA' masked by method in 'pkgB'

foo
# <R7_generic> foo(x, ...) with 3 methods:
# 1: method(foo, class_integer) [pkgA]
# 2: method(foo, class_double) [pkgB]
# 3: method(foo, class_double) [pkgA]

foo(1)
# [1] "pkgB::foo(<double>)"
```

</div>

<div class="code-with-filename">

**loading pkgB, then pkgA**

``` R
library(pkgB)
library(pkgA)
# method foo(<double>) from 'pkgB' masked by method in 'pkgA'

foo
# <R7_generic> foo(x, ...) with 3 methods:
# 1: method(foo, class_integer) [pkgA]
# 2: method(foo, class_double) [pkgA]
# 3: method(foo, class_double) [pkgB]

foo(1)
# [1] "pkgA::foo(<double>)"
```

</div>

From here, a number of design decisions would be necessary to handle
more complex cases.

### Signature Ambiguity

Beyond the simplest case scenario, we must begin to consider when
generics should be permitted to share a symbol and what to do with them
if we decide they should not be shared.

Signature ambiguity can come in a number of different forms.

1.  Supersets

    <div class="code-with-filename">

    **pkgA**
    ``` R
    bar <- R7::new_generic("bar", "x")
    ```

    </div>

    <div class="code-with-filename">

    **pkgB**
    ``` R
    bar <- R7::new_generic("bar", c("x", "y"))
    ```

    </div>

    > There is precedence for this style of method mismatch resolution
    > in `S4`’s `conformMethods` behavior.

2.  Rearrangements

    <div class="code-with-filename">

    **pkgA**
    ``` R
    bar <- R7::new_generic("bar", c("x", "y"))
    ```

    </div>

    <div class="code-with-filename">

    **pkgB**
    ``` R
    bar <- R7::new_generic("bar", c("y", "x"))
    ```

    </div>

3.  Named argument mismatch

    <div class="code-with-filename">

    **pkgA**
    ``` R
    bar <- R7::new_generic("bar", "a")
    ```

    </div>

    <div class="code-with-filename">

    **pkgB**
    ``` R
    bar <- R7::new_generic("bar", "b")
    ```

    </div>

This is further complicated by R’s flexible function call syntax, which
allows positional and named arguments to be mixed, relying on argument
matching to resolve a call.

Below are a few proposals to address this ambiguity.

#### Option 1: Disallow conflicts, Mask on any Ambiguity

The easiest resolution is to consider any break in signature as a sign
that generics are no longer compatible. The previous generic is masked
by the newly loaded generic.

##### Support for evolving dispatch args

If one package were to add a new dispatch argument, or alter a dispatch
arg name, packages which had previously aimed to be compatible would
immediately be incompatible and would be masked instead of merged.

##### Impact on current R7 design

As all generics’ method tables are stored in the R7 namespace, handling
for generic masking would need to be added. When packages are unloaded,
they should restore the dispatch state that existed before they were
loaded.

#### Option 2: Allow supersets, Mask on Conflict

To try to permit extension of type signatures without sweeping breaking
changes, methods of generics whose dispatch args are part of the same
set could still be merged.

Dispatch args are considered to be from the same *“set”* if the shared
args are always identical. That is to say we could have three functions
such as

``` r
bar <- R7::new_generic("bar",   "a")
bar <- R7::new_generic("bar", c("a", "b"))
bar <- R7::new_generic("bar", c("a", "b", "c", "easyas", "one", "two", "three"))
```

and they would not be in conflict, as the “types” of any undeclared
arguments could be interpretted as `ANY`-types which are only
specialized by the other methods.

##### Support for evolving dispatch args

This mechanism would allow package authors to add dispatch arguments
without fear of breaking package interactions. Renaming dispatch
arguments might result in conflicts, causing packages to mask one
another.

##### Impact on current R7 design

Like above, masking would still require changes. However, no changes to
dispatch mechanism needed.

#### Option 3: Allow rearrangement & mismatch

> Before I lose you, just be aware that I’m trying to be exhaustive,
> even if it means considering some wild deviations from traditional R.
> This mechanism is likely undesirable because it comes with the
> cognitive and refactoring burden of shifting call syntax as conflicts
> are introduced.

In its most permissive form, registration could accept entirely
different `dispatch_args` that were either renamed or rearranged. In
this scenario, the named arguments become ambiguous. There are a couple
ways that this ambiguity might be managed:

- If parameter names are ambiguous, those parameters may only be
  provided as positional arguments to allow for dispatch. Positional
  arguments are used for dispatch first before named arguments are
  matched against remainder.
- If parameter names are ambiguous, all named parameters are disallowed

##### Support for evolving dispatch args

Even if dispatch arguments are not used after there is a conflict, they
would still impact the unambiguous cases.

##### Impact on current R7 design

Trying to reconcile dispatch arguments or dispatch on only positional
arguments would require a rework of call argument matching to deduce
dispatch arguments.

### Compatibility Constraints

Although this proposal is framed as “open” registration, that doesn’t
preclude package authors from setting some boundaries for how open they
want to be.

When providing a generic function, a package author may want to request
a more explicit relationship with packages that might extend it. We will
explore this through the perspective of one package providing a generic,
but keep in mind that neither package is priveleged. Both packages might
come with “terms and conditions” that need to be agreed to by other
parties.

Upon attaching a new generic into a scope or namespace, both sides of
this contract are applied.

- First, the newly attached generic’s requirements are enforced. If a
  current generic does not adhere to these requirements its methods are
  de-listed.
- Second, if any current generic’s requirements fail to be satisfied
  with the introduction of a new generic, its methods are de-listed.

If a package is detached, the methods that might have been de-listed
should be restored by recalculating the terms of registration in the
appropriate search order allowing attaching and detaching to be
reversible operations.

> In the following examples, the arguments and values used to
> communicate this contract are placeholders. The example API
> prioritizes communication over a well-typed, unambiguous interface.

Packages may request

#### Explicit Compatibility

If we wish to treat our generic like a specific contract for behavior,
where we want more assurance that other package’s methods adhere to this
contract beyond just using a similarly named function, we may wish to
have other packages explicitly state their compatibility by listing our
package as a compatible generic.

<div class="code-with-filename">

**pkgA/R/foo.R**

``` R
foo <- R7::new_generic("foo", "a",
  require_compatibility = "explicit"
)
```

</div>

<div class="code-with-filename">

**pkgB/R/foo.R**

``` R
foo <- R7::new_generic("foo", "a", 
  declare_compatibility = "pkgA"
)
```

</div>

**Attaching `pkgA`, then `pkgB` will…**

1.  Check `pkgB`’s compatibility requirements (there are none)
2.  Check whether existing generics’ compatibility requirements are
    satisfied (they are)
3.  Conclude that both package generics can live happily together

**Attaching `pkgB`, then `pkgA` will…**

1.  Check `pkgA`’s compatibility requirements (it is satisfied)
2.  Check whether existing generics’ compatibility requirements are
    satisified (there are none)
3.  Conclude that both package generics can live happily together

If we introduce a new packge which does not conform to this contract

<div class="code-with-filename">

**pkgC/R/foo.R**

``` R
foo <- R7::new_generic("foo", "a")
```

</div>

**Attaching `pkgC`, then `pkgB`, then `pkgA` will…**

1.  Check `pkgA`’s compatibility requirements (it is not satisfied).
2.  Iterate through existing generics to de-list the methods of packages
    whose generics do not conform to `pkgA`’s compatibility requirements
3.  Check whether existing generics’ compatibility requirements are
    satisfied (there are none)
4.  Conclude that we must de-list the generic (and methods) from `pkgC`

**Attaching `pkgA`, then `pkgB`, then `pkgC` will…**

1.  Check `pkgC`’s compatibility requirements (there are none).
2.  Check whether existing generic’s compatibility requirements are
    satisfied (`pkgA`’s requirements are not)
3.  Conclude that `pkgA`’s generic (and methods) must be de-listed

Finally, if we introduce another package which imposes a different
contract

<div class="code-with-filename">

**pkgD/R/foo.R**

``` R
foo <- R7::new_generic("foo", "a",
  require_compatibility = "explicit"
  declare_compatibility = "pkgA"
)
```

</div>

**Attaching `pkgA`, then `pkgD` will…**

1.  Check `pkgD`’s compatibility requirements (it is not satisfied).
2.  Iterating through existing generics, we will find that `pkgA` does
    not declare compatibility with `pkgD` and therefore does not conform
    to `pkgA`’s compatibility requirements.
3.  Conclude that `pkgA`’s generic (and methods) must be de-listed.

#### Signature Compatibility

If a design is adopted that permits registration of generics with
varying dispatch args, a package may want to impose constraints based on
the similarity of the dispatch args.

<div class="code-with-filename">

**inkjet/R/print.R**

``` R
#' Send a document to a printer
print <- R7::new_generic("print", "document", 
  require_compatibility = "dispatch_names"
)
```

</div>

Identical dispatch args, especially if the naming of the arguments is
some indication of the intended scope of a generic, can be a heuristic
for the intentionality of generic compatibility.

##### Examples of varying dispatch arguments

<div class="code-with-filename">

**pkgA/R/foo.R**

``` R
foo <- R7::new_generic("foo", "a", 
  require_compatibility = "dispatch_names"
)
```

</div>

<div class="code-with-filename">

**pkgB/R/foo.R**

``` R
foo <- R7::new_generic("foo", "a")
```

</div>

<div class="code-with-filename">

**pkgC/R/foo.R**

``` R
foo <- R7::new_generic("foo", c("a", "b"))
```

</div>

<div class="code-with-filename">

**pkgD/R/foo.R**

``` R
foo <- R7::new_generic("foo", "c")
```

</div>

In this example, `pkgB` conforms to `pkgA`’s requirements.

Likewise, `pkgC` could be considered to conform to `pkgA`’s dispatch
signature or not, depending on which mechanism of signature amibuity is
used. The varying degrees of signature compatibility themselves could be
separate requirement types.

### Namespaced calls

When referring to a generic within a package namespace (`pkgA::foo()`),
how might we want it to behave?

#### Option 1: Identical to global calls

The simplest solution, calling a namespaced generic is treated exactly
the same as a global call.

This might be unintuitive as the intended behaviors of the package might
be assumed to be used when calling explicitly from the package
namespace.

#### Option 2: Restrict to only use package methods

Perhaps most conservatively, one approach might be to restrict dispatch
to only the methods that are provided by the provided package.

#### Option 3: Reapply constraints

Even if a package’s methods have been de-listed in the global methods
tables due to compatibility issues with other attached packages, perhaps
a namespaced call should re-evaulate the methods table such that the
package of interest’s constraints are applied last, regardless of search
order, ensuring that any packages which conform to its required contract
are picked up.

<div class="code-with-filename">

**dplyr/R/mutate.R**

``` R
#' Create, modify, and delete columns
mutate <- R7::new_generic("mutate", ".data", 
  require_compatibility = "dispatch_names"
)
```

</div>

<div class="code-with-filename">

**genetic.algorithms/R/mutate.R**

``` R
#' Create, modify, and delete columns
mutate <- R7::new_generic("mutate", "algorithm")
```

</div>

Provided the two packages above, loading `dplyr` then
`genetic.algorithms` would de-list `dplyr`’s methods in the global
namespace as its compatibility requirements are not met by
`genetic.algorithms`. However, we might still expect that a `dplyr`-like
`mutate` could be called using `dplyr::mutate()`.

In this situation, `dplyr` is treated as though it has been attached
most recently and its own `mutate` methods take precedence.

This would prevent packages from masking methods, while allowing for
dispatch on specialized subclasses.

#### Option 4: Reapply constraints at earliest point in search path

If it is preferred that packages can mask identical dispatch signatures
even when generics are called using the namespaced syntax, then it might
be preferred for the package to try to maintain as much of the package
search order as possible while still ensuring that its compatibility
requirements are met. This is more similar to the generalized global
call, while ensuring that the package’s extension constraints are
applied.

In this scenario, the package’s generic is at the bottom of the search
order, and any additional generics provided by packages attached latter
in the search order are layered on top if they satisfy its compatibility
requirements, masking its methods.

### Internal Calls

Furthermore, calls to generics provided by a package from within the
same package namespace may want to ensure that their generic contract is
met. What might this behavior look like?

#### Option 1: Identical to global calls

The simplest solution, calling an internal generic is treated exactly
the same as a global call.

This might be unintuitive as the intended behaviors of the package might
be assumed to be used when calling explicitly from the package
namespace.

#### Option 2: Restrict to only use package methods

Most conservatively, calls within the package that creates a generic
could use only methods that they themselves provide.

This would lose much of the value of having generics that other packages
can extend. Generics could not be used to implement a tacit trait which
can be leveraged by other packages.

#### Option 3: Reapply constraints

Reapply generic constraints, as though the parent package’s constraints
were applied last.

This would allow packages to register generics which conform to the
package’s requirements, while also allowing the methods from the generic
in the calling scope to potentially be masked. This means that generics
used outside a namespace and within a namespace may result in different
behaviors.

#### Option 4: Reapply constraints at earliest point in search path

As above when called with a namespaced function call, a generic called
from within its originating package could also follow a behavior where
its methods may be masked by attached packages, while ensuring its
compatibility constraints are enforced.

### Communication

As the composition of generics might make it difficult to track intended
behaviors, it is critical that end-users of these generics are provided
with informative indications when packages might alter the behavior of
existing code beyond the addition of new methods.

#### Method Masking

Any time a method is masked by another method, this could be
communicated using a package startup message similar to function
masking.

<div class="code-with-filename">

**pkgA/R/foo.R**

``` R
#' @export
foo <- R7::new_generic("foo", "x")
R7::method(foo, R7::class_numeric) <- function(x) "pkgA::foo(<numeric>)"
```

</div>

<div class="code-with-filename">

**pkgB/R/foo.R**

``` R
#' @export
foo <- R7::new_generic("foo", "x")
R7::method(foo, R7::class_double) <- function(x) "pkgB::foo(<double>)"
```

</div>

<div class="code-with-filename">

**loading pkgA, then pkgB**

``` R
library(pkgA)
library(pkgB)
# method foo(<double>) from 'pkgA' masked by method in 'pkgB'
```

</div>

When printing generics, masked methods can still be displayed, but will
be further down the dispatch list. The originating package namespace
should be indicated so that corrective actions, should a user want to
keep a method from a particular package, are more intuitive.

``` R
foo
# <R7_generic> foo(x, ...) with 3 methods:
# 1: method(foo, class_integer) [pkgA]
# 2: method(foo, class_double) [pkgB]
# 3: method(foo, class_double) [pkgA]
```

A helper function could be provided to help explain method dispatch.
Such a function could have a number of interfaces, but we’ll consider a
scenario where the index in the method table is used to explain it.

``` R
explain(foo, 3)
# method(foo, class_double) [pkgA]
#   - masked by method with the same signature from `pkgB`
```

#### Generic De-listing

When compatibility constraints are applied, it’s possible that one or
more packages’ methods will be de-listed due to incompatible
constraints.

When a generic’s required capability contract is not upheld by a newly
attached package, its methods are de-listed. This is effectively the
same as masking, but is distinct in that it is due to a conflict in
compatibility, not dispatch.

<div class="code-with-filename">

**pkgA/R/foo.R**

``` R
#' @export
foo <- R7::new_generic("foo", "x", require_compatibility = "explicit")
R7::method(foo, R7::class_numeric) <- function(x) "pkgA::foo(<numeric>)"
R7::method(foo, R7::class_character) <- function(x) "pkgA::foo(<character>)"
```

</div>

<div class="code-with-filename">

**pkgB/R/foo.R**

``` R
#' @export
foo <- R7::new_generic("foo", "x")
R7::method(foo, R7::class_double) <- function(x) "pkgB::foo(<double>)"
```

</div>

<div class="code-with-filename">

**loading pkgA, then pkgB**

``` R
library(pkgA)
library(pkgB)
# generic `pkgA::foo` masked by `pkgB::foo`: 
#   `pkgB::foo` does not list explicit compatibility, required by 
#   `pkgA::foo`
```

</div>

When printing generics, de-listed methods should be indicated

``` R
foo
# <R7_generic> foo(x, ...) with 2 methods:
# 1: method(foo, class_double) [pkgB]
# 2: method(foo, class_integer) [pkgA*]
# 3: method(foo, class_double) [pkgA*]
# 4: method(foo, class_character) [pkgA*]
#    *compatibility constraints not satisfied, methods unavailable
```

Like masking, de-listed methods should also be introspectable with
helpful indicators about the state of the method dispatch.

``` R
explain(foo, 3)
# method(foo, class_double) [pkgA*]
#  - unused during dispatch because `pkgB::foo` does not list 
#    explicit compatibility required by `pkgA::foo`
#  - masked by method with same dispatch signature from `pkgB`
```

### Documentation

In addition to introspect generics, explicit `help` documentation should
likewise provide clarity about the interfaces exposed through a unified
generic. Especially in situations where packages provide their own
documentation, it is important to provide documentation in a way that
exposes all of this information.

When generics do not have a distinct origin, each package might provide
its own generic documentation. Just like any other name that has
documentation provided by mulitple identical aliases, a menu can be
provided to select which documentation to view. This is the simplest
solution, and is a natural extension of existing R workflows.