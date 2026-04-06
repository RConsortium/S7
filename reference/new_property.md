# Define a new property

A property defines a named component of an object. Properties are
typically used to store (meta) data about an object, and are often
limited to a data of a specific `class`.

By specifying a `getter` and/or `setter`, you can make the property
"dynamic" so that it's computed when accessed or has some non-standard
behaviour when modified. Dynamic properties are not included as an
argument to the default class constructor.

See the "Properties: Common Patterns" section in
`vignette("class-objects")` for more examples.

## Usage

``` r
new_property(
  class = class_any,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  name = NULL
)
```

## Arguments

- class:

  Class that the property must be an instance of. See
  [`as_class()`](https://rconsortium.github.io/S7/reference/as_class.md)
  for details.

- getter:

  An optional function used to get the value. The function should take
  `self` as its sole argument and return the value. If you supply a
  `getter`, you are responsible for ensuring that it returns an object
  of the correct `class`; it will not be validated automatically.

  If a property has a getter but doesn't have a setter, it is read only.

- setter:

  An optional function used to set the value. The function should take
  `self` and `value` and return a modified object.

- validator:

  A function taking a single argument, `value`, the value to validate.

  The job of a validator is to determine whether the property value is
  valid. It should return `NULL` if the object is valid, or if it's not
  valid, a single string describing the problem. The message should not
  include the name of the property as this will be automatically
  appended to the beginning of the message.

  The validator will be called after the `class` has been verified, so
  your code can assume that `value` has known type.

- default:

  When an object is created and the property is not supplied, what
  should it default to? If `NULL`, it defaults to the "empty" instance
  of `class`. This can also be a quoted call, which then becomes a
  standard function promise in the default constructor, evaluated at the
  time the object is constructed.

- name:

  Property name, primarily used for error messages. Generally don't need
  to set this here, as it's more convenient to supply as the element
  name when defining a list of properties. If both `name` and a
  list-name are supplied, the list-name will be used.

## Value

An S7 property, i.e. a list with class `S7_property`.

## Examples

``` r
# Simple properties store data inside an object
Pizza <- new_class("Pizza", properties = list(
  slices = new_property(class_numeric, default = 10)
))
my_pizza <- Pizza(slices = 6)
my_pizza@slices
#> [1] 6
my_pizza@slices <- 5
my_pizza@slices
#> [1] 5

your_pizza <- Pizza()
your_pizza@slices
#> [1] 10

# Dynamic properties can compute on demand
Clock <- new_class("Clock", properties = list(
  now = new_property(getter = function(self) Sys.time())
))
my_clock <- Clock()
my_clock@now; Sys.sleep(1)
#> [1] "2026-04-06 22:29:49 UTC"
my_clock@now
#> [1] "2026-04-06 22:29:50 UTC"
# This property is read only, because there is a 'getter' but not a 'setter'
try(my_clock@now <- 10)
#> Error : Can't set read-only property <Clock>@now

# Because the property is dynamic, it is not included as an
# argument to the default constructor
try(Clock(now = 10))
#> Error in Clock(now = 10) : unused argument (now = 10)
args(Clock)
#> function () 
#> NULL
```
