# Property introspection

- `prop_names(x)` returns the names of the properties

- `prop_exists(x, "prop")` returns `TRUE` and if only `x` has property
  `prop`.

- `prop_info()` returns a data frame describing the properties of an S7
  object or class, with one row per property.

## Usage

``` r
prop_names(object)

prop_exists(object, name)

prop_info(object)
```

## Arguments

- object:

  Either an S7 object (an instance) or an S7 class.

- name:

  A string giving the property of interest.

## Value

- `prop_names()` returns a character vector

- `prop_exists()` returns a single `TRUE` or `FALSE`.

- `prop_info()` returns a data frame with one row per property and the
  following columns:

  - `name`: a character vector of property names.

  - `default`: a list column of property defaults.

  - `class`: a character description of the class.

  - `getter`, `setter`, `validator`: logical vectors indicating whether
    the property has a getter, setter, or validator.

## Examples

``` r
Horse <- new_class("Horse", properties = list(
  name = class_character,
  colour = class_character,
  height = new_property(class_numeric, default = 15),
  age = new_property(
    class_numeric,
    validator = function(value) if (value < 0) "must be positive"
  ),
  now = new_property(getter = function(self) Sys.time())
))

prop_names(Horse)
#> [1] "name"        "parent"      "package"     "properties"  "abstract"   
#> [6] "constructor" "validator"  
prop_exists(Horse, "col")
#> [1] FALSE
prop_exists(Horse, "colour")
#> [1] FALSE
prop_info(Horse)
#>     name default                 class getter setter validator
#> 1   name                   <character>  FALSE  FALSE     FALSE
#> 2 colour                   <character>  FALSE  FALSE     FALSE
#> 3 height      15 <integer> or <double>  FALSE  FALSE     FALSE
#> 4    age         <integer> or <double>  FALSE  FALSE      TRUE
#> 5    now                         <ANY>   TRUE  FALSE     FALSE

# All functions also work with objects, not just classes
lex <- Horse(colour = "bay", height = 15, name = "Lex", age = 3)
prop_names(lex)
#> [1] "name"   "colour" "height" "age"    "now"   
prop_exists(lex, "age")
#> [1] TRUE
```
