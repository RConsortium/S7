# S7 wrappers for base types

The following S7 classes represent base types allowing them to be used
within S7:

- `class_logical`

- `class_integer`

- `class_double`

- `class_complex`

- `class_character`

- `class_raw`

- `class_list`

- `class_expression`

- `class_name`

- `class_call`

- `class_function`

- `class_environment` (can only be used for properties)

We also include three union types to model numerics, atomics, and
vectors respectively:

- `class_numeric` is a union of `class_integer` and `class_double`.

- `class_atomic` is a union of `class_logical`, `class_numeric`,
  `class_complex`, `class_character`, and `class_raw`.

- `class_vector` is a union of `class_atomic`, `class_list`, and
  `class_expression`.

- `class_language` is a union of `class_name` and `class_call`.

## Usage

``` r
class_logical

class_integer

class_double

class_complex

class_character

class_raw

class_list

class_expression

class_name

class_call

class_function

class_environment

class_numeric

class_atomic

class_vector

class_language
```

## Value

S7 classes wrapping around common base types and S3 classes.

## Examples

``` r
class_integer
#> <S7_base_class>: <integer>
class_numeric
#> <S7_union>: <integer> or <double>
class_factor
#> <S7_S3_class>: S3<factor>
```
