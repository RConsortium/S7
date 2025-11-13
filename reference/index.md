# Package index

## Key functions

- [`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md)
  [`new_object()`](https://rconsortium.github.io/S7/reference/new_class.md)
  : Define a new S7 class
- [`new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.md)
  [`S7_dispatch()`](https://rconsortium.github.io/S7/reference/new_generic.md)
  : Define a new generic
- [`new_union()`](https://rconsortium.github.io/S7/reference/new_union.md)
  : Define a class union
- [`` `method<-`() ``](https://rconsortium.github.io/S7/reference/method-set.md)
  : Register an S7 method for a generic
- [`S7_inherits()`](https://rconsortium.github.io/S7/reference/S7_inherits.md)
  [`check_is_S7()`](https://rconsortium.github.io/S7/reference/S7_inherits.md)
  : Does this object inherit from an S7 class?
- [`validate()`](https://rconsortium.github.io/S7/reference/validate.md)
  [`valid_eventually()`](https://rconsortium.github.io/S7/reference/validate.md)
  [`valid_implicitly()`](https://rconsortium.github.io/S7/reference/validate.md)
  : Validate an S7 object

## Properties

- [`new_property()`](https://rconsortium.github.io/S7/reference/new_property.md)
  : Define a new property
- [`prop()`](https://rconsortium.github.io/S7/reference/prop.md)
  [`` `prop<-`() ``](https://rconsortium.github.io/S7/reference/prop.md)
  [`` `@`() ``](https://rconsortium.github.io/S7/reference/prop.md) :
  Get/set a property
- [`prop_names()`](https://rconsortium.github.io/S7/reference/prop_names.md)
  [`prop_exists()`](https://rconsortium.github.io/S7/reference/prop_names.md)
  : Property introspection
- [`props()`](https://rconsortium.github.io/S7/reference/props.md)
  [`` `props<-`() ``](https://rconsortium.github.io/S7/reference/props.md)
  [`set_props()`](https://rconsortium.github.io/S7/reference/props.md) :
  Get/set multiple properties
- [`S7_data()`](https://rconsortium.github.io/S7/reference/S7_data.md)
  [`` `S7_data<-`() ``](https://rconsortium.github.io/S7/reference/S7_data.md)
  : Get/set underlying "base" data

## Method dispatch

- [`convert()`](https://rconsortium.github.io/S7/reference/convert.md) :
  Convert an object from one type to another
- [`class_missing`](https://rconsortium.github.io/S7/reference/class_missing.md)
  : Dispatch on a missing argument
- [`class_any`](https://rconsortium.github.io/S7/reference/class_any.md)
  : Dispatch on any class
- [`method()`](https://rconsortium.github.io/S7/reference/method.md) :
  Find a method for an S7 generic
- [`method_explain()`](https://rconsortium.github.io/S7/reference/method_explain.md)
  : Explain method dispatch
- [`super()`](https://rconsortium.github.io/S7/reference/super.md) :
  Force method dispatch to use a superclass
- [`S7_class()`](https://rconsortium.github.io/S7/reference/S7_class.md)
  : Retrieve the S7 class of an object

## Packages

Functions needed when using S7 within a package. See
[`vignette("packages")`](https://rconsortium.github.io/S7/articles/packages.md)
for more details.

- [`methods_register()`](https://rconsortium.github.io/S7/reference/methods_register.md)
  : Register methods in a package
- [`new_external_generic()`](https://rconsortium.github.io/S7/reference/new_external_generic.md)
  : Generics in other packages

## Compatibility

These tools provide a layer of compatibility between S7 and S3 classes,
S4 classes, and base types. See
[`vignette("compatibility")`](https://rconsortium.github.io/S7/articles/compatibility.md)
for more details.

- [`class_logical`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_integer`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_double`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_complex`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_character`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_raw`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_list`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_expression`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_name`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_call`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_function`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_environment`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_numeric`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_atomic`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_vector`](https://rconsortium.github.io/S7/reference/base_classes.md)
  [`class_language`](https://rconsortium.github.io/S7/reference/base_classes.md)
  : S7 wrappers for base types
- [`class_factor`](https://rconsortium.github.io/S7/reference/base_s3_classes.md)
  [`class_Date`](https://rconsortium.github.io/S7/reference/base_s3_classes.md)
  [`class_POSIXct`](https://rconsortium.github.io/S7/reference/base_s3_classes.md)
  [`class_POSIXlt`](https://rconsortium.github.io/S7/reference/base_s3_classes.md)
  [`class_POSIXt`](https://rconsortium.github.io/S7/reference/base_s3_classes.md)
  [`class_data.frame`](https://rconsortium.github.io/S7/reference/base_s3_classes.md)
  [`class_formula`](https://rconsortium.github.io/S7/reference/base_s3_classes.md)
  : S7 wrappers for key S3 classes
- [`new_S3_class()`](https://rconsortium.github.io/S7/reference/new_S3_class.md)
  : Declare an S3 class
- [`S4_register()`](https://rconsortium.github.io/S7/reference/S4_register.md)
  : Register an S7 class with S4
