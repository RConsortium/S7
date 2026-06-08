---
name: property-dots
description: >
  Guide for writing functions that accept property name-value pairs via 
  `...`. Use when adding a function whose `...` collects property values.
---

# Accept properties via `...`

Use this skill when writing a new function that collects property name-value pairs
through `...`, e.g. ``new_object(`_parent`, ...)`` or ``set_props(`_object`, ...)``.

Two problems show up whenever `...` carries property values:

1. A fixed argument can shadow a property of the same name. If `set_props()`
   had a formal called `object`, then `set_props(x, object = 1)` would bind
   `object` to the formal instead of treating it as the property `object`.
2. Callers sometimes want to supply property values programmatically, as a
   single list, rather than as individual `name = value` pairs.

`collect_dots()` and the underscore naming convention solve these.

For example, take (a simple version of) `update_props()`

```r
update_props <- function(`_object`, ...) {
  props(`_object`) <- collect_dots(...)
  `_object`
}

# Both work
update_props(x, object = 1, width = 10)
update_props(x, list(object = 1, width = 10))
```

## The underscore convention

Property names starting with `_` are reserved for internal use. This lets you name a fixed argument with a leading `_` so it can never collide with a real property passed through `...`.

- ``new_object(`_parent`, ...)`` — `_parent` is the parent object.
- ``set_props(`_object`, ...)`` — `_object` is the object to modify.

`_parent` and `_object` are not syntactically valid names, so they **must be
backtick-quoted** everywhere they appear:

```r
set_props <- function(`_object`, ..., .check = TRUE) {
  props(`_object`, check = .check) <- collect_dots(...)
  `_object`
}
```

Because these arguments are always passed positionally (and generated
constructors pass the parent positionally), the unusual name never burdens
callers — it only stops the name from competing with `...`.

Apply the convention when:

- The function takes properties via `...`, AND
- It also needs a fixed argument that a user might plausibly use as a property
  name (`object`, `parent`, `value`, ...).


## Collecting the dots with `collect_dots()`

`collect_dots(...)` returns a named list of the `...` values. As a convenience,
if `...` is a single unnamed list, its elements are used instead, which makes
it easy to build the values programmatically. It errors if any value is
unnamed.

```r
collect_dots(x = 1, y = 2)      # list(x = 1, y = 2)
collect_dots(list(x = 1, y = 2)) # list(x = 1, y = 2) -- spliced
collect_dots(1)                  # error: All arguments to `...` must be named.
collect_dots(list(1))            # error: All elements of `..1` must be named.
```
Use `collect_dots()` instead of `list(...)`.

## Documenting it

In the `@param` for the underscore argument, explain the unusual name and point
to `new_property()`:

```r
#' @param _object The object to modify. It has an unusual name to avoid clashing
#'   with property names supplied in `...`; see [new_property()] for details.
```

In the `@param` for `...`, mention the single-list shortcut:

```r
#' @param ... Name-value pairs of properties to set. As a convenience, you can
#'   supply a single unnamed list instead of individual name-value pairs, which
#'   makes it easy to set properties programmatically.
```

roxygen handles `@param _object` and a backtick-quoted formal fine.
