---
name: property-dots
description: Guide for writing S7 functions that accept property name-value pairs via `...`. Use when adding or editing a function whose `...` collects property values (like `new_object()`, `set_props()`, or `convert()`), so that the fixed arguments don't clash with property names.
---

# Accept properties via `...`

Use this skill when writing a function that collects property name-value pairs
through `...`, e.g. ``new_object(`_parent`, ...)``, ``set_props(`_object`, ...)``,
or a downcasting `convert()` method.

Two problems show up whenever `...` carries property values:

1. A fixed argument can shadow a property of the same name. If `set_props()`
   had a formal called `object`, then `set_props(x, object = 1)` would bind
   `object` to the formal instead of treating it as the property `object`.
2. Callers sometimes want to supply property values programmatically, as a
   single list, rather than as individual `name = value` pairs.

`splice_dots()` and the underscore naming convention solve these.

## The underscore convention

Property names starting with `_` are reserved for internal use (documented in
`new_property()`). This lets you name a fixed argument with a leading `_` so it
can never collide with a real property passed through `...`.

- ``new_object(`_parent`, ...)`` — `_parent` is the parent object.
- ``set_props(`_object`, ...)`` — `_object` is the object to modify.

`_parent` and `_object` are not syntactically valid names, so they **must be
backtick-quoted** everywhere they appear:

```r
set_props <- function(`_object`, ..., .check = TRUE) {
  props(`_object`, check = .check) <- splice_dots(...)
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

Do *not* rename existing public arguments where it would be a breaking change
without good reason. For example `convert(from, to, ...)` keeps `from`/`to` as
is; only the dots-handling was updated.

## Collecting the dots with `splice_dots()`

`splice_dots(...)` returns a named list of the `...` values. As a convenience,
if `...` is a single unnamed list, its elements are used instead, which makes
it easy to build the values programmatically. It errors if any value is
unnamed.

```r
splice_dots(x = 1, y = 2)      # list(x = 1, y = 2)
splice_dots(list(x = 1, y = 2)) # list(x = 1, y = 2) -- spliced
splice_dots(1)                  # error: All arguments to `...` must be named.
splice_dots(list(1))            # error: All elements of `..1` must be named.
```

Replace any `args <- list(...)` that collects properties with
`args <- splice_dots(...)`. Don't hand-roll the single-list check — that logic
lives in `splice_dots()` (and `is_single_list()`) so all callers behave
identically.

## Getting the error call right

`splice_dots()` reports its error against its caller via
`error_call = sys.call(-1)`. This is correct for ordinary functions:

```r
new_object <- function(`_parent`, ...) {
  ...
  args <- splice_dots(...)   # errors blame `new_object()`
  ...
}
```

It is **not** reliable inside an S7 generic, because dispatch rewrites the call
stack (you'll see a bogus call like `.set_ops_need_as_vector()`). For a
generic, pass the call explicitly:

```r
convert <- function(from, to, ...) {
  ...
  } else if (is_down_cast(from, to)) {
    convert_down(from, to, splice_dots(..., error_call = quote(convert())))
  }
  ...
}
```

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

roxygen handles `@param _object` and a backtick-quoted formal fine; the usage
line renders as ``set_props(`_object`, ...)``.

## Worked example

A function that returns a modified copy of an object, setting properties from
`...`:

```r
#' @param _object The object to modify. It has an unusual name to avoid clashing
#'   with property names supplied in `...`; see [new_property()] for details.
#' @param ... Name-value pairs of properties to set. As a convenience, you can
#'   supply a single unnamed list instead of individual name-value pairs.
update_props <- function(`_object`, ...) {
  props(`_object`) <- splice_dots(...)
  `_object`
}
```

Now both of these work, even for a property named `object`:

```r
update_props(x, object = 1, width = 10)
update_props(x, list(object = 1, width = 10))
```

## Checklist

- [ ] Name any clash-prone fixed argument with a leading `_`, backtick-quoted
      everywhere it appears.
- [ ] Collect property values with `splice_dots(...)`, not `list(...)`.
- [ ] For an S7 generic, pass `error_call = quote(generic())` to `splice_dots()`.
- [ ] Document the `_` argument (cross-reference `new_property()`) and the
      single-list shortcut on `...`.
- [ ] Add a test that a property whose name matches the old fixed argument can
      now be set, and a test for the single-list shortcut.
- [ ] Run `air format .` and re-document.
