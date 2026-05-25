Title: Improve property getter and setter tracebacks

Body:

## Summary

Errors from dynamic property getters and setters now have a property-specific
call in the traceback. For a class `foo` and property `x`, errors raised inside
the dynamic getter or setter are attributed to `` `<foo>@x`(...) `` instead of
an inlined closure call.

The public R APIs and `.Call()` signatures are unchanged.

## Implementation notes

Internally, the C property path now evaluates dynamic getter and setter calls
through a shared non-hash environment parented to the S7 namespace and created
at native startup. Each call temporarily binds the closure under a synthetic
symbol like `<foo>@x`, evaluates `` `<foo>@x`(object) `` or
`` `<foo>@x`(object, value) `` with `Rf_eval()`, then uses
`R_UnwindProtect()` to clear the transient binding on success or error.
The temporary no-recursion marker is cleared from the entry object on errors
and from the relevant entry/return objects on successful getter and setter
calls.

This avoids adding `withCallingHandlers()` around the hot property access path.

## Follow-up error classes

There are two other property-error classes that should probably get similar
call treatment in a follow-up:

- Errors thrown by S7 itself for bad property operations, such as accessing or
  assigning a property that does not exist.
- Errors thrown from custom validators. These likely need a different approach
  for constructing the appropriate property call, because the error originates
  inside user-supplied validation code rather than at the direct property
  getter/setter call boundary.

## Benchmarks

Benchmarks used optimized temporary installs for `origin/main`,
`origin/wrap-prop-error`, and this branch. Each expression ran for 100,000
iterations with `bench::mark()` on R version 4.6.0 (2026-04-24),
`aarch64-apple-darwin23` on Darwin arm64. The getter error benchmark clears
the temporary getter marker on a local object before each access so
`origin/main` continues to exercise the erroring getter after the first error.

| Variant | Getter success median | Getter error median | Error call head |
| --- | ---: | ---: | --- |
| current/main | 656 ns | 8,569 ns | `<closure>` |
| PR 627 `withCallingHandlers()` | 1,599 ns | 16,154 ns | `@` |
| new shared-env C path | 779 ns | 8,938 ns | `<foo>@x` |

## Testing

- `testthat::test_local(".", filter = "property", load_package = "source")`
  - PASS: 105
- `testthat::test_local(".", load_package = "source")`
  - PASS: 661
