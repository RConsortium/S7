Title: Improve property getter and setter tracebacks

Body:

## Summary

Errors from dynamic property getters and setters now have a property-specific
call in the traceback. For a class `foo` and property `x`, errors raised inside
the dynamic getter or setter are attributed to `` `foo@x`(...) `` instead of an
inlined closure call.

The public R APIs and `.Call()` signatures are unchanged.

## Implementation notes

Internally, the C property path now evaluates dynamic getter and setter calls
through a shared non-hash environment parented to the S7 namespace and created
at native startup. Each call temporarily binds the closure under a synthetic
symbol like `foo@x`, evaluates `` `foo@x`(object) `` or
`` `foo@x`(object, value) `` with `Rf_eval()`, then uses `R_UnwindProtect()` to
restore or remove the binding on success or error.
The same unwind cleanup clears the temporary no-recursion marker used while the
getter or setter is running.

This avoids adding `withCallingHandlers()` around the hot property access path.

## Benchmarks

Benchmarks used optimized temporary installs for `origin/main`,
`origin/wrap-prop-error`, and this branch. Each expression ran for 100,000
iterations with `bench::mark()` on R version 4.6.0 (2026-04-24),
`aarch64-apple-darwin23` on Darwin arm64.

| Variant | Getter success median | Getter error median | Error call head |
| --- | ---: | ---: | --- |
| current/main | 656 ns | 2,255 ns | `<closure>` |
| PR 627 `withCallingHandlers()` | 1,476 ns | 3,116 ns | `@` |
| new shared-env C path | 738 ns | 7,257 ns | `foo@x` |

## Testing

- `testthat::test_local(".", filter = "property", load_package = "source")`
  - PASS: 102
- `testthat::test_local(".", load_package = "source")`
  - PASS: 658
