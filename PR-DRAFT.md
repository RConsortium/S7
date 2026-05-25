Title: Improve property getter and setter tracebacks

Body:

## Summary

Before this change, if a custom property getter or setter threw an error,
the traceback pointed at the inlined closure body. It did not show which
class or property was being accessed, which made the error harder to
connect back to the S7 object:

```r
Error in (function (self) stop("nope"))(<object>):
! nope
```

Now the same error points at the property accessor:

```r
Error in `<foo>@x`:
! nope
```

That makes it clear that S7 was evaluating the getter or setter for
property `x` on class `foo`.

The public R APIs and `.Call()` signatures are unchanged.

## Implementation notes

Internally, dynamic getters and setters are now called through a shared
non-hash environment parented to the S7 namespace. The environment is
created when the native code starts up.

For each accessor call, the C code temporarily binds the closure to a
synthetic name like `<foo>@x`, evaluates `` `<foo>@x`(object) `` or
`` `<foo>@x`(object, value) `` with `Rf_eval()`, then uses
`R_UnwindProtect()` to remove the binding on success or error.

The temporary no-recursion marker is cleared from the object that entered
the accessor on errors. On successful setter calls, it is also cleared
from the returned object when that is a different object.

This avoids adding `withCallingHandlers()` around the hot property
access path.

This is an alternative to PR 627's `withCallingHandlers()` approach. It
keeps the traceback improvement for dynamic property accessors, but builds
the displayed call in the C accessor path instead of wrapping every public
property access at the R level.

## Follow-up Errors

Two related kinds of property errors still deserve the same kind of
traceback improvement:

- Errors thrown by S7 itself for bad property operations, such as
  accessing or assigning a property that does not exist.
- Errors thrown from custom validators. These likely need a different
  way to choose the right displayed call, because the error comes from
  user validation code rather than from the getter or setter call itself.

## Benchmarks

Benchmarks used optimized temporary installs for `origin/main`, PR 627
(`origin/wrap-prop-error`), and this branch. Each expression ran for
100,000 iterations with `bench::mark()` on R 4.6.0 (2026-04-24),
`aarch64-apple-darwin23` on Darwin arm64.

The getter error benchmark clears the temporary getter marker on a local
object before each access so `origin/main` continues to run the erroring
getter after the first error.

| Variant                        | Getter success median | Getter error median | Error call head |
| ------------------------------ | --------------------: | ------------------: | --------------- |
| current/main                   |                656 ns |            8,569 ns | `<closure>`     |
| PR 627 `withCallingHandlers()` |              1,599 ns |           16,154 ns | `@`             |
| new shared-env C path          |                779 ns |            8,938 ns | `<foo>@x`       |
