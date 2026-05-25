Title: Improve property getter and setter tracebacks

Body:

## Summary

Before this change, errors from custom property getters and setters did
not clearly identify the property being accessed. If the error had no
call, R printed only the message:

```r
Error: nope
```

If the error used R's default call, R printed the inlined accessor
closure:

```r
Error in (function (self)  : nope
Calls: @ -> @.S7_object -> <Anonymous>
```

Neither form tells you that S7 was evaluating property `x` on class
`foo`.

With this change, ordinary errors from dynamic getters and setters show
the property accessor in the top-level error:

```r
Error in `<foo>@x`(<object>) : nope
Calls: @ -> @.S7_object -> <foo>@x
```

Errors deliberately signaled with no call, such as
`stop("nope", call. = FALSE)`, still print without a call in the error
header. But they now have a useful property frame in `traceback()`:

```r
Error: nope

> traceback()
4: stop("nope", call. = FALSE) at #1
3: `<foo>@x`(<object>) at property.R#197
2: `@.S7_object`(foo(), x)
1: foo()@x
```

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
improves the common dynamic accessor case by changing the call R sees
while evaluating the getter or setter, instead of rewriting error
conditions after they have been thrown. That means this keeps the hot path
lean, but it does not rewrite call-less error headers the way PR 627 does.

## Follow-up Errors

Two related kinds of property errors still deserve the same kind of
traceback improvement:

- Errors thrown by S7 itself for bad property operations, such as
  accessing or assigning a property that does not exist.
- Errors thrown from custom validators. These likely need a different way
  to choose the right displayed call, because the error comes from
  validation code rather than from a dynamic getter or setter.

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
