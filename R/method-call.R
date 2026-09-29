#' Access the generic call and user frame from within a method
#'
#' @description
#' These helpers give a method stable access to three pieces of context that
#' are otherwise obscured by S7's dispatch machinery:
#'
#' * `S7_generic_call()` returns the originating call to the generic. This is
#'   useful as the `call` for an error message, so that the user sees the
#'   generic call (e.g. `foo(1)`) rather than S7's internal dispatch.
#'
#' * `S7_user_frame()` returns the frame from which the generic was called.
#'   This is the equivalent of [parent.frame()] in an S3 method, and is
#'   useful if you need non-standard evaluation.
#'
#' * `S7_generic_fun()` returns the generic function itself. This is useful if
#'   you need to inspect the generic, e.g. to retrieve its name or dispatch
#'   arguments.
#'
#' `S7_generic_call()` and `S7_user_frame()` skip intermediate frames when a
#' method re-dispatches the same generic to a superclass with [super()],
#' reporting the outermost user-facing call and its caller. Set
#' `skip_super = FALSE` to report the nearest call to the generic instead, i.e.
#' the `super()` call inside the method.
#'
#' You can also call these helpers from a function that the method calls, such
#' as a shared error helper; they use the innermost active method of an S7
#' generic. They aren't supported in methods for S3 or S4 generics, or for
#' operators like `+`.
#'
#' @param match Set to `TRUE` to process with [match.call()] and name all
#'   arguments.
#' @param skip_super Set to `FALSE` to stop at methods that re-dispatch the
#'   same generic with [super()], instead of skipping past them.
#' @returns `S7_generic_call()` returns a call; `S7_user_frame()` returns an
#'   environment; `S7_generic_fun()` returns the generic function. All error if
#'   called outside of a method.
#' @seealso [S7_dispatch()] for how methods are called, and [super()] for
#'   superclass dispatch.
#' @export
#' @examples
#' # S7_generic_call() reports the call to the generic:
#' foo := new_generic("x")
#' method(foo, class_double) <- function(x) {
#'   list(user = S7_generic_call(), nearest = S7_generic_call(skip_super = FALSE))
#' }
#' foo(1)
#'
#' # By default, it skips past super() re-dispatches:
#' Number := new_class(parent = class_double)
#' method(foo, Number) <- function(x) {
#'   foo(super(x, class_double))
#' }
#' foo(Number(1))
#'
#' # S7_user_frame() supplies the enclosing environment for non-standard
#' # evaluation, so an expression can mix columns of the data with variables
#' # from where the generic was called, like subset():
#' keep_rows := new_generic("data")
#' method(keep_rows, class_data.frame) <- function(data, condition) {
#'   rows <- eval(substitute(condition), data, S7_user_frame())
#'   data[rows, , drop = FALSE]
#' }
#'
#' threshold <- 4
#' df <- data.frame(x = 1:3, y = c(2, 5, 8))
#' # `x` and `y` come from the data frame; `threshold` from this frame
#' keep_rows(df, x + y > threshold)
#'
#' # S7_generic_fun() returns the generic itself, e.g. to use its name in a
#' # message:
#' bar := new_generic("x")
#' method(bar, class_double) <- function(x) {
#'   generic <- S7_generic_fun()
#'   paste0("Called ", generic@name, "()")
#' }
#' bar(1)
S7_generic_call <- function(match = FALSE, skip_super = TRUE) {
  stopifnot(isTRUE(skip_super) || isFALSE(skip_super))
  idx <- generic_call_frame(skip_super)
  call <- sys.call(idx)
  if (isTRUE(match)) {
    call <- match.call(sys.function(idx), call, envir = sys.frame(idx))
  }
  call
}

#' @rdname S7_generic_call
#' @export
S7_user_frame <- function(skip_super = TRUE) {
  stopifnot(isTRUE(skip_super) || isFALSE(skip_super))
  frame <- generic_call_frame(skip_super)
  sys.frame(sys.parents()[frame])
}

#' @rdname S7_generic_call
#' @export
S7_generic_fun <- function() {
  # super() only re-dispatches the same generic, so no need to skip it
  frame <- generic_call_frame(skip_super = FALSE)
  sys.function(frame)
}

generic_call_frame <- function(skip_super, call = sys.call(-1L)) {
  parents <- sys.parents()

  frame <- active_generic_frame(parents)
  if (is.na(frame)) {
    stop2("Must be called from within a method.", call = call)
  }

  if (!skip_super) {
    return(frame)
  }

  # Walk past same-generic super() re-dispatches.
  while (is_super_dispatch(frame)) {
    parent <- parent_generic_frame(frame, parents)
    if (
      is.na(parent) || !identical(sys.function(parent), sys.function(frame))
    ) {
      break
    }
    frame <- parent
  }

  frame
}

# Frame of the generic that dispatched the innermost active method.
# S7_dispatch() evaluates the method in the generic's frame, so the generic is
# always the method's direct parent. Methods invoked any other way (e.g. by an
# operator, or called directly) have no generic frame. Methods registered for
# S3 and S4 generics are plain functions, so they look like helpers called by
# the enclosing S7 method (if any).
active_generic_frame <- function(parents) {
  for (i in rev(seq_along(parents))) {
    fun <- sys.function(i)
    if (inherits(fun, "S7_generic")) {
      break
    }
    if (inherits(fun, "S7_method")) {
      generic <- parents[[i]]
      if (generic > 0L && inherits(sys.function(generic), "S7_generic")) {
        return(generic)
      }
      break
    }
  }

  NA_integer_
}

parent_generic_frame <- function(frame, parents) {
  parent <- parents[[frame]]
  while (parent > 0L) {
    if (inherits(sys.function(parent), "S7_generic")) {
      return(parent)
    }
    parent <- parents[[parent]]
  }

  NA_integer_
}

# S7_dispatch() marks the generic's frame with `_dispatched_super` when it
# unwraps a super() object.
is_super_dispatch <- function(i) {
  exists("_dispatched_super", envir = sys.frame(i), inherits = FALSE)
}
