#' Access the generic call and user frame from within a method
#'
#' @description
#' These helpers give a method stable access to two pieces of context that are
#' otherwise obscured by S7's dispatch machinery:
#'
#' * `S7_generic_call()` returns the originating call to the generic. This is
#'   useful as the `call` for an error message, so that the user sees the
#'   generic call (e.g. `foo(1)`) rather than S7's internal dispatch.
#'
#' * `S7_user_frame()` returns the frame from which the generic was called.
#'   This is the equivalent of [parent.frame()] in an S3 method, and is
#'   useful if you need non-standard evaluation.
#'
#' Both helpers skip intermediate frames when a method re-dispatches the same
#' generic to a superclass with [super()], reporting the outermost user-facing
#' call and its caller.
#'
#' @returns `S7_generic_call()` returns a call; `S7_user_frame()` returns an
#'   environment. Both error if called outside of a method.
#' @seealso [S7_dispatch()] for how methods are called, and [super()] for
#'   superclass dispatch.
#' @export
#' @examples
#' # S7_generic_call() reports the call to the generic, skipping super():
#' foo <- new_generic("foo", "x")
#' method(foo, class_double) <- function(x) {
#'   S7_generic_call()
#' }
#'
#' Number <- new_class("Number", parent = class_double)
#' method(foo, Number) <- function(x) {
#'   foo(super(x, class_double))
#' }
#'
#' foo(Number(1))
#'
#' # S7_user_frame() supplies the enclosing environment for non-standard
#' # evaluation, so an expression can mix columns of the data with variables
#' # from where the generic was called, like subset():
#' keep_rows <- new_generic("keep_rows", "data")
#' method(keep_rows, class_data.frame) <- function(data, condition) {
#'   rows <- eval(substitute(condition), data, S7_user_frame())
#'   data[rows, , drop = FALSE]
#' }
#'
#' threshold <- 4
#' df <- data.frame(x = 1:3, y = c(2, 5, 8))
#' # `x` and `y` come from the data frame; `threshold` from this frame
#' keep_rows(df, x + y > threshold)
S7_generic_call <- function() {
  idx <- generic_call_frame()
  sys.call(idx)
}

#' @rdname S7_generic_call
#' @export
S7_user_frame <- function() {
  frame <- generic_call_frame()
  sys.frame(sys.parents()[frame])
}

generic_call_frame <- function(call = sys.call(-1L)) {
  parents <- sys.parents()

  # Find the nearest enclosing generic, scanning outward from the caller.
  frame <- NA_integer_
  for (i in rev(seq_len(sys.nframe() - 1L))) {
    if (inherits(sys.function(i), "S7_generic")) {
      frame <- i
      break
    }
  }
  if (is.na(frame)) {
    stop2("Must be called from within a method.", call = call)
  }

  # Walk past super() re-dispatches: i.e. a generic whose caller is itself a
  # method dispatched by the *same* generic
  repeat {
    caller <- parents[frame]
    if (caller == 0L) {
      break
    }
    caller_generic <- parents[caller]
    is_super <- caller_generic != 0L &&
      inherits(sys.function(caller_generic), "S7_generic") &&
      identical(sys.function(caller_generic), sys.function(frame)) &&
      !identical(sys.function(caller), S7_dispatch)
    if (is_super) {
      frame <- caller_generic
    } else {
      break
    }
  }

  frame
}
