#' Does this object inherit from a class?
#'
#' * `S7_inherits()` returns `TRUE` or `FALSE`.
#' * `check_is_S7()` throws an error if `x` isn't the specified `class`.
#'
#' @param x An object.
#' @param class A class specification (anything accepted by [as_class()]:
#'   an S7 class, S7 union, S3 class, S4 class, base type wrapper like
#'   [class_integer], or [class_any]/[class_missing]). If `NULL`, only tests
#'   whether `x` is an S7 object, without testing for a specific class.
#' @param arg Argument name used in error message.
#' @param call The call to report in the error message. Defaults to the
#'   calling function.
#' @returns
#' * `S7_inherits()` returns a single `TRUE` or `FALSE`.
#' * `check_is_S7()` returns nothing; it's called for its side-effects.
#'
#' @note Starting with \R 4.3.0, `base::inherits()` can accept an S7 class as
#' the second argument, supporting usage like `inherits(x, Foo)`.
#' @export
#' @examples
#' Foo1 := new_class()
#' Foo2 := new_class()
#'
#' S7_inherits(Foo1(), Foo1)
#' check_is_S7(Foo1())
#' check_is_S7(Foo1(), Foo1)
#'
#' S7_inherits(Foo1(), Foo2)
#' try(check_is_S7(Foo1(), Foo2))
#'
#' # Also works with other class specifications
#' S7_inherits(1L, class_integer)
#' S7_inherits(data.frame(), new_S3_class("data.frame"))
#' S7_inherits(1L, class_integer | class_character)
#'
#' if (getRversion() >= "4.3.0") {
#'   inherits(Foo1(), Foo1)
#' }
S7_inherits <- function(x, class = NULL) {
  class <- as_class(class)
  if (is.null(class)) {
    has_S7_class(x)
  } else {
    class_inherits(x, class)
  }
}

has_S7_class <- function(x) {
  identical(class(x), "S7_object") ||
    inherits(x, "S7_class") ||
    !is.null(.Call(S7_class_, x))
}

#' @export
#' @rdname S7_inherits
# called from src/prop.c
check_is_S7 <- function(
  x,
  class = NULL,
  arg = deparse(substitute(x)),
  call = sys.call(-1L)
) {
  class <- as_class(class)
  if (S7_inherits(x, class)) {
    return(invisible())
  }

  msg <- sprintf(
    "`%s` must be %s, not a %s.",
    arg,
    if (is.null(class)) "an <S7_object>" else paste0("a ", class_desc(class)),
    obj_desc(x)
  )
  stop2(msg, call = call)
}
