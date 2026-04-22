#' Force method dispatch to use a superclass
#'
#' @description
#' `super(from, to)` causes the dispatch for the next generic to use the method
#' for the superclass `to` instead of the actual class of `from`. It's needed
#' when you want to implement a method in terms of the implementation of its
#' superclass.
#'
#' ## S3 & S4
#' `super()` performs a similar role to [NextMethod()] in S3 or
#' [methods::callNextMethod()] in S4, but is much more explicit:
#'
#' * The super class that `super()` will use is known when write `super()`
#'   (i.e. statically) as opposed to when the generic is called
#'   (i.e. dynamically).
#'
#' * All arguments to the generic are explicit; they are not automatically
#'   passed along.
#'
#' This makes `super()` more verbose, but substantially easier to
#' understand and reason about.
#'
#' ## `super()` in S3 generics
#'
#' Note that you can't use `super()` in methods for an S3 generic.
#' For example, imagine that you have made a subclass of "integer":
#'
#' ```{r}
#' MyInt <- new_class("MyInt", parent = class_integer, package = NULL)
#' ```
#'
#' Now you go to write a custom print method:
#'
#' ```{r}
#' method(print, MyInt) <- function(x, ...) {
#'    cat("<MyInt>")
#'    print(super(x, to = class_integer))
#' }
#'
#' MyInt(10L)
#' ```
#'
#' This doesn't work because `print()` isn't an S7 generic so doesn't
#' understand how to interpret the special object that `super()` produces.
#' While you could resolve this problem with [NextMethod()] (because S7 is
#' implemented on top of S3), we instead recommend using [S7_data()] to extract
#' the underlying base object:
#'
#' ```{r}
#' method(print, MyInt) <- function(x, ...) {
#'    cat("<MyInt>")
#'    print(S7_data(x))
#' }
#'
#' MyInt(10L)
#' ```
#'
#' @param from An S7 object to cast.
#' @param to An S7 class specification, passed to [as_class()]. Must be a
#'   superclass of `object`.
#' @returns An `S7_super` object which should always be passed
#'   immediately to a generic. It has no other special behavior.
#' @export
#' @examples
#' Foo1 <- new_class("Foo1", properties = list(x = class_numeric, y = class_numeric))
#' Foo2 <- new_class("Foo2", Foo1, properties = list(z = class_numeric))
#'
#' total <- new_generic("total", "x")
#' method(total, Foo1) <- function(x) x@x + x@y
#'
#' # This won't work because it'll be stuck in an infinite loop:
#' method(total, Foo2) <- function(x) total(x) + x@z
#'
#' # We could write
#' method(total, Foo2) <- function(x) x@x + x@y + x@z
#' # but then we'd need to remember to update it if the implementation
#' # for total(<Foo1>) ever changed.
#'
#' # So instead we use `super()` to call the method for the parent class:
#' method(total, Foo2) <- function(x) total(super(x, to = Foo1)) + x@z
#' total(Foo2(1, 2, 3))
#'
#' # To see the difference between convert() and super() we need a
#' # method that calls another generic
#'
#' bar1 <- new_generic("bar1", "x")
#' method(bar1, Foo1) <- function(x) 1
#' method(bar1, Foo2) <- function(x) 2
#'
#' bar2 <- new_generic("bar2", "x")
#' method(bar2, Foo1) <- function(x) c(1, bar1(x))
#' method(bar2, Foo2) <- function(x) c(2, bar1(x))
#'
#' obj <- Foo2(1, 2, 3)
#' bar2(obj)
#' # convert() affects every generic:
#' bar2(convert(obj, to = Foo1))
#' # super() only affects the _next_ call to a generic:
#' bar2(super(obj, to = Foo1))
super <- function(from, to) {
  check_is_S7(from)

  to <- as_class(to)
  check_can_inherit(to)
  if (!class_inherits(from, to)) {
    msg <- sprintf(
      "%s doesn't inherit from %s",
      obj_desc(from),
      class_desc(to)
    )
    stop(msg)
  }

  # Must not change order of these fields as C code indexes by position
  structure(
    list(
      object = from,
      dispatch = class_dispatch(to)
    ),
    class = "S7_super"
  )
}

#' Force method dispatch to use the next method of a superclass
#' @description
#' `next_super(from, to)` causes the dispatch for the next generic to use the
#' **next method** for the superclass `to` instead of the actual class of
#' `from`. Unlike \link[S7:super]{`super(from, to)`}, the method of super
#' class `to` is ignored if it exists. This is convient alternative to
#' `super(from, to)` when `from` may inherit from other super classes that
#' are not explicitly known.
#'
#' @param from an S7 object
#' @param to an S7_class object of which to dispatch
#' @export
#' @examples
#' class_a <- new_class(
#'   "a",
#'   properties = list(
#'     a = class_character
#'   )
#' )
#'
#' class_b <- new_class(
#'   "b",
#'   parent = class_a,
#'   properties = list(
#'     b = class_numeric
#'   ),
#'   constructor = function(a_obj = class_a(), b = character()) {
#'     new_object(a_obj, b = b)
#'   }
#' )
#'
#' class_d <- new_class(
#'   "d",
#'   parent = class_b,
#'   properties = list(
#'     d = class_any
#'   ),
#'   constructor = function(b_obj = class_b(), d = NULL) {
#'     new_object(b_obj, d = d)
#'   }
#' )
#'
#' class_c <- new_class(
#'   "c",
#'   parent = class_a,
#'   properties = list(
#'     c = class_logical
#'   ),
#'   constructor = function(a_obj = class_a(), c = logical()) {
#'     new_object(a_obj, c = c)
#'   }
#' )
#'
#' class_e <- new_class(
#'   "e",
#'   parent = class_c,
#'   properties = list(
#'     e = class_any
#'   ),
#'   constructor = function(c_obj = class_b(), e = NULL) {
#'     new_object(c_obj, e = e)
#'   }
#' )
#'
#'
#' aa <- class_a(a = "hello")
#' ba <- class_b(aa, b = 1)
#' dba <- class_d(ba)
#' cdba <- class_c(dba, c = TRUE)
#' ecdba <- class_e(cdba)
#'
#' log_class <- function(x) {
#'   cat("inherits: ", x, "\n")
#' }
#'
#' bar <- new_generic("bar", "x")
#'
#' method(bar, class_a) <- function(x) {
#'   log_class("a")
#' }
#'
#' method(bar, class_b) <- function(x) {
#'   bar(super(x, class_a))
#'   log_class("b")
#' }
#'
#' method(bar, class_d) <- function(x) {
#'   bar(super(x, class_b))
#'   log_class("d")
#' }
#'
#' method(bar, class_c) <- function(x) {
#'   bar(super(x, class_a))
#'   log_class("c")
#' }
#'
#' method(bar, class_e) <- function(x) {
#'   bar(super(x, class_c))
#'   log_class("e")
#' }
#'
#' baz <- new_generic("baz", "x")
#'
#' method(baz, class_a) <- function(x) {
#'   log_class("a")
#' }
#'
#' method(baz, class_b) <- function(x) {
#'   baz(next_super(x, class_b))
#'   log_class("b")
#' }
#'
#' method(baz, class_d) <- function(x) {
#'   baz(next_super(x, class_d))
#'   log_class("d")
#' }
#'
#' method(baz, class_c) <- function(x) {
#'   baz(next_super(x, class_c))
#'   log_class("c")
#' }
#'
#' method(baz, class_e) <- function(x) {
#'   baz(next_super(x, class_e))
#'   log_class("e")
#' }
#'
#' # bar uses  `super`, disptaching to explicit methods
#' bar(ecdba)
#' # bar uses `next_super`, dispatching on the next inherited superclasses
#' baz(ecdba)
#' # note, if no method exists, an error will be thrown:
#' # attempt to find next method beyond the root class...
#' try(baz(next_super(ecdba, class_a)))
next_super <- function(from, to) {
  check_is_S7(from)
  to <- as_class(to)
  check_can_inherit(to)
  if (!class_inherits(from, to)) {
    msg <- sprintf(
      "%s doesn't inherit from %s", obj_desc(from),
      class_desc(to)
    )
    stop(msg)
  }
  from_to <- S7_class(from)
  to_name <- S7_class_name(to)
  while (!is.null(from_to)) {
    if (S7_class_name(from_to) == to_name) {
      break
    }
    from_to <- attr(from_to, "parent")
  }
  structure(
    list(
      object = from,
      dispatch = class_dispatch(from_to)[-1L]
    ),
    class = "S7_super"
  )
}

#' @export
print.S7_super <- function(x, ...) {
  str(x, ...)
  invisible(x)
}
#' @export
str.S7_super <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  cat("super(", obj_desc(object$object), ", <", object$dispatch[[1]], ">)", sep = "")
}
