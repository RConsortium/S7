#' An r7 object
#' @export
r7_object <- class_new(
  name = "r7_object",
  parent = NULL,
  constructor = function() list()
)

#' r7 generics and method objects
#' @param name,generic The name or generic object of the generic
#' @param signature The signature of the generic
#' @param fun The function of the method'
#' @param envir The environment to lookup the generic in.
#' @export
r7_generic <- class_new(
  name = "r7_generic",
  properties = list(name = "character", signature = "list"),
  parent = "function",
  constructor = function(name, signature, envir = parent.frame()) {
    fun <- function() NULL
    formals(fun) <- signature
    sig_call <- as.call(c(as.symbol("list"), lapply(names(signature), function(x) { bquote(object_class(.(arg)), list(arg = as.symbol(x)))})))
    method_call <- as.call(c(as.call(c(as.symbol("method"), as.symbol(name), sig_call)), lapply(names(signature), as.symbol)))
    body(fun) <- method_call
    environment(fun) <- envir
    object_new(name = name, signature = signature, .data = fun)
  }
)

#' @rdname r7_generic
#' @export
r7_method <- class_new(
  name = "r7_method",
  properties = list(generic = r7_generic, signature = "list", fun = "function"),
  parent = "function",
  constructor = function(generic, signature, fun) {
    object_new(generic = generic, signature = signature, .data = fun)
  }
)

utils::globalVariables(c("parent", "constructor", "name"))
