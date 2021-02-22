#' Retrieve or register an r7 method for a generic
#'
#' @param generic The generic to retrieve or register
#' @param signature The method signature
#' @param value The new function to use for the method.
#' @export
method <- function(generic, signature) {
  .Call(method_, generic, signature);
}

#' @rdname method
#' @export
method_register <- function(generic, signature, value) {
  env <- environment(generic)
  generic_name <- generic@name

  table <- env[[".r7_methods"]]
  if (is.null(table)) {
    table <- new.env(hash = TRUE, parent = emptyenv())
    env[[".r7_methods"]] <- table
  }
  gen_tbl <- table[[generic_name]]
  if (is.null(gen_tbl)) {
    gen_tbl <- new.env(hash = TRUE, parent = emptyenv())
    table[[generic_name]] <- gen_tbl
  }

  p_tbl <- gen_tbl
  while (length(signature) > 1) {
    tbl <- p_tbl[[signature[[1]]]]
    if (is.null(tbl)) {
      tbl <- new.env(hash = TRUE, parent = emptyenv())
      p_tbl[[signature[[1]]]] <- tbl
    }
    signature <- signature[-1]
    p_tbl <- tbl
  }

  p_tbl[[signature[[1]]]] <- value

  invisible(generic)
}

#' @rdname method
#' @export
`method<-` <- function(generic, signature, value) {
  method_register(generic, signature, value)
}
