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
  if (!is.character(signature) && !inherits(signature, "list")) {
    signature <- list(signature)
  }

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
  for (i in seq_along(signature)) {
    if (inherits(signature[[i]], "class_union")) {
      for (class in signature[[1]]@classes) {
        method_register(generic, c(signature[seq_len(i - 1)], class@name), value)
      }
      return(invisible(generic))
    } else if (inherits(signature[[i]], "r7_class")) {
      signature[[i]] <- signature[[i]]@name
    }
    if (i == length(signature)) {
      p_tbl[[signature[[i]]]] <- value
    } else {
      tbl <- p_tbl[[signature[[i]]]]
      if (is.null(tbl)) {
        tbl <- new.env(hash = TRUE, parent = emptyenv())
        p_tbl[[signature[[i]]]] <- tbl
      }
      p_tbl <- tbl
    }
  }

  invisible(generic)
}

#' @rdname method
#' @export
`method<-` <- function(generic, signature, value) {
  method_register(generic, signature, value)
}
