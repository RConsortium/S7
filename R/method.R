#' Retrieve or register an r7 method for a generic
#'
#' @param generic The generic to retrieve or register
#' @param signature The method signature
#' @param value The new function to use for the method.
#' @export
method <- function(generic, signature) {
  # This slows down the method dispatch too much
  #generic <- as_generic(generic)

  out <- .Call(method_, generic, signature)
  if(is.null(out)) {
    # If no R7 method is found, see if there are any S3 methods registered
    if (inherits(generic, "r7_generic")) {
      generic <- generic@name
    } else {
      generic <- as.character(substitute(generic))
    }

    out <- getS3method(generic, signature[[1]][[1]], optional = TRUE)

    # If no method found check if the generic has a default method
    out <- getS3method(generic, "default", optional = TRUE)
  }

  if (is.null(out)) {
    stop(sprintf("No methods found for generic '%s' for classes:\n%s", generic, paste0("- ", signature,  collapse = "\n"), call. = FALSE))
  }

  out
}

#' @rdname method
#' @export
method_register <- function(generic, signature, value) {
  generic <- as_generic(generic)

  if (!is.character(signature) && !inherits(signature, "list")) {
    signature <- list(signature)
  }

  generic_name <- generic@name

  p_tbl <- generic@methods

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

as_generic <- function(generic) {
  if (length(generic) == 1 && is.character(generic)) {
    generic <- match.fun(generic)
  }

  if (!inherits(generic, "r7_generic")) {
    stop("`generic` must be a 'r7_generic':\n- `generic` is a '", class(generic)[[1]], "'", call. = FALSE)
  }

  generic
}
