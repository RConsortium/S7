#' Retrieve or register an r7 method for a generic
#'
#' @param generic The generic to retrieve or register
#' @param signature The method signature
#' @param ignore An optional function to ignore during method lookup
#' @param value The new function to use for the method.
#' @importFrom utils getS3method
#' @export
method <- function(generic, signature) {
  method_impl(generic, signature, ignore = NULL)
}

method_impl <- function(generic, signature, ignore) {
  # This slows down the method dispatch too much
  #generic <- as_generic(generic)

  out <- .Call(method_, generic, signature, ignore)
  if(is.null(out)) {
    # If no R7 method is found, see if there are any S3 methods registered
    if (inherits(generic, "r7_generic")) {
      args <- generic@signature
      generic <- generic@name
    } else {
      generic <- as.character(substitute(generic))
      args <- args(formals(match.fun(generic)))
    }
    args <- args[names(args) != "..."]

    out <- getS3method(generic, signature[[1]][[1]], optional = TRUE)

    # If no method found check if the generic has a default method
    out <- getS3method(generic, "default", optional = TRUE)
  }

  if (is.null(out)) {
    stop(sprintf("No methods found for generic '%s' with classes:\n%s", generic, paste0("- ", names(args), ": ", vcapply(signature, paste0, collapse = ", "), collapse = "\n"), call. = FALSE))
  }

  out
}

#' Retrieve the next applicable method after the current one
#'
#' @inheritParams method
#' @param current_method The class of the current method
#' @export
method_next <- function(generic, signature) {
  current_method <- sys.function(sys.parent(1))
  method_impl(generic, signature, ignore = current_method)
}

#' @rdname method
#' @export
method_new <- function(generic, signature, value) {
  generic <- as_generic(generic)

  if (!is.character(signature) && !inherits(signature, "list")) {
    signature <- list(signature)
  }

  if (!inherits(value, "r7_method")) {
    value <- r7_method(generic, signature, value)
  }

  generic_name <- generic@name

  p_tbl <- generic@methods

  for (i in seq_along(signature)) {
    if (inherits(signature[[i]], "class_union")) {
      for (class in signature[[1]]@classes) {
        method_new(generic, c(signature[seq_len(i - 1)], class@name), value)
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
  method_new(generic, signature, value)
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
