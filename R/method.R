#' Retrieve or register an r7 method for a generic
#'
#' @param generic The generic to retrieve or register
#' @param signature The method signature
#' @param value The new function to use for the method.
#' @param envir The environment to lookup the generic in.
#' @export
method <- function(generic, signature, envir = parent.frame()) {
  fun <- get(generic, envir = envir)
  env <- environment(fun)
  table <- env[[".r7_methods"]]
  tbl <- table[[generic]]
  while(length(signature) > 1) {
    classes <- .Call(class_names_, signature[[1]])
    tbl <- find_method_table(classes, tbl)
    signature <- signature[-1]
  }
  classes <- .Call(class_names_, signature[[1]])
  find_method_table(classes, tbl)
}

find_method_table <- function(classes, table) {
  for (class in classes) {
    val <- table[[class]]
    if (!is.null(val)) {
      return(val)
    }
  }
}
  #.Call(method_, generic, signature, parent.frame())

#' @rdname method
#' @param envir The environment to lookup the generic in.
#' @export
method_register <- function(generic, signature, value, envir = parent.frame()) {
  fun <- get(generic, envir = envir)
  env <- environment(fun)
  table <- env[[".r7_methods"]]
  if (is.null(table)) {
    table <- new.env(hash = TRUE, parent = emptyenv())
    env[[".r7_methods"]] <- table
  }
  gen_tbl <- table[[generic]]
  if (is.null(gen_tbl)) {
    gen_tbl <- new.env(hash = TRUE, parent = emptyenv())
    table[[generic]] <- gen_tbl
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
}

blah <- function(generic) {
  fun <- get(generic, envir = parent.frame())
}
