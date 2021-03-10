#' Retrieve or register an R7 method for a generic
#'
#' @param generic The generic to retrieve or register
#' @param signature The method signature
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
  if (is.null(out)) {
    # If no R7 method is found, see if there are any S3 methods registered
    if (inherits(generic, "R7_generic")) {
      args <- generic@signature
      generic <- generic@name
    } else {
      generic <- find_function_name(generic, topenv(environment(generic)))
      args <- args(formals(generic))
    }
    args <- args[names(args) != "..."]

    out <- getS3method(generic, signature[[1]][[1]], optional = TRUE)

    # If no method found check if the generic has a default method
    out <- getS3method(generic, "default", optional = TRUE)
  }

  if (is.null(out)) {
    stop(sprintf("Can't find method for generic '%s' with arguments of type:\n%s", generic, paste0("- ", names(args), ": ", vcapply(signature, paste0, collapse = ", "), collapse = "\n"), call. = FALSE))
  }

  out
}

find_function_name <- function(x, env) {
  nms <- ls(env, all.names = TRUE, sorted = FALSE)
  for (name in nms) {
    if (identical(get0(name, envir = env, mode = "function", inherits = FALSE), x)) {
      return(name)
    }
  }
  return(NULL)
}

#' Retrieve the next applicable method after the current one
#'
#' @export
method_next <- function() {
  current_method <- sys.function(sys.parent(1))

  methods <- list()
  i <- 1
  while (!inherits(current_method, "R7_generic")) {
    methods <- c(methods, current_method)
    i <- i + 1
    current_method <- sys.function(sys.parent(i))
  }

  generic <- current_method
  signature <- eval(generic_generate_signature_call(generic@signature), parent.frame())

  method_impl(generic, signature, ignore = methods)
}

#' Register R7 methods
#'
#' When registering methods for R7 generics defined in other packages you must
#' put `method_register()` in your packages [.onLoad] function.
#'
#' @importFrom utils getFromNamespace packageName
#' @export
method_register <- function() {
  package <- packageName(parent.frame())
  tbl <- asNamespace(package)[[".__S3MethodsTable__."]][[".R7_methods"]]
  for (x in tbl) {
    if (isNamespaceLoaded(x$package)) {
      ns <- asNamespace(x$package)
      new_method(getFromNamespace(x$generic, ns), x$signature, x$value)
    } else {
      setHook(packageEvent(x$package, "onLoad"), local({x <- x
        function(...) {
        ns <- asNamespace(x$package)
        new_method(getFromNamespace(x$generic, ns), x$signature, x$value)
      }}))
    }
  }
}

#' @rdname method
#' @export
new_method <- function(generic, signature, value) {
  if (is.character(generic)) {
    if (!length(generic) == 1) {
      stop("`generic` must be a generic function or a length 1 character vector", call. = FALSE)
    }

    pieces <- strsplit(generic, "::")[[1]]
    if (length(pieces) == 2) {
      package <- pieces[[1]]
      generic <- pieces[[2]]
      # Get current package, if any
      current_package <- packageName(parent.frame())
      if (!is.null(current_package)) {
        tbl <- asNamespace(current_package)[[".__S3MethodsTable__."]]
        if (is.null(tbl[[".R7_methods"]])) {
          tbl[[".R7_methods"]] <- list()
        }
        tbl[[".R7_methods"]] <- append(tbl[[".R7_methods"]], list(list(generic = generic, package = package, signature = signature, value = value)))

        return(invisible())
      }
      generic <- getFromNamespace(generic, asNamespace(package))
    }
  }

  generic <- as_generic(generic)

  if (!is.character(signature) && !inherits(signature, "list")) {
    signature <- list(signature)
  }

  if (!inherits(value, "R7_method")) {
    value <- R7_method(generic, signature, value)
  }

  generic_name <- generic@name

  p_tbl <- generic@methods

  for (i in seq_along(signature)) {
    if (inherits(signature[[i]], "class_union")) {
      for (class in signature[[1]]@classes) {
        new_method(generic, c(signature[seq_len(i - 1)], class@name), value)
      }
      return(invisible(generic))
    } else if (inherits(signature[[i]], "R7_class")) {
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
  new_method(generic, signature, value)
}

as_generic <- function(generic) {
  if (length(generic) == 1 && is.character(generic)) {
    generic <- match.fun(generic)
  }

  if (!inherits(generic, "R7_generic")) {
    stop("`generic` must be a 'R7_generic':\n- `generic` is a '", class(generic)[[1]], "'", call. = FALSE)
  }

  generic
}
