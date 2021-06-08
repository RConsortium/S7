#' Retrieve or register an R7 method for a generic
#'
#' @param generic The generic to retrieve or register
#' @param signature The method signature
#' @param method,value The new function to use as the method.
#' @importFrom utils getS3method
#' @export
method <- function(generic, signature) {
  signature <- as(signature, "list")
  method_impl(generic, signature, ignore = NULL)
}

method_impl <- function(generic, signature, ignore) {
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
  NULL
}

#' Retrieve the next applicable method after the current one
#'
#' @export
next_method <- function() {
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
      new_method(getFromNamespace(x$generic, ns), x$signature, x$method)
    } else {
      setHook(packageEvent(x$package, "onLoad"),
        local({
          x <- x
          function(...) {
            ns <- asNamespace(x$package)
            if (is.null(x$version) || getNamespaceVersion(ns) >= x$version) {
              new_method(getFromNamespace(x$generic, ns), x$signature, x$method)
            }
          }
        })
      )
    }
  }
}

arg_to_string <- function(arg) {
  if (is.na(names(arg)[[1]])) {
    return("does not exist")
  }
  sprintf("is `%s = %s`", names(arg), deparse(arg[[1]]))
}

method_compatible <- function(method, generic) {
  generic_formals <- suppressWarnings(formals(args(generic)))
  method_formals <- formals(method)

  # This can happen for some primitive functions such as `[`
  if (length(generic_formals) == 0) {
    return()
  }

  for (i in seq_len(length(generic_formals) - 1)) {
    if (!identical(generic_formals[i], method_formals[i])) {
      stop(sprintf("`method` must be consistent with <R7_generic> %s.\n- Argument %i in generic %s\n- Argument %i in method %s", generic@name, i, arg_to_string(generic_formals[i]), i, arg_to_string(method_formals[i])), call. = FALSE)
    }
  }

  if ("..." %in% names(generic_formals) && !"..." %in% names(method_formals)) {
      stop(sprintf("`method` must be consistent with <R7_generic> %s.\n- `generic` has `...`\n- `method` does not have `...`", generic@name), call. = FALSE)
  }

  if (!"..." %in% names(generic_formals) && "..." %in% names(method_formals)) {
      stop(sprintf("`method` must be consistent with <R7_generic> %s.\n- `generic` does not have `...`\n- `method` has `...`", generic@name), call. = FALSE)
  }
  TRUE
}

#' @rdname method
#' @param package The package to register the method in, only used for soft
#'   dependencies. The default `NULL` looks up the package based on the parent
#'   frame.
#' @export
new_method <- function(generic, signature, method, package = NULL) {
  if (inherits(generic, "R7_external_generic")) {
    # Get current package, if any
    if (!is.null(package)) {
      tbl <- asNamespace(package)[[".__S3MethodsTable__."]]
      if (is.null(tbl[[".R7_methods"]])) {
        tbl[[".R7_methods"]] <- list()
      }
      tbl[[".R7_methods"]] <- append(tbl[[".R7_methods"]], list(list(generic = generic$generic, package = generic$package, signature = signature, method = method, version = generic$version)))

      return(invisible())
    }
    generic <- getFromNamespace(generic$generic, asNamespace(generic$package))
  }

  if (!is.character(signature) && !inherits(signature, "list")) {
    signature <- list(signature)
  }

  generic <- as_generic(generic)

  method_compatible(method, generic)

  if (!inherits(method, "R7_method")) {
    method <- R7_method(generic, signature, method)
  }

  if (inherits(generic, "S3_generic")) {
    if (inherits(signature[[1]], "R7_class")) {
      signature[[1]] <- signature[[1]]@name
    }
    registerS3method(attr(generic, "name"), signature[[1]], method, envir = parent.frame())
    return(invisible(generic))
  }

  generic_name <- generic@name

  p_tbl <- generic@methods


  for (i in seq_along(signature)) {
    if (inherits(signature[[i]], "R7_union")) {
      for (class in signature[[i]]@classes) {
        new_method(generic, c(signature[seq_len(i - 1)], class@name), method)
      }
      return(invisible(generic))
    } else if (inherits(signature[[i]], "R7_class")) {
      signature[[i]] <- signature[[i]]@name
    }
    if (i == length(signature)) {
      p_tbl[[signature[[i]]]] <- method
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
  new_method(generic, signature, value, package = packageName(parent.frame()))
}

find_generic_name <- function(generic) {
  env <- environment(generic) %||% baseenv()
  for (nme in names(env)) {
    if (identical(generic, env[[nme]])) {
      return(nme)
    }
  }
}

as_generic <- function(generic) {
  if (length(generic) == 1 && is.character(generic)) {
    fun <- match.fun(generic)
    generic <- fun
  }
  if (!inherits(generic, "R7_generic")) {
    attr(generic, "name") <- find_generic_name(generic)
    class(generic) <- "S3_generic"
  }

  generic
}

#' Lookup the R7 method for the current generic and call it.
#' @export
method_call <- function() {
  .Call(method_call_, sys.call(-1), sys.function(-1), sys.frame(-1))
}
