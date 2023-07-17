as_generic <- function(x) {
  if (is_generic(x) || is_external_generic(x) || is_S4_generic(x)) {
    x
  } else if (is.function(x)) {
    as_S3_generic(x)
  } else {
    msg <- sprintf("`generic` must be a function, not a %s", obj_desc(x))
    stop(msg, call. = FALSE)
  }
}

as_S3_generic <- function(x) {
  use_method <- find_call(body(x), quote(UseMethod))
  if (!is.null(use_method)) {
    return(S3_generic(x, as.character(use_method[[2]])))
  } else {
    name <- find_base_name(x)
    if (name %in% names(base_ops)) {
      return(base_ops[[name]])
    } else if (!is.na(name) && is_internal_generic(name)) {
      return(S3_generic(x, name))
    }
  }

  stop("`generic` is a function, but not an S3 generic function", call. = FALSE)
}

S3_generic <- function(generic, name) {
  out <- list(generic = generic, name = name)
  class(out) <- "S7_S3_generic"
  out
}

is_S3_generic <- function(x) inherits(x, "S7_S3_generic")

is_S4_generic <- function(x) inherits(x, "genericFunction")

# Is the generic defined in the "current" package
is_local_generic <- function(generic, package) {
  if (is_external_generic(generic)) {
    return(FALSE)
  }

  generic_pkg <- package_name(generic)
  is.null(generic_pkg) || generic_pkg == package
}

package_name <- function(f) {
  env <- environment(f)
  if (is.null(env)) {
    "base"
  } else {
    (packageName(env))
  }
}

generic_n_dispatch <- function(x) {
  if (is_S3_generic(x)) {
    1
  } else if (is_generic(x)) {
    length(x@dispatch_args)
  } else if (is_external_generic(x)) {
    length(x$dispatch_args)
  } else if (methods::is(x, "genericFunction")) {
    length(x@signature)
  } else {
    stop(sprintf("Invalid input %", obj_desc(x)), call. = FALSE)
  }
}

# Internal generics -------------------------------------------------------

find_base_name <- function(f, candidates = NULL) {
  env <- baseenv()
  candidates <- candidates %||% names(env)
  for (name in candidates) {
    if (identical(f, env[[name]])) {
      return(name)
    }
  }

  NA
}

is_internal_generic <- function(x) {
  x %in% internal_generics()
}

internal_generics <- function() {
  group <- unlist(group_generics(), use.names = FALSE)
  primitive <- .S3PrimitiveGenerics

  # Extracted from ?"internal generic"
  internal <- c("[", "[[", "$", "[<-", "[[<-", "$<-", "unlist",
    "cbind", "rbind", "as.vector")

  c(group, primitive, internal)
}

group_generics <- function() {
  # S3 group generics can be defined by combining S4 group generics
  groups <- list(
    Ops = c("Arith", "Compare", "Logic"),
    Math = c("Math", "Math2"),
    Summary = "Summary",
    Complex = "Complex"
  )

  out <- lapply(groups, function(x) unlist(lapply(x, methods::getGroupMembers)))
  if (getRversion() >= "4.3") {
    out$matrixOps <- c("%*%")
  }
  out
}
