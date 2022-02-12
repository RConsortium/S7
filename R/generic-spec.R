as_generic <- function(x) {
  if (inherits(x, "R7_generic") || is_external_generic(x)) {
    x
  } else if (inherits(x, "genericFunction")) {
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
    name <- find_generic_name(x)
    if (!is.na(name) && is_internal_generic(name)) {
      return(S3_generic(x, name))
    }
  }

  stop("`generic` is a function, but not an S3 generic function", call. = FALSE)
}

S3_generic <- function(generic, name) {
  structure(list(generic = generic, name = name), class = "R7_S3_generic")
}

is_s3_generic <- function(x) inherits(x, "R7_S3_generic")


# Internal generics -------------------------------------------------------

find_generic_name <- function(generic) {
  env <- baseenv()
  for (nme in names(env)) {
    if (identical(generic, env[[nme]])) {
      return(nme)
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

  lapply(groups, function(x) unlist(lapply(x, methods::getGroupMembers)))
}