global_variables <- function(names) {
  env <- topenv(parent.frame())
  if (
    exists(".__global__", envir = env) &&
      bindingIsLocked(".__global__", env = env)
  ) {
    get("unlockBinding", baseenv())(".__global__", env = env)
    on.exit(lockBinding(".__global__", env = env))
  }
  current <- get0(".__global__", envir = env, ifnotfound = character())
  current <- unique(c(current, names))
  assign(".__global__", current, envir = env)
}

vlapply <- function(X, FUN, ...) {
  vapply(X = X, FUN = FUN, FUN.VALUE = logical(1), ...)
}
every <- function(X, FUN, ...) {
  for (x in X) {
    if (!FUN(x, ...)) {
      return(FALSE)
    }
  }
  TRUE
}
some <- function(X, FUN, ...) {
  for (x in X) {
    if (FUN(x, ...)) {
      return(TRUE)
    }
  }
  FALSE
}
vcapply <- function(X, FUN, ...) {
  vapply(X = X, FUN = FUN, FUN.VALUE = character(1), ...)
}

paste_c <- function(...) {
  paste(c(...), collapse = "")
}

stop2 <- function(message, call = sys.call(-1L), class = NULL) {
  stop(errorCondition(
    message = paste(message, collapse = "\n"),
    call = call,
    class = class
  ))
}

warning2 <- function(message, call = sys.call(-1L), class = NULL) {
  warning(warningCondition(
    message = paste(message, collapse = "\n"),
    call = call,
    class = class
  ))
}

method_signature <- function(generic, signature) {
  single <- length(generic@dispatch_args) == 1
  if (single) {
    signature <- class_deparse(signature[[1]])
  } else {
    classes <- vcapply(signature, class_deparse)
    signature <- paste0("list(", paste0(classes, collapse = ", "), ")")
  }

  sprintf("method(%s, %s)", generic@name, signature)
}

names2 <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep("", length(x))
  } else {
    nms
  }
}

# Collect `...` into a named list. As a convenience, a single unnamed list is
# spliced in so its elements become the values, making it easy to supply
# values programmatically. All values must be named.
collect_dots <- function(..., call = sys.call(-1)) {
  args <- list(...)

  is_single_list <- length(args) == 1L &&
    !nzchar(names2(args)) &&
    is.list(args[[1L]])

  if (is_single_list) {
    args <- args[[1L]]
    if ("" %in% names2(args)) {
      stop2("All elements of `..1` must be named.", call = call)
    }
  } else if ("" %in% names2(args)) {
    stop2("All arguments to `...` must be named.", call = call)
  }
  args
}

is_prefix <- function(x, y) {
  length(x) <= length(y) && identical(unclass(x), unclass(y)[seq_along(x)])
}

oxford_or <- function(x) {
  n <- length(x)
  if (n == 1) {
    x
  } else if (n == 2) {
    paste0(x[[1]], " or ", x[[2]])
  } else if (n >= 2) {
    x <- c(x[seq(1, n - 2, by = 1)], paste0(x[[n - 1]], ", or ", x[[n]]))
    paste0(x, collapse = ", ")
  }
}

str_nest <- function(
  object,
  prefix,
  ...,
  nest.lev = 0,
  indent.str = paste(rep.int(" ", max(0, nest.lev + 1)), collapse = "..")
) {
  names <- format(names(object))

  for (i in seq_along(object)) {
    cat(indent.str, prefix, " ", names[[i]], ":", sep = "")

    xi <- object[[i]]
    if (is.function(xi)) {
      str_function(xi, nest.lev = nest.lev + 1)
    } else {
      str(xi, ..., nest.lev = nest.lev + 1)
    }
  }
}

str_function <- function(object, ..., nest.lev = 0) {
  attr(object, "srcref") <- NULL
  if (identical(class(object), "function")) {
    cat(" ")
  }
  str(object, ..., nest.lev = nest.lev)
}

check_name <- function(
  name,
  arg = deparse(substitute(name)),
  call = sys.call(-1L)
) {
  if (length(name) != 1 || !is.character(name)) {
    msg <- sprintf("`%s` must be a single string.", arg)
    stop2(msg, call = call)
  }
  if (is.na(name) || name == "") {
    msg <- sprintf("`%s` must not be \"\" or NA.", arg)
    stop2(msg, call = call)
  }
}

check_function <- function(
  f,
  args,
  arg = deparse(substitute(f)),
  call = sys.call(-1L)
) {
  if (!is.function(f)) {
    msg <- sprintf("`%s` must be a function.", arg)
    stop2(msg, call = call)
  }

  # `args` is either a single formals list (e.g. alist(self = , value = ))
  # or an unnamed list of such formals lists. Distinguish via names: a
  # single signature has named entries (one per arg); a list of signatures
  # is unnamed.
  if (length(args) == 0 || any(nzchar(names2(args)))) {
    candidates <- list(as.pairlist(args))
  } else {
    candidates <- lapply(args, as.pairlist)
  }

  for (cand in candidates) {
    if (identical(formals(f), cand)) {
      return(invisible())
    }
  }

  expected <- oxford_or(vapply(candidates, show_args, character(1)))
  msg <- sprintf(
    "`%s` must be %s, not %s.",
    arg,
    expected,
    show_args(formals(f))
  )
  stop2(msg, call = call)
}

show_function <- function(x, constructor = FALSE) {
  args <- formals(x)

  if (constructor) {
    # don't show the defaults arg values in the constructor, keep it compact
    # TODO: do show the default values next to properties in class printouts.
    args <- lapply(args, function(q) quote(expr = ))
  }

  show_args(args, suffix = " {...}")
}

show_args <- function(x, name = "function", suffix = "") {
  if (length(x) == 0) {
    args <- ""
  } else {
    val <- vcapply(x, deparse1)
    args <- paste0(names(x), ifelse(val == "", "", " = "), val, collapse = ", ")
  }

  paste0(name, "(", args, ")", suffix)
}

modify_list <- function(x, new_vals) {
  stopifnot(
    is.null(x) || is.list(x) || is.pairlist(x),
    all(nzchar(names2(x)))
  )
  x <- x %||% list()

  if (length(new_vals)) {
    nms <- names2(new_vals)
    if (!all(nzchar(nms))) {
      stop2("All elements in `new_vals` must be named.")
    }
    x[nms] <- new_vals
  }

  x
}

deparse_trunc <- function(x, width, collapse = "\n") {
  x <- deparse1(x, collapse)
  if (nchar(x) > width) {
    x <- sprintf("%s....", substr(x, 0, width - 4))
  }
  x
}

is_plain_list <- function(x) {
  is.list(x) && !is.object(x)
}

# For older versions of R ----------------------------------------------------
deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}

list2DF <- function(x = list(), nrow = 0L) {
  stopifnot(is.list(x), is.null(nrow) || nrow >= 0L)
  if (n <- length(x)) {
    if (length(nrow <- unique(lengths(x))) > 1L) {
      stop2("All variables should have the same length.")
    }
  } else {
    if (is.null(nrow)) {
      nrow <- 0L
    }
  }
  if (is.null(names(x))) {
    names(x) <- character(n)
  }
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(nrow)
  x
}
