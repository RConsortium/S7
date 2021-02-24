is_named <- function (x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(FALSE)
  }
  if (any(is.na(nms) | nms == "")) {
    return(FALSE)
  }
  TRUE
}

normalize_signature <- function(signature, envir = parent.frame()) {
  if (!is_named(signature)) {
    if (!is.character(signature)) {
      stop("`signature` must either be named types or an unnamed character vector of argument names", call. = FALSE)
    }
    out <- vector("list", length(signature))
    for (i in seq_along(out)) {
      out[[i]] <- quote(expr =)
    }
    names(out) <- signature
    return(out)
  }
  signature <- as.list(signature)
  for (i in seq_along(signature)) {
    if (is.character(signature[[i]])) {
      signature[[i]] <- class_get(signature[[i]], envir)
    }
  }
  signature
}
