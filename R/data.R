#' Get/set underlying "base" data
#'
#' When an S7 class inherits from an existing base type, it can be useful
#' to work with the underlying object, i.e. the S7 object stripped of its
#' S7 class and properties. If the class inherits from an S3 class,
#' `S7_data()` preserves the S3 class so the result remains a valid
#' object of that type.
#'
#' @inheritParams prop
#' @param value Object used to replace the underlying data.
#' @return `S7_data()` returns the data stored in the base object;
#'   `S7_data<-()` is called for its side-effects and returns `object`
#'   invisibly.
#' @export
#' @examples
#' Text := new_class(parent = class_character)
#' y <- Text(c(foo = "bar"))
#' y
#' S7_data(y)
#'
#' S7_data(y) <- c("a", "b")
#' y
#'
#' # S3 classes are preserved
#' MyDF := new_class(parent = class_data.frame)
#' S7_data(MyDF(data.frame(x = 1, y = 2)))
S7_data <- function(object) {
  check_is_S7(object)
  check_not_reference(object, "S7_data()")

  # Objects accepted by is_legacy_S7_object() have no parent metadata to restore.
  base <- base_parent(S7_class(object))
  out <- zap_attr(object, c(s7_data_attrs(object, base), S7_transient_attrs))
  if (is_S3_class(base)) {
    class(out) <- base$class
  }
  out
}

S7_transient_attrs <- c(".getting_prop", ".setting_prop", ".should_validate")

s7_data_attrs <- function(
  object,
  base = base_parent(S7_class(object)),
  replacing = FALSE
) {
  storage <- prop_storage_names(object)
  if (is_S3_class(base)) {
    data_attrs <- base$attributes
    if (replacing) {
      # Writable properties keep their stored values when the data is replaced.
      props <- class_properties(S7_class(object))
      read_only <- vlapply(props, prop_is_read_only)
      data_attrs <- intersect(data_attrs, storage[read_only])
    }
    storage <- setdiff(storage, data_attrs)
  }
  c(storage, S7_object_attrs)
}

# Walk up the @parent chain to the first non-S7 ancestor (or S7_object),
# resolving external classes so the walk can continue through them.
base_parent <- function(class) {
  while (is_class(class)) {
    parent <- attr(class, "parent", exact = TRUE)
    if (is.null(parent)) {
      break
    }
    class <- resolve_parent(parent, attr(class, "package", exact = TRUE))
  }
  class
}

#' @export
#' @rdname S7_data
`S7_data<-` <- function(object, check = TRUE, value) {
  check_is_S7(object)
  check_not_reference(object, "S7_data<-")

  for (name in s7_data_attrs(object, replacing = TRUE)) {
    attr(value, name) <- attr(object, name, exact = TRUE)
  }
  if (isTRUE(check)) {
    validate(value)
  }
  return(invisible(value))
}

zap_attr <- function(x, names) {
  for (name in names) {
    attr(x, name) <- NULL
  }
  x
}
