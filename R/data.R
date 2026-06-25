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
  check_not_environment(object, "S7_data()")

  if (is_S4_data_part_object(object)) {
    return(S4_data_part(object))
  }

  out <- zap_attr(
    object,
    c(prop_storage_names(object), "class", "_S7_class", "S7_class")
  )

  base <- base_parent(S7_class(object))
  if (is_S3_class(base)) {
    class(out) <- base$class
  }
  out
}

# Walk up the @parent chain to the first non-S7 ancestor (or S7_object).
base_parent <- function(class) {
  while (is_class(class) && class@name != "S7_object") {
    class <- class@parent
  }
  class
}

#' @export
#' @rdname S7_data
`S7_data<-` <- function(object, check = TRUE, value) {
  check_is_S7(object)
  check_not_environment(object, "S7_data<-")

  if (is_S4_data_part_object(object)) {
    object <- S4_initialize_data_part(value, object)
    if (isTRUE(check)) {
      validate(object)
    }
    return(invisible(object))
  }

  s7_attrs <- c(prop_storage_names(object), "class", "_S7_class", "S7_class")
  for (name in s7_attrs) {
    attr(value, name) <- attr(object, name, exact = TRUE)
  }
  if (isTRUE(check)) {
    validate(value)
  }
  return(invisible(value))
}

is_S4_data_part_object <- function(object) {
  isS4(object) && ".Data" %in% methods::slotNames(object)
}

S4_data_part <- function(object) {
  data <- methods::slot(object, ".Data")
  attrs <- attributes(object) %||% list()
  attrs[c(S4_data_part_protected_attributes(object), ".S3Class")] <- NULL
  attributes(data) <- modify_list(attributes(data), attrs)
  data
}

zap_attr <- function(x, names) {
  for (name in names) {
    attr(x, name) <- NULL
  }
  x
}
