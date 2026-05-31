#' S7 wrappers for matrices and arrays
#'
#' @description
#' S7 bundles classes for matrices and arrays. Because the element type of a
#' matrix or array is independent of its dimensions, there is one class for
#' each base vector type:
#'
#' * `class_logical_matrix`, `class_integer_matrix`, `class_double_matrix`,
#'   `class_complex_matrix`, `class_character_matrix`, `class_raw_matrix`, and
#'   `class_list_matrix` for matrices.
#' * `class_logical_array`, `class_integer_array`, `class_double_array`,
#'   `class_complex_array`, `class_character_array`, `class_raw_array`, and
#'   `class_list_array` for arrays.
#'
#' `class_matrix` and `class_array` are unions of these element-typed classes.
#' Like other unions (e.g. [class_numeric]), they can be used in [method()]
#' and [new_property()] but not as a `parent` in [new_class()]. To subclass a
#' matrix or array you must pick an element type, e.g.
#' `new_class("my_matrix", class_double_matrix)`.
#'
#' @seealso [base_classes] and [base_s3_classes] for other classes that
#'   provide compatibility with base types and S3 classes.
#' @return S7 classes wrapping around base matrices and arrays.
#' @name base_arrays
#' @order 0
#' @examples
#' # Create an S7 class that extends a double matrix
#' Cov <- new_class("Cov", class_double_matrix)
#' Cov(matrix(c(1, 0.5, 0.5, 1), nrow = 2))
#'
#' # `class_matrix` and `class_array` dispatch on any element type
#' n_dim <- new_generic("n_dim", "x")
#' method(n_dim, class_array) <- function(x) length(dim(x))
#' n_dim(Cov(diag(3)))
NULL

valid_dimnames <- function(self) {
  dn <- dimnames(self)
  if (is.null(dn)) {
    TRUE
  } else if (!is.list(dn) || length(dn) != length(dim(self))) {
    FALSE
  } else {
    for (i in seq_along(dimnames(self))) {
      if (is.null(dn[[i]])) {
        next
      }
      if (!is.character(dn[[i]]) || length(dn[[i]]) != dim(self)[[i]]) {
        return(FALSE)
      }
    }
    TRUE
  }
}

validate_matrix <- function(self) {
  if (!is.matrix(self)) {
    # is.matrix() methods should only return TRUE if valid
    "is.matrix(self) is FALSE"
  } else if (
    !is.integer(dim(self)) || length(dim(self)) != 2L || !all(dim(self) >= 0L)
  ) {
    "dim(self) must be a non-negative integer vector of length 2"
  } else if (!valid_dimnames(self)) {
    "dimnames(self) must be NULL or a length 2 list of either NULL or a character vector of length equal to its corresponding dimension"
  }
}

validate_array <- function(self) {
  if (is.array(self)) {
    # is.array() methods should only return TRUE if valid
    return(invisible(NULL))
  }
  if (
    !is.integer(dim(self)) || length(dim(self)) == 0L || !all(dim(self) >= 0L)
  ) {
    return("dim(self) must be a non-empty non-negative integer vector")
  }
  if (!valid_dimnames(self)) {
    return(
      "dimnames(self) must be NULL or a list of either NULL or a character vector of length equal to its corresponding dimension"
    )
  }
  "is.array(self) is FALSE"
}

# Construct an element-typed matrix or array S3 class. The element type is
# kept out of the `class` vector (which mirrors the implicit S3 class of a
# bare object, e.g. `c("matrix", "array")`) so that structural inheritance
# still matches base objects; instead it is enforced by the validator.
new_base_matrix <- function(type) {
  is_type <- get(paste0("is.", type), envir = baseenv(), mode = "function")

  constructor <- function(
    .data,
    nrow = NULL,
    ncol = NULL,
    byrow = FALSE,
    dimnames = NULL
  ) {
    nrow <- nrow %||% NROW(.data)
    if (is.null(ncol)) {
      ncol <- NCOL(.data)
      if (length(.data) != (nrow * ncol)) {
        ncol <- length(.data) %/% nrow
      }
    }
    matrix(.data, nrow, ncol, byrow, dimnames)
  }
  formals(constructor)$.data <- call(type)

  out <- new_S3_class(
    c("matrix", "array"),
    constructor = constructor,
    validator = function(self) {
      validate_matrix(self) %||%
        if (!is_type(self)) {
          sprintf(
            "Underlying data must be <%s> not <%s>",
            type,
            base_class(self)
          )
        }
    }
  )
  out$desc <- sprintf("S3<%s matrix>", type)
  out
}

new_base_array <- function(type) {
  is_type <- get(paste0("is.", type), envir = baseenv(), mode = "function")

  constructor <- function(
    .data,
    dim = base::dim(.data) %||% length(.data),
    dimnames = base::dimnames(.data)
  ) {
    array(.data, dim, dimnames)
  }
  formals(constructor)$.data <- call(type)

  out <- new_S3_class(
    "array",
    constructor = constructor,
    validator = function(self) {
      validate_array(self) %||%
        if (!is_type(self)) {
          sprintf(
            "Underlying data must be <%s> not <%s>",
            type,
            base_class(self)
          )
        }
    }
  )
  out$desc <- sprintf("S3<%s array>", type)
  out
}

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_logical_matrix <- new_base_matrix("logical")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_integer_matrix <- new_base_matrix("integer")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_double_matrix <- new_base_matrix("double")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_complex_matrix <- new_base_matrix("complex")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_character_matrix <- new_base_matrix("character")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_raw_matrix <- new_base_matrix("raw")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_list_matrix <- new_base_matrix("list")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_logical_array <- new_base_array("logical")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_integer_array <- new_base_array("integer")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_double_array <- new_base_array("double")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_complex_array <- new_base_array("complex")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_character_array <- new_base_array("character")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_raw_array <- new_base_array("raw")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 1
class_list_array <- new_base_array("list")

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 2
class_matrix <- NULL

#' @export
#' @rdname base_arrays
#' @format NULL
#' @order 2
class_array <- NULL

# Defined onLoad because `new_union()` is not available at build time
on_load_define_matrix_array_classes <- function() {
  class_matrix <<- new_union(
    class_logical_matrix,
    class_integer_matrix,
    class_double_matrix,
    class_complex_matrix,
    class_character_matrix,
    class_raw_matrix,
    class_list_matrix
  )
  class_array <<- new_union(
    class_logical_array,
    class_integer_array,
    class_double_array,
    class_complex_array,
    class_character_array,
    class_raw_array,
    class_list_array
  )
}
