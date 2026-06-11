#' Use an environment as the base type of an S7 class
#'
#' @description
#' \ifelse{html}{
#' {\figure{lifecycle-experimental.svg}{options: alt='\[Experimental\]'}}}{\strong{\[Experimental\]
#' }}
#'
#' `class_environment` is the [base][base_classes] wrapper for environments.
#' Unlike all other R objects, environments have reference semantics, i.e., they
#' are modified in place. It's not clear what all the implications of this are
#' for S7, so we are marking the use of `class_environment` as experimental.
#'
#' Its use is subject to the following caveats:
#'
#' * [S7_data()] and `S7_data<-()` error, because swapping the underlying data
#'   would destroy the existing attributes.
#'
#' * The default [convert()] method errors when upcasting to an environment
#'   because stripping the subclass's properties would mutate `from` in place.
#'
#' @export
#' @examples
#' Counter := new_class(class_environment)
#' counter <- Counter()
#' counter$n <- 0L
#'
#' # Reference semantics: `copy` and `counter` are the same object, so
#' # mutating one is visible through the other.
#' copy <- counter
#' copy$n <- 10L
#' counter$n
class_environment <- NULL

check_not_environment <- function(object, fn, call = sys.call(-1L)) {
  if (!is.environment(object)) {
    return(invisible())
  }

  msg <- paste_c(
    sprintf("Can't call `%s` on an environment.\n", fn),
    "See ?class_environment for details."
  )
  stop2(msg, call = call)
}

# Define onload to avoid dependencies between files
on_load_define_environment <- function() {
  class_environment <<- new_base_class("environment")
}
