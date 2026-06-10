register_S3_method <- function(
  generic,
  signature,
  method,
  envir = parent.frame(),
  call = sys.call(-1L)
) {
  sig <- signature[[1]]

  class <- switch(
    class_type(sig),
    `NULL` = "NULL",
    missing = stop2(
      "`class_missing` not supported for non-operator S3 generics.",
      call = NULL
    ),
    any = "default",
    S7_base = sig$class,
    S7 = S7_class_name(sig),
    S7_union = stop2("Unreachable", call = NULL),
    S7_S3 = sig$class[[1]],
    S4 = sig@className
  )

  if (is_local_s3_generic(generic)) {
    register_local_s3_method(generic, class, method)
  } else {
    # Register external generics in their own namespace
    external_generic <- get0(generic$name, envir = envir)
    if (is_external_generic(external_generic)) {
      envir <- asNamespace(external_generic$package)
    }
    registerS3method(generic$name, class, method, envir)
  }
}

# `registerS3method()` registers into the S3 methods table of
# `environment(generic)`, but `UseMethod()` dispatches using the table of
# `topenv(environment(generic))`. These are the same for package and global
# generics, but differ for a generic defined in a local environment.
is_local_s3_generic <- function(generic) {
  env <- environment(generic$generic)
  !is.null(env) && !identical(env, topenv(env))
}
register_local_s3_method <- function(generic, class, method) {
  dispatch_env <- topenv(environment(generic$generic))
  table <- dispatch_env[[".__S3MethodsTable__."]]
  if (is.null(table)) {
    table <- new.env(parent = baseenv())
    dispatch_env[[".__S3MethodsTable__."]] <- table
  }
  assign(paste(generic$name, class, sep = "."), method, envir = table)
  invisible(method)
}
