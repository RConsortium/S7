register_S4_method <- function(
  generic,
  signature,
  method,
  env = parent.frame(),
  call = sys.call(-1L)
) {
  S4_env <- topenv(env)
  S4_signature <- lapply(signature, S4_class, S4_env = S4_env, call = call)
  methods::setMethod(generic, S4_signature, method, where = S4_env)
}

S4_class <- function(x, S4_env, call = sys.call(-1L)) {
  switch(
    class_type(x),
    `NULL` = "NULL",
    missing = "missing",
    any = "ANY",
    S7_base = base_to_S4(x$class),
    S4 = x,
    S7 = S4_registered_class(x, S4_env = S4_env, call = call),
    S7_S3 = S4_registered_class(x, S4_env = S4_env, call = call),
    S7_union = stop2(
      "Internal error: union should be flattened upstream.",
      call = NULL
    )
  )
}

# S4 dispatch uses `class()` to find a method, but `class(1.5)` is "numeric",
# not "double", so registering under "double" silently misses real doubles.
# Mapping to "numeric" catches doubles but also matches integers too. There's
# no clean S4 way to say "doubles only" and this seems likely to be what
# people want.
base_to_S4 <- function(class) {
  switch(class, double = "numeric", class)
}

S4_registered_class <- function(
  x,
  S4_env,
  call = sys.call(-1L)
) {
  class <- tryCatch(
    methods::getClass(class_register(x), where = S4_env),
    error = function(err) NULL
  )
  if (is.null(class)) {
    msg <- sprintf(
      "Class has not been registered with S4; please call S4_register(%s).",
      class_deparse(x)
    )
    stop2(msg, call = call)
  }
  class
}
