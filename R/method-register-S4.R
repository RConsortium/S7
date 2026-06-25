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

should_register_S4_method <- function(generic, signature) {
  is_internal_S3_generic(generic) && signature_has_S4_ancestor(signature)
}

signature_has_S4_ancestor <- function(signature) {
  any(vlapply(signature, class_has_S4_ancestor))
}

class_has_S4_ancestor <- function(class) {
  switch(
    class_type(class),
    S4 = TRUE,
    S7 = S7_extends_S4(class),
    FALSE
  )
}

S3_generic_S4_signature <- function(generic) {
  if (!is_internal_S3_generic(generic)) {
    return(NULL)
  }

  generic <- methods::getGeneric(generic$name)
  if (is.null(generic) || !is_S4_generic(generic)) {
    return(NULL)
  }

  generic@signature
}

is_external_S3_generic <- function(generic) {
  is_external_generic(generic) &&
    identical(generic$dispatch_args, "__S3__")
}

is_internal_S3_generic <- function(generic) {
  if (is_S3_generic(generic)) {
    return(
      is_internal_generic(generic$name) &&
        identical(find_base_name(generic$generic), generic$name)
    )
  }

  is_external_S3_generic(generic) &&
    identical(generic$package, "base") &&
    is_internal_generic(generic$name)
}
