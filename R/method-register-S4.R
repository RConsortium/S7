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
  is_internal_generic(generic$name) && signature_has_S4_ancestor(signature)
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
