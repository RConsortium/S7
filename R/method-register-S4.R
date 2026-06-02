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
