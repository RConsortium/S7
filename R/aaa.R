
new_function <- function(args, body, env) {
  as.function.default(c(args, body), env)
}
