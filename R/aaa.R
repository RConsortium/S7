

new_function <- function(args = NULL,
                         body = call(`{`),
                         env = asNamespace("S7")) {
  as.function.default(c(args, body), env)
}
