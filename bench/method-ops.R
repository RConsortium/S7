# Compare installed builds in fresh R sessions, alternating their order:
# Rscript bench/method-ops.R /tmp/baseline-lib /tmp/baseline-1.rds
# Rscript bench/method-ops.R /tmp/candidate-lib /tmp/candidate-1.rds
# Rscript bench/method-ops.R /tmp/candidate-lib /tmp/candidate-2.rds
# Rscript bench/method-ops.R /tmp/baseline-lib /tmp/baseline-2.rds
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
.libPaths(c(args[[1]], .libPaths()))
library(S7)

Fallback := new_class(parent = class_double)
Registered := new_class(parent = class_double)
LogicalFallback := new_class(parent = class_logical)
LogicalRegistered := new_class(parent = class_logical)
method(`-`, list(Registered, class_missing)) <- \(e1, e2) 1
method(`+`, list(Registered, class_double)) <- \(e1, e2) 2
method(`!`, LogicalRegistered) <- \(e1) FALSE
identity_generic := new_generic("x")
method(identity_generic, Registered) <- \(x) 1

x <- Fallback(1)
y <- Registered(1)
flag <- LogicalFallback(TRUE)
registered_flag <- LogicalRegistered(TRUE)
long <- Fallback(rep(1, 10000))

exprs <- list(
  unary_plus_fallback = quote(+x),
  unary_minus_fallback = quote(-x),
  not_fallback = quote(!flag),
  binary_left_fallback = quote(x + 1),
  binary_right_fallback = quote(1 + x),
  long_vector_fallback = quote(-long),
  unary_registered = quote(-y),
  binary_registered = quote(y + 1),
  not_registered = quote(!registered_flag),
  generic_control = quote(identity_generic(y))
)
expected <- list(
  Fallback(1),
  Fallback(-1),
  LogicalFallback(FALSE),
  Fallback(2),
  Fallback(2),
  Fallback(rep(-1, 10000)),
  1,
  2,
  FALSE,
  1
)
values <- lapply(exprs, eval)
stopifnot(identical(unname(values), expected))
# Exclude class environments so outputs can be compared across R sessions.
outputs <- lapply(values, function(x) {
  list(class = class(x), value = if (S7_inherits(x)) S7_data(x) else x)
})

result <- bench::mark(
  exprs = exprs,
  check = FALSE,
  filter_gc = FALSE,
  min_time = 1,
  min_iterations = 1000
)
timings <- data.frame(
  case = names(exprs),
  median_us = as.numeric(result$median) * 1e6,
  mem_alloc = as.numeric(result$mem_alloc),
  iterations = result$n_itr,
  gc = result$n_gc
)
saveRDS(
  list(
    library = find.package("S7"),
    R = R.version.string,
    outputs = outputs,
    timings = timings
  ),
  args[[2]]
)
print(timings, row.names = FALSE)
