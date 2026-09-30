# Compare installed builds in fresh R sessions, alternating their order:
# Rscript bench/method-trace.R /tmp/baseline-lib /tmp/baseline-1.rds traced
# Rscript bench/method-trace.R /tmp/candidate-lib /tmp/candidate-1.rds traced
# Use "untraced" for builds without tracing support.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L, args[[3]] %in% c("traced", "untraced"))
.libPaths(c(args[[1]], .libPaths()))
load_ms <- system.time(library(S7))[["elapsed"]] * 1000

TraceClass := new_class(package = NULL)
obj <- TraceClass()

make_case <- function(name, dispatch_args, trace_generic, trace_method) {
  env <- new.env(parent = globalenv())
  generic <- new_generic(name = name, dispatch_args = dispatch_args)
  signature <- if (length(dispatch_args) == 1L) {
    TraceClass
  } else {
    list(TraceClass, TraceClass)
  }
  method(generic, signature) <- if (length(dispatch_args) == 1L) {
    function(x, ...) 1L
  } else {
    function(x, y, ...) 1L
  }
  env[[name]] <- generic
  table <- generic@methods
  if (length(dispatch_args) == 2L) {
    table <- table$TraceClass
  }
  if (trace_method) {
    suppressMessages(trace(
      "TraceClass",
      quote(NULL),
      print = FALSE,
      where = table
    ))
  }
  if (trace_generic) {
    suppressMessages(trace(name, quote(NULL), print = FALSE, where = env))
  }
  list(env = env, table = table)
}

exprs <- list()
states <- list(untraced = c(FALSE, FALSE))
if (args[[3]] == "traced") {
  states <- c(
    states,
    list(
      method = c(FALSE, TRUE),
      generic = c(TRUE, FALSE),
      both = c(TRUE, TRUE)
    )
  )
}
for (dispatch in list(single = "x", multiple = c("x", "y"))) {
  prefix <- if (length(dispatch) == 1L) "single" else "multiple"
  for (state in names(states)) {
    name <- paste(prefix, state, sep = "_")
    case <- make_case(
      name,
      dispatch,
      states[[state]][[1]],
      states[[state]][[2]]
    )
    assign(name, case$env[[name]])
    exprs[[name]] <- if (length(dispatch) == 1L) {
      substitute(FUN(obj), list(FUN = as.name(name)))
    } else {
      substitute(FUN(obj, obj), list(FUN = as.name(name)))
    }
  }
}
stopifnot(all(vapply(exprs, \(expr) identical(eval(expr), 1L), logical(1))))

if (args[[3]] == "traced") {
  installation <- make_case("installation", "x", FALSE, FALSE)
  install_generic_trace <- function() {
    suppressMessages(trace(
      "installation",
      quote(NULL),
      print = FALSE,
      where = installation$env
    ))
    suppressMessages(untrace("installation", where = installation$env))
    invisible(NULL)
  }
  install_method_trace <- function() {
    suppressMessages(trace(
      "TraceClass",
      quote(NULL),
      print = FALSE,
      where = installation$table
    ))
    suppressMessages(untrace("TraceClass", where = installation$table))
    invisible(NULL)
  }
  exprs$generic_trace_roundtrip <- quote(install_generic_trace())
  exprs$method_trace_roundtrip <- quote(install_method_trace())
}

result <- bench::mark(
  exprs = exprs,
  check = FALSE,
  filter_gc = FALSE,
  min_time = 0.5,
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
    load_ms = load_ms,
    timings = timings
  ),
  args[[2]]
)
print(timings, row.names = FALSE)
