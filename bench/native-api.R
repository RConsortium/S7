# Run from the repository root against an installed build in a fresh R session:
# Rscript bench/native-api.R /tmp/S7-library /tmp/native-api-1.rds
# Repeat in fresh sessions to assess variation between runs.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
.libPaths(c(args[[1]], .libPaths()))
library(S7)
stopifnot(
  normalizePath(find.package("S7")) == normalizePath(file.path(args[[1]], "S7"))
)

run_benchmark <- function(output) {
  build_dir <- tempfile("S7-native-bench-")
  dir.create(build_dir)
  withr::defer(unlink(build_dir, recursive = TRUE))
  stopifnot(file.copy("bench/native-api.c", build_dir))
  dll_path <- file.path(
    build_dir,
    paste0("s7nativebench", .Platform$dynlib.ext)
  )
  compilation <- callr::rcmd(
    "SHLIB",
    c("-o", dll_path, "native-api.c"),
    wd = build_dir,
    env = c(
      PKG_CPPFLAGS = paste0(
        "-I",
        shQuote(system.file("include", package = "S7", mustWork = TRUE))
      )
    )
  )
  dll <- dyn.load(dll_path)
  withr::defer(dyn.unload(dll_path))
  native_class <- getNativeSymbolInfo("native_class", dll)
  native_prop <- getNativeSymbolInfo("native_prop", dll)
  eval_call <- getNativeSymbolInfo("eval_call", dll)

  Example := new_class(
    properties = list(
      value = class_double,
      doubled = new_property(class_double, getter = \(self) self@value * 2)
    )
  )
  object <- Example(value = 2)
  batch <- 1000L

  # Cache functions, arguments, and complete calls before timing. Both R routes
  # reuse their calls; only the qualified route repeats the namespace lookup.
  cached_class <- as.call(list(S7::S7_class, object))
  cached_stored <- as.call(list(S7::prop, object, "value"))
  cached_getter <- as.call(list(S7::prop, object, "doubled"))
  qualified_class <- as.call(list(quote(S7::S7_class), object))
  qualified_stored <- as.call(list(quote(S7::prop), object, "value"))
  qualified_getter <- as.call(list(quote(S7::prop), object, "doubled"))
  eval_env <- baseenv()

  exprs <- list(
    class_native = quote(.Call(native_class, object, batch)),
    class_cached = quote(.Call(eval_call, cached_class, eval_env, batch)),
    class_qualified = quote(.Call(eval_call, qualified_class, eval_env, batch)),
    stored_native = quote(.Call(native_prop, object, "value", batch)),
    stored_cached = quote(.Call(eval_call, cached_stored, eval_env, batch)),
    stored_qualified = quote(.Call(
      eval_call,
      qualified_stored,
      eval_env,
      batch
    )),
    getter_native = quote(.Call(native_prop, object, "doubled", batch)),
    getter_cached = quote(.Call(eval_call, cached_getter, eval_env, batch)),
    getter_qualified = quote(.Call(
      eval_call,
      qualified_getter,
      eval_env,
      batch
    ))
  )
  expected <- rep(list(Example, 2, 4), each = 3)
  for (i in seq_along(exprs)) {
    stopifnot(identical(eval(exprs[[i]]), expected[[i]]))
  }

  result <- bench::mark(
    exprs = exprs,
    env = environment(),
    check = FALSE,
    filter_gc = FALSE,
    min_time = 1,
    min_iterations = 200
  )
  timings <- data.frame(
    case = names(exprs),
    median_us = as.numeric(result$median) * 1e6 / batch,
    batches = result$n_itr,
    gc = result$n_gc
  )
  saveRDS(
    list(
      library = find.package("S7"),
      R = R.version.string,
      platform = R.version$platform,
      batch_size = batch,
      compilation = compilation,
      timings = timings,
      batch_times = result$time
    ),
    output
  )
  print(timings, row.names = FALSE)
  invisible(timings)
}

run_benchmark(args[[2]])
