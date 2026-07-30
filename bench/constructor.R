# Constructor, validation, and memory benchmarks for S7 (#723).
#
# Usage
# -----
# Measure the current source tree:
#
#     Rscript bench/constructor.R
#
# Compare two versions. Save a run from each, then diff them:
#
#     git stash && Rscript bench/constructor.R --save=/tmp/before.rds
#     git stash pop && Rscript bench/constructor.R --save=/tmp/after.rds
#     Rscript bench/constructor.R --compare=/tmp/before.rds,/tmp/after.rds
#
# Run a subset with --only=calls,memory (default: both).
#
pkgload::load_all(quiet = TRUE)

# helpers ---------------------------------------------------------------------

# A chain of `depth` classes. By default each level adds nothing, so cost scales
# with the number of `new_object()` calls rather than the number of properties.
# With `add_property = TRUE`, each level adds one uniquely named property.
# Built programmatically, hence `new_class(name = )` rather than `:=`.
deep_class <- function(depth, abstract = FALSE, add_property = FALSE) {
  class <- S7_object
  for (i in seq_len(depth)) {
    properties <- if (add_property) {
      setNames(list(class_double), paste0("x", i))
    } else {
      list()
    }
    class <- new_class(
      name = paste0("Deep", i),
      parent = class,
      abstract = abstract,
      properties = properties
    )
  }
  class
}

# One class with `n` base type properties, so cost scales with property count.
wide_class <- function(n) {
  props <- rep(list(class_double), n)
  if (n > 0) {
    names(props) <- paste0("p", seq_len(n))
  }
  new_class(name = paste0("Wide", n), properties = props)
}

wide_args <- function(n) {
  setNames(as.list(as.double(seq_len(n))), paste0("p", seq_len(n)))
}

# benchmarks ------------------------------------------------------------------

# Whole operations, at microsecond scale. Every class is built once before the
# single randomised `bench::mark()` call, so class definition is not timed.
bench_calls <- function() {
  Deep1 <- deep_class(1)
  Deep5 <- deep_class(5)
  Deep10 <- deep_class(10)

  Wide0 <- wide_class(0)
  Wide2 <- wide_class(2)
  Wide10 <- wide_class(10)
  Wide50 <- wide_class(50)
  args10 <- wide_args(10)
  args50 <- wide_args(50)

  WithSetter <- new_class(
    name = "WithSetter",
    properties = list(
      x = class_double,
      y = new_property(
        class_double,
        setter = function(self, value) {
          attr(self, "y") <- value
          self
        }
      )
    )
  )

  Custom <- new_class(
    name = "Custom",
    properties = list(x = class_double),
    constructor = function(x = 0) new_object(S7_object(), x = x)
  )

  # Abstract ancestors mean one `new_object()` call does all the work, so this
  # bounds what collapsing a chain of generated constructors could buy.
  OneShot <- new_class(
    name = "OneShot",
    parent = deep_class(5, abstract = TRUE)
  )

  obj5 <- Deep5()
  obj2 <- Wide2(p1 = 1, p2 = 2)

  exprs <- list(
    depth1 = quote(Deep1()),
    depth5 = quote(Deep5()),
    depth10 = quote(Deep10()),
    oneshot5 = quote(OneShot()),
    wide0 = quote(Wide0()),
    wide2 = quote(Wide2(p1 = 1, p2 = 2)),
    wide10 = quote(do.call(Wide10, args10)),
    wide50 = quote(do.call(Wide50, args50)),
    setter = quote(WithSetter(x = 1, y = 2)),
    custom = quote(Custom(x = 1)),
    validate5 = quote(validate(obj5)),
    # The pieces the constructor path is built from, for attributing a change to
    # a particular step. All are comfortably above the resolution floor.
    dispatch = quote(class_dispatch(Deep5)),
    inherits = quote(S7_inherits(obj5)),
    prop_at = quote(obj2@p1),
    prop_fn = quote(prop(obj2, "p1"))
  )

  res <- bench::mark(
    exprs = exprs,
    env = environment(),
    check = FALSE,
    filter_gc = FALSE,
    min_iterations = 200
  )
  data.frame(
    case = names(exprs),
    us = round(as.numeric(res$median) * 1e6, 1),
    row.names = NULL
  )
}

# Per-instance memory, by hierarchy depth. Flat is correct: every instance
# should reference one shared class object.
bench_memory <- function() {
  depths <- c(1, 5, 10, 20)
  bytes <- vapply(
    depths,
    function(d) {
      Class <- deep_class(d)
      # The first value includes the shared class graph; the second includes
      # only the new object's contribution.
      as.numeric(lobstr::obj_sizes(Class(), Class())[[2]])
    },
    numeric(1)
  )
  data.frame(depth = depths, bytes_per_object = round(bytes))
}

# reporting -------------------------------------------------------------------

all_benchmarks <- c("calls", "memory")

run_all <- function(only = all_benchmarks) {
  out <- list()
  if ("calls" %in% only) {
    out$calls <- bench_calls()
  }
  if ("memory" %in% only) {
    out$memory <- bench_memory()
  }
  out
}

print_run <- function(run) {
  for (name in names(run)) {
    cat("\n== ", name, " ==\n", sep = "")
    print(run[[name]], row.names = FALSE)
  }
  invisible(run)
}

# Both runs must come from back-to-back invocations; see the drift note above.
compare_runs <- function(before, after) {
  for (name in intersect(names(before), names(after))) {
    a <- before[[name]]
    b <- after[[name]]
    key <- names(a)[[1]]
    value <- names(a)[[2]]
    merged <- merge(a, b, by = key, suffixes = c("_before", "_after"))
    from <- merged[[paste0(value, "_before")]]
    to <- merged[[paste0(value, "_after")]]
    merged$change <- sprintf("%+.0f%%", 100 * (to - from) / from)
    cat("\n== ", name, " ==\n", sep = "")
    print(merged, row.names = FALSE)
  }
  invisible(NULL)
}

# main ------------------------------------------------------------------------

arg_value <- function(args, flag) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(hit) == 0) NULL else sub(paste0("^", flag, "="), "", hit[[1]])
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  compare <- arg_value(args, "--compare")
  if (!is.null(compare)) {
    paths <- strsplit(compare, ",", fixed = TRUE)[[1]]
    stopifnot(length(paths) == 2)
    compare_runs(readRDS(paths[[1]]), readRDS(paths[[2]]))
    return(invisible())
  }

  only <- arg_value(args, "--only")
  only <- if (is.null(only)) {
    all_benchmarks
  } else {
    strsplit(only, ",", fixed = TRUE)[[1]]
  }

  run <- run_all(only)
  print_run(run)

  save <- arg_value(args, "--save")
  if (!is.null(save)) {
    saveRDS(run, save)
    cat("\nsaved to ", save, "\n", sep = "")
  }
  invisible(run)
}

if (!interactive()) {
  main()
}
