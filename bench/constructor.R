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
# Methodology notes
# -----------------
# These are the traps that produced wrong answers while working on #723.
#
# * Each table is produced by a *single* `bench::mark()` call, listing every
#   expression at once. `mark()` interleaves and randomises the order of the
#   expressions it is given, so thermal drift, background load, and GC land on
#   all of them roughly equally. Timing each case in its own `mark()` call
#   forfeits that. Add new cases to the existing call, don't add a call.
#
# * Absolute timings drift by up to ~8% between R sessions, which is larger than
#   many effects worth chasing here. Compare within a run, or via two `--save`
#   runs made back to back on an idle machine. Never compare against a figure
#   pasted from an older session.
#
# * Everything here is at microsecond scale, which is the scale that decisions
#   actually get made at. Sub-microsecond primitives (a single attribute read,
#   say) are not covered, because `bench::mark()` appears to quantise somewhere
#   around 40ns.
#
# * `class_double` properties reject integers, so fixtures must use `as.double()`
#   or construction fails validation instead of being measured.
#
# * `pkgload::load_all()` defaults to `export_all = TRUE`, which puts S7's
#   internal `@` on the search path where it shadows base's primitive. Any
#   measurement of `@` is therefore optimistic about S7's overhead relative to an
#   installed package; for `@` specifically, install to a temporary library and
#   measure that instead.

pkgload::load_all(quiet = TRUE)

# helpers ---------------------------------------------------------------------

# Bytes retained per object, from the gc() delta over `n` live instances.
# Crude, but it is the only measure that catches an object holding a private
# copy of something shared: `object.size()` follows closure environments, so it
# reports numbers that are wildly too large.
bytes_per_object <- function(f, n = 5000) {
  # Ncells (cons cells) are 56 bytes on 64-bit, Vcells 8. Approximate; the
  # comparison between depths is the point, not the absolute figure.
  used <- function() {
    gc(full = TRUE)
    sum(gc()[, "used"] * c(56, 8))
  }
  invisible(f())
  before <- used()
  keep <- lapply(seq_len(n), function(i) f())
  after <- used()
  rm(keep)
  (after - before) / n
}

# A chain of `depth` classes, each adding nothing, so cost scales with the
# number of `new_object()` calls rather than the number of properties. Built
# programmatically, hence `new_class(name = )` rather than `:=`.
deep_class <- function(depth, abstract = FALSE) {
  class <- S7_object
  for (i in seq_len(depth)) {
    class <- new_class(
      name = paste0("Deep", i),
      parent = class,
      abstract = abstract,
      properties = list(x = class_double, y = class_double)
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

# fixtures --------------------------------------------------------------------

# Returns the expressions to benchmark plus the environment holding the classes
# they refer to. Every class is built here, once, so the expressions only
# construct; building inside them would time class definition too.
call_fixtures <- function() {
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

  # A custom constructor takes the stack-inspecting path in `new_object()`,
  # unlike the generated ones.
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

  obj5 <- Deep5(x = 1, y = 2)

  exprs <- list(
    depth1 = quote(Deep1(x = 1, y = 2)),
    depth5 = quote(Deep5(x = 1, y = 2)),
    depth10 = quote(Deep10(x = 1, y = 2)),
    oneshot5 = quote(OneShot(x = 1, y = 2)),
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
    prop_at = quote(obj5@x),
    prop_fn = quote(prop(obj5, "x"))
  )
  list(env = environment(), exprs = exprs)
}

# benchmarks ------------------------------------------------------------------

# Whole operations, at microsecond scale. One randomised bench::mark() call.
bench_calls <- function() {
  fix <- call_fixtures()
  res <- bench::mark(
    exprs = fix$exprs,
    env = fix$env,
    check = FALSE,
    filter_gc = FALSE,
    min_iterations = 200
  )
  data.frame(
    case = names(fix$exprs),
    us = round(as.numeric(res$median) * 1e6, 1),
    row.names = NULL
  )
}

# Per-instance memory, by hierarchy depth. Flat is correct: every instance
# should reference one shared class object. Growth of roughly a kilobyte per
# level means each instance carries a private copy of its class and every
# ancestor (see #723).
bench_memory <- function() {
  depths <- c(1, 5, 10, 20)
  # Deliberately not named `class`: binding an S7 class to that name shadows
  # base::class(), which is how a stray class() call inside new_object() once
  # recursed until it blew the C stack.
  bytes <- vapply(
    depths,
    function(d) {
      cls <- deep_class(d)
      bytes_per_object(function() cls(x = 1, y = 2))
    },
    numeric(1)
  )
  data.frame(depth = depths, bytes_per_object = round(bytes), row.names = NULL)
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
