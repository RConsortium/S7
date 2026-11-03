# Compare str() output normalization and writing, with all candidates in one
# bench::mark() call. Output goes to an open null connection, excluding terminal
# rendering. Exact output bytes are checked before timing.
#
# Rscript bench/str.R /tmp/s7-str-bench.rds

str_writers <- list(
  current = function(out) {
    out <- sub("^ +", "", paste(out, collapse = "\n"))
    cat(" ", out, "\n", sep = "")
  },
  prefix_cat = function(out) {
    out <- sub("^ *", " ", paste(out, collapse = "\n"))
    cat(out, "\n", sep = "")
  },
  full_regex_cat = function(out) {
    out <- sub("^ *(.*)$", " \\1\n", paste(out, collapse = "\n"))
    cat(out, sep = "")
  },
  full_regex_pcre_cat = function(out) {
    out <- sub(
      "(?s)^ *(.*)$",
      " \\1\n",
      paste(out, collapse = "\n"),
      perl = TRUE
    )
    cat(out, sep = "")
  },
  prefix_write = function(out) {
    writeLines(sub("^ *", " ", paste(out, collapse = "\n")))
  },
  prefix_pcre_write = function(out) {
    writeLines(sub("^ *", " ", paste(out, collapse = "\n"), perl = TRUE))
  },
  vector_cat = function(out) {
    if (length(out) == 0L) {
      out <- ""
    }
    out[[1L]] <- sub("^ *", " ", out[[1L]])
    cat(out, sep = "\n")
  },
  vector_write = function(out) {
    if (length(out) == 0L) {
      out <- ""
    }
    out[[1L]] <- sub("^ *", " ", out[[1L]])
    writeLines(out)
  }
)

str_write_only <- list(
  cat_scalar = \(out) cat(out, sep = ""),
  write_scalar = \(out) writeLines(out, sep = ""),
  cat_lines = \(out) cat(out, sep = "\n"),
  write_lines = \(out) writeLines(out)
)

capture_bytes <- function(fun, out) {
  con <- rawConnection(raw(), "w")
  withr::defer(close(con))
  withr::with_output_sink(con, fun(out))
  rawConnectionValue(con)
}

bench_str <- function() {
  values <- list(
    scalar = TRUE,
    nested = list(flag = TRUE, values = 1:5, child = list(text = c("a", "b"))),
    long = setNames(as.list(seq_len(100)), paste0("item", seq_len(100)))
  )
  lines <- lapply(values, \(x) utils::capture.output(str(x, nest.lev = 1)))
  edge_cases <- list(
    character(),
    "",
    "   ",
    c("value", "  continuation", ""),
    c("  value", "  continuation"),
    c(" chr \"café\"", "  chr \"日本語\"")
  )
  for (out in c(lines, edge_cases)) {
    expected <- capture_bytes(str_writers$current, out)
    for (writer in str_writers) {
      stopifnot(identical(capture_bytes(writer, out), expected))
    }
  }

  exprs <- list()
  for (case in names(values)) {
    for (variant in names(str_writers)) {
      name <- paste("format_write", case, variant, sep = "/")
      exprs[[name]] <- substitute(
        str_writers[[variant]](lines[[case]]),
        list(variant = variant, case = case)
      )
    }

    normalized <- lines[[case]]
    normalized[[1L]] <- sub("^ *", " ", normalized[[1L]])
    complete <- paste0(paste(normalized, collapse = "\n"), "\n")
    expected <- charToRaw(complete)
    for (variant in names(str_write_only)) {
      out <- if (endsWith(variant, "scalar")) complete else normalized
      stopifnot(identical(
        capture_bytes(str_write_only[[variant]], out),
        expected
      ))
      name <- paste("write_only", case, variant, sep = "/")
      exprs[[name]] <- substitute(
        str_write_only[[variant]](out),
        list(variant = variant, out = out)
      )
    }

    for (variant in c(
      "current",
      "full_regex_cat",
      "prefix_write",
      "vector_write"
    )) {
      name <- paste("str_capture_write", case, variant, sep = "/")
      exprs[[name]] <- substitute(
        str_writers[[variant]](utils::capture.output(str(
          values[[case]],
          nest.lev = 1
        ))),
        list(variant = variant, case = case)
      )
    }
  }

  # This streaming comparison is valid for the scalar fixture, whose native
  # str() prefix follows the R-version convention. It does not normalize the
  # first line of arbitrary custom str() methods.
  stream_scalar <- function() {
    if (getRversion() >= "4.7.0") {
      cat(" ")
    }
    str(values$scalar, nest.lev = 1)
  }
  new_str <- getRversion() >= "4.7.0"
  stream_scalar_cached <- function() {
    if (new_str) {
      cat(" ")
    }
    str(values$scalar, nest.lev = 1)
  }
  stopifnot(identical(
    capture_bytes(\(out) stream_scalar(), NULL),
    capture_bytes(str_writers$current, lines$scalar)
  ))
  stopifnot(identical(
    capture_bytes(\(out) stream_scalar_cached(), NULL),
    capture_bytes(str_writers$current, lines$scalar)
  ))
  exprs[["stream/scalar/version_check"]] <- quote(stream_scalar())
  exprs[["stream/scalar/cached_version"]] <- quote(stream_scalar_cached())
  exprs[["version_check/scalar/compare"]] <- quote(getRversion() >= "4.7.0")

  con <- file(nullfile(), "w")
  withr::defer(close(con))
  result <- withr::with_output_sink(
    con,
    bench::mark(
      exprs = exprs,
      env = environment(),
      check = FALSE,
      filter_gc = FALSE,
      min_time = 0.3,
      min_iterations = 200
    )
  )
  keys <- strsplit(names(exprs), "/", fixed = TRUE)
  timings <- data.frame(
    group = vapply(keys, `[[`, "", 1L),
    case = vapply(keys, `[[`, "", 2L),
    variant = vapply(keys, `[[`, "", 3L),
    median_us = as.numeric(result$median) * 1e6,
    mem_alloc = as.numeric(result$mem_alloc),
    iterations = result$n_itr,
    gc = result$n_gc
  )
  list(
    R = R.version.string,
    platform = R.version$platform,
    bench_version = as.character(utils::packageVersion("bench")),
    output_lines = lengths(lines),
    timings = timings
  )
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) == 1L)
  result <- bench_str()
  saveRDS(result, args[[1L]])
  print(result$timings, row.names = FALSE)
}
