#!/usr/bin/env Rscript

# S7 evolution compat lab
#
# Empirically records what happens to a downstream package (evoB) when its
# upstream dependency (evoA) changes. For each scenario in scenarios.R:
#
#   1. install evoA 1.0.0, then install evoB (capturing install output)
#   2. run evoB's smoke test (baseline; should be OK)
#   3. install evoA 2.0.0 *without* rebuilding evoB
#   4. re-run the smoke test: what users see after upgrading only evoA
#   5. reinstall evoB against evoA 2.0.0 (capturing install output)
#   6. re-run the smoke test
#   7. (--check only) R CMD check evoB against evoA 2.0.0
#
# Usage, from the S7 package root:
#
#   Rscript tools/evolution/run.R [--check] [--s7=path] [scenario ...]
#
# * --check    also run R CMD check on evoB (slower, but classifies severity)
# * --s7=path  install S7 from `path` instead of the current directory
# * scenario   one or more scenario names to run (default: all)
#
# Output: tools/evolution/results.md, full logs in tools/evolution/logs/.

args <- commandArgs(trailingOnly = TRUE)
check_mode <- "--check" %in% args
s7_source <- sub("^--s7=", "", grep("^--s7=", args, value = TRUE))
if (length(s7_source) == 0) {
  s7_source <- "."
}
filter <- args[!grepl("^--", args)]

lab_dir <- "tools/evolution"
if (!file.exists(file.path(lab_dir, "scenarios.R"))) {
  stop("Run from the S7 package root: Rscript tools/evolution/run.R")
}
source(file.path(lab_dir, "scenarios.R"))

if (length(filter) > 0) {
  unknown <- setdiff(filter, names(scenarios))
  if (length(unknown) > 0) {
    stop("Unknown scenario(s): ", paste(unknown, collapse = ", "))
  }
  scenarios <- scenarios[filter]
}

# Helpers -------------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

r_bin <- file.path(R.home("bin"), "R")
rscript_bin <- file.path(R.home("bin"), "Rscript")

path_sep <- .Platform$path.sep

run_cmd <- function(bin, args, env = character(), dir = NULL) {
  if (!is.null(dir)) {
    old <- setwd(dir)
    on.exit(setwd(old))
  }
  output <- suppressWarnings(
    system2(bin, args, stdout = TRUE, stderr = TRUE, env = env)
  )
  list(
    status = attr(output, "status") %||% 0L,
    output = as.character(output)
  )
}

lib_env <- function(libs) {
  c(
    paste0("R_LIBS=", paste(libs, collapse = path_sep)),
    "R_TESTS=",
    "_R_CHECK_FORCE_SUGGESTS_=false"
  )
}

install_pkg <- function(path, dest_lib, libs) {
  run_cmd(
    r_bin,
    c(
      "CMD",
      "INSTALL",
      "--no-multiarch",
      "--no-byte-compile",
      "--no-docs",
      "-l",
      shQuote(dest_lib),
      shQuote(path)
    ),
    env = lib_env(libs)
  )
}

run_script <- function(lines, libs, dir) {
  path <- file.path(dir, "script.R")
  writeLines(lines, path)
  run_cmd(rscript_bin, shQuote(path), env = lib_env(libs))
}

check_pkg <- function(pkg_dir, libs, dir) {
  build <- run_cmd(
    r_bin,
    c("CMD", "build", "--no-build-vignettes", "--no-manual", shQuote(pkg_dir)),
    env = lib_env(libs),
    dir = dir
  )
  if (build$status != 0) {
    return(build)
  }

  tarball <- file.path(
    dir,
    grep(
      "^evoB.*\\.tar\\.gz$",
      list.files(dir),
      value = TRUE
    )[[1]]
  )
  run_cmd(
    r_bin,
    c(
      "CMD",
      "check",
      "--no-multiarch",
      "--no-manual",
      "-o",
      shQuote(file.path(dir, "check")),
      shQuote(tarball)
    ),
    env = lib_env(libs)
  )
}

write_package <- function(
  dir,
  name,
  version,
  imports,
  code,
  exports,
  tests = NULL,
  extra_ns = NULL
) {
  unlink(dir, recursive = TRUE)
  dir.create(file.path(dir, "R"), recursive = TRUE)

  writeLines(
    c(
      paste0("Package: ", name),
      "Title: Fixture for the S7 Evolution Compat Lab",
      paste0("Version: ", version),
      "Authors@R: person(\"S7\", \"Developers\", , \"s7@example.com\", role = c(\"aut\", \"cre\"))",
      "Description: A fixture package for the S7 evolution compatibility lab.",
      "License: GPL-3",
      paste0("Imports: ", paste(imports, collapse = ", ")),
      "Encoding: UTF-8"
    ),
    file.path(dir, "DESCRIPTION")
  )

  writeLines(
    c(
      "import(S7)",
      if (exports) 'exportPattern("^[a-zA-Z]")',
      extra_ns
    ),
    file.path(dir, "NAMESPACE")
  )

  writeLines(code, file.path(dir, "R", "code.R"))
  writeLines(
    c(
      ".onLoad <- function(...) S7::S7_on_load()",
      ".onUnload <- function(...) S7::S7_on_unload()",
      "S7::S7_on_build()"
    ),
    file.path(dir, "R", "zzz.R")
  )

  if (!is.null(tests)) {
    dir.create(file.path(dir, "tests"))
    writeLines(tests, file.path(dir, "tests", "smoke.R"))
  }

  invisible(dir)
}

classify <- function(res) {
  if (res$status != 0) {
    "ERROR"
  } else if (any(grepl("Warning", res$output))) {
    "WARNING"
  } else {
    "OK"
  }
}

classify_check <- function(res) {
  status <- grep("^Status: ", res$output, value = TRUE)
  if (length(status) == 1) {
    sub("^Status: ", "", status)
  } else if (res$status != 0) {
    "ERROR"
  } else {
    "???"
  }
}

excerpt <- function(res, n = 6) {
  interesting <- grep("Warning|Error|ERROR|WARNING", res$output)
  if (length(interesting) == 0) {
    return(character())
  }
  lines <- sort(unique(c(interesting, interesting + 1)))
  lines <- lines[lines <= length(res$output)]
  utils::head(res$output[lines], n)
}

# Setup ----------------------------------------------------------------------

work <- file.path(tempdir(), "S7-evolution-lab")
base_lib <- file.path(work, "base-lib")
dir.create(base_lib, recursive = TRUE, showWarnings = FALSE)

log_root <- file.path(lab_dir, "logs")
unlink(log_root, recursive = TRUE)

message("Installing S7 from ", normalizePath(s7_source))
s7_install <- install_pkg(normalizePath(s7_source), base_lib, base_lib)
if (s7_install$status != 0) {
  writeLines(s7_install$output)
  stop("Failed to install S7")
}
s7_desc <- read.dcf(file.path(base_lib, "S7", "DESCRIPTION"))[1, ]

# Run scenarios ---------------------------------------------------------------

run_scenario <- function(sc) {
  message("Scenario: ", sc$name)
  dir <- file.path(work, sc$name)
  unlink(dir, recursive = TRUE)
  lib <- file.path(dir, "lib")
  dir.create(lib, recursive = TRUE)
  libs <- c(lib, base_lib)

  a_dir <- file.path(dir, "evoA")
  b_dir <- file.path(dir, "evoB")

  stages <- list()

  write_package(a_dir, "evoA", "1.0.0", "S7", sc$a_v1, exports = TRUE)
  stages[["evoA 1.0.0: install"]] <- install_pkg(a_dir, lib, libs)

  write_package(
    b_dir,
    "evoB",
    "1.0.0",
    c("S7", "evoA"),
    sc$b,
    exports = FALSE,
    tests = sc$b_test,
    extra_ns = sc$b_ns
  )
  stages[["evoB install (v1)"]] <- install_pkg(b_dir, lib, libs)
  stages[["evoB test (v1)"]] <- run_script(sc$b_test, libs, dir)

  write_package(a_dir, "evoA", "2.0.0", "S7", sc$a_v2, exports = TRUE)
  stages[["evoA 2.0.0: install"]] <- install_pkg(a_dir, lib, libs)
  stages[["evoB test (v2, stale)"]] <- run_script(sc$b_test, libs, dir)

  stages[["evoB install (v2)"]] <- install_pkg(b_dir, lib, libs)
  stages[["evoB test (v2)"]] <- run_script(sc$b_test, libs, dir)

  if (check_mode) {
    stages[["evoB R CMD check (v2)"]] <- check_pkg(b_dir, libs, dir)
  }

  # Save full logs
  log_dir <- file.path(log_root, sc$name)
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  for (stage in names(stages)) {
    slug <- gsub("[^a-z0-9]+", "-", tolower(stage))
    writeLines(stages[[stage]]$output, file.path(log_dir, paste0(slug, ".log")))
  }

  stages
}

results <- lapply(scenarios, run_scenario)

# Report ----------------------------------------------------------------------

stage_status <- function(stages) {
  vapply(
    names(stages),
    function(nm) {
      if (grepl("check", nm)) {
        classify_check(stages[[nm]])
      } else {
        classify(stages[[nm]])
      }
    },
    character(1)
  )
}

report <- c(
  "# Evolution compat lab results",
  "",
  "Generated by `Rscript tools/evolution/run.R`. Do not edit by hand.",
  "",
  paste0("* Date: ", format(Sys.Date())),
  paste0("* S7 version: ", s7_desc[["Version"]]),
  paste0("* R version: ", getRversion()),
  paste0(
    "* Mode: ",
    if (check_mode) "install + test + R CMD check" else "install + test"
  ),
  "",
  "Stages: evoB is written against evoA 1.0.0. \"stale\" = evoA upgraded to",
  "2.0.0 but evoB not rebuilt (what a user sees after upgrading only evoA);",
  "\"v2\" = evoB rebuilt against evoA 2.0.0 (what R CMD check sees).",
  ""
)

for (name in names(results)) {
  stages <- results[[name]]
  status <- stage_status(stages)

  report <- c(
    report,
    paste0("## ", name),
    "",
    scenarios[[name]]$expect,
    "",
    "| Stage | Result |",
    "|-------|--------|",
    sprintf("| %s | %s |", names(status), status),
    ""
  )

  for (stage in names(stages)) {
    ex <- excerpt(stages[[stage]])
    if (length(ex) > 0) {
      report <- c(
        report,
        paste0("`", stage, "`:"),
        "",
        "```",
        ex,
        "```",
        ""
      )
    }
  }
}

writeLines(report, file.path(lab_dir, "results.md"))
message("Wrote ", file.path(lab_dir, "results.md"))
