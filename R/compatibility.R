# Where needed, attach an environment containing @ that works with S7
activate_backward_compatiblility <- function() {
  if (getRversion() < "4.3.0" && !"S7_at" %in% search()) {
    args <- list(list("@" = `@`), name = "S7_at", warn.conflicts = FALSE)
    do.call("attach", args)
  }
  invisible()
}

# The masking S7 performs deliberately: `@` over base (on R < 4.3.0, where
# S7 exports its own `@`), and `:=` over rlang and data.table.
s7_expected_masks <- list(
  base = "@",
  rlang = ":=",
  data.table = ":="
)

# Re-emit the conflict report that library() would have produced (see
# checkConflicts() in base's library()), minus S7's expected masks. Used when
# `.conflicts.OK` makes library() skip its report, which is all-or-nothing.
report_conflicts <- function(pkgname) {
  # A user-configured conflicts.policy takes over conflict handling in
  # library(); don't second-guess it.
  if (!is.null(getOption("conflicts.policy"))) {
    return(invisible())
  }

  sp <- search()
  lib.pos <- match(paste0("package:", pkgname), sp)
  ob <- names(as.environment(lib.pos))
  is_fun <- function(names, pos) {
    vapply(names, exists, NA, where = pos, mode = "function", inherits = FALSE)
  }
  masked_msg <- get(".maskedMsg", envir = baseenv())

  first <- TRUE
  for (i in setdiff(
    seq_along(sp),
    c(lib.pos, match(c("Autoloads", "CheckExEnv"), sp, 0L))
  )) {
    same <- intersect(names(as.environment(i)), ob)
    same <- setdiff(same, s7_expected_masks[[sub("^package:", "", sp[i])]])
    same <- same[!startsWith(same, ".__")]
    # Like library(), only report bindings of the same kind whose values
    # actually differ.
    same <- same[is_fun(same, i) == is_fun(same, lib.pos)]
    same <- same[
      vapply(
        same,
        \(nm) !identical(get(nm, pos = i), get(nm, pos = lib.pos)),
        NA
      )
    ]
    if (length(same) == 0L) {
      next
    }

    if (first) {
      first <- FALSE
      packageStartupMessage(
        gettextf(
          "\nAttaching package: %s\n",
          sQuote(pkgname),
          domain = "R-base"
        ),
        domain = NA
      )
    }
    packageStartupMessage(
      masked_msg(sort(same), pkg = sQuote(sp[i]), by = i < lib.pos),
      domain = NA
    )
  }

  invisible()
}

search_has_bind_conflict <- function(pkgname) {
  pkg <- paste0("package:", pkgname)
  env <- as.environment(pkg)
  bind <- env[[":="]]
  where <- setdiff(search(), c(pkg, "Autoloads", "CheckExEnv"))

  for (pos in where) {
    other <- as.environment(pos)
    if (!exists(":=", envir = other, inherits = FALSE)) {
      next
    }

    other_bind <- other[[":="]]
    if (is.function(other_bind) && !identical(other_bind, bind)) {
      return(TRUE)
    }
  }

  FALSE
}

# conflictRules() has been in base since R 3.6.0, so it is always available
# given our R >= 4.2.0 requirement.
activate_bind_compatibility <- function() {
  for (package in c("data.table", "rlang")) {
    rule <- conflictRules(package)
    conflictRules(
      package,
      mask.ok = rule$mask.ok,
      exclude = union(rule$exclude, ":=")
    )
  }

  # Declare S7's expected masks so that a strict conflicts.policy (which
  # errors on undeclared conflicts and ignores `.conflicts.OK`) still lets
  # S7 attach. library() reads conflictRules() before loading the namespace,
  # so this only takes effect once S7's namespace is already loaded (e.g.
  # imported by another package); attaching S7 cold under a strict policy
  # requires the user to declare the rules, as that policy intends. Don't
  # override rules the user has already declared.
  rule <- conflictRules("S7")
  if (is.null(rule$mask.ok)) {
    conflictRules("S7", mask.ok = s7_expected_masks, exclude = rule$exclude)
  }

  invisible()
}

#' @aliases @
#' @usage NULL
#' @rawNamespace if (getRversion() < "4.3.0") export(`@`)
#' @name prop
NULL

if (getRversion() < "4.3.0") {
  `@` <- function(object, name) {
    if (inherits(object, "S7_object")) {
      name <- as.character(substitute(name))
      prop(object, name)
    } else {
      name <- substitute(name)
      do.call(base::`@`, list(object, name))
    }
  }
}
