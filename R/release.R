# Extra bullets for usethis::use_release_issue()
release_bullets <- function() {
  c(
    "Run the evolution compat lab, `Rscript tools/evolution/run.R --check`, and check that `tools/evolution/results.md` is unchanged. If it changed, cross-package behavior changed: update `vignette(\"evolution\")` to match."
  )
}
