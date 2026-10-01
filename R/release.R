# Extra bullets for usethis::use_release_issue()
release_bullets <- function() {
  c(
    "Run the evolution compat lab, `Rscript tools/evolution/run.R --check`, and review changes in `tools/evolution/results.md`. If the recorded behavior changed, update `vignette(\"evolution\")` to match."
  )
}
