# Run with S7 0.2.2 and ggplot2 4.0.3 installed. S7 0.2.2 predates `:=`.
# The first argument is an installed ggforce 0.5.0 package directory.
# The original artifact was built on R 4.6.0 on 2026-04-26 at 05:29:06 UTC.
# Run from this directory. Keep the mapping unchanged: its missing class
# metadata is part of the saved ggforce artifact, not synthesized by this test.
library(S7)
stopifnot(packageVersion("S7") == "0.2.2")
ggforce_path <- commandArgs(trailingOnly = TRUE)[[1L]]
env <- new.env(parent = asNamespace("ggplot2"))
lazyLoad(file.path(ggforce_path, "R", "ggforce"), env)
mapping <- env$GeomArcBar$default_aes
stopifnot(
  identical(class(mapping), c("ggplot2::mapping", "uneval", "gg", "S7_object")),
  is.null(attr(mapping, "S7_class", exact = TRUE))
)
saveRDS(mapping, "legacy-ggforce-mapping.rds", version = 2)

Legacy <- new_class(
  name = "Legacy",
  parent = class_list,
  properties = list(label = class_character)
)
saveRDS(
  Legacy(.data = list(x = 1L), label = "old"),
  "legacy-object.rds",
  version = 2
)
