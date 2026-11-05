args <- commandArgs(trailingOnly = TRUE)
tree <- args[[1]]
repo <- "/Users/tomasz/github/RConsortium/S7"
.libPaths(c(normalizePath(file.path(repo, "revdep/library.noindex/S7", tree)), .libPaths()))
library(S7)
cat("S7 ", as.character(packageVersion("S7")), "\n", sep = "")
# iAR's public generic shape: omitted x has an explicit NULL default.
f <- new_generic("f", "x", function(x = NULL, ...) S7_dispatch())
method(f, NULL) <- function(x, ...) "null method"
cat("f(): ")
print(tryCatch(f(), error = conditionMessage))
cat("f(NULL): ")
print(tryCatch(f(NULL), error = conditionMessage))
# mixtime's two dispatch arguments with an omitted defaulted second argument.
g <- new_generic("g", c("x", "cal"), function(x, cal = "default", ...) S7_dispatch())
method(g, list(class_any, class_any)) <- function(x, cal, ...) paste("cal=", cal)
cat("g(1): ")
print(tryCatch(g(1), error = conditionMessage))
cat("g(1, cal='default'): ")
print(tryCatch(g(1, cal = "default"), error = conditionMessage))
# ggplotplus' public package-boundary parent pattern.
library(ggplot2)
cat("ggplot2 ", as.character(packageVersion("ggplot2")), "\n", sep = "")
probe <- tryCatch(new_class("Probe", package = "probe", parent = ggplot2::class_ggplot), error = identity)
cat("direct class_ggplot parent: ")
print(if (inherits(probe, "error")) conditionMessage(probe) else "class construction succeeded")
probe_alias <- tryCatch(new_class("ProbeAlias", package = "probe", parent = new_external_class("ggplot2", "class_ggplot")), error = identity)
cat("explicit exported alias parent: ")
print(if (inherits(probe_alias, "error")) conditionMessage(probe_alias) else "class construction succeeded")
