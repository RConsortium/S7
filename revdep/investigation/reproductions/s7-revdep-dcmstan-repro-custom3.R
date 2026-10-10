args <- commandArgs(trailingOnly = TRUE)
tree <- args[[1]]
repo <- "/Users/tomasz/github/RConsortium/S7"
.libPaths(c(normalizePath(file.path(repo, "revdep/library.noindex/S7", tree)), .libPaths()))
library(S7)
cat("S7 ", as.character(packageVersion("S7")), "\n", sep = "")
parent <- new_class("Parent", package = "probe", properties = list(model = new_property(class = class_character, default = "parent"), model_args = new_property(class = class_list, default = list())))
model_property <- new_property(class = class_character, setter = function(self, value) {
  if (!is.null(self@model)) stop("@model is read-only", call. = FALSE)
})
child <- new_class("Child", package = "probe", parent = parent, properties = list(model = model_property))
child_direct <- new_class("ChildDirect", package = "probe", parent = parent, properties = list(model = model_property), constructor = function(model = "child", model_args = list()) {
  new_object(parent(model = model, model_args = model_args))
})
cat("parent constructor formals/body:\n"); print(formals(parent)); print(body(parent))
cat("child constructor formals/body:\n"); print(formals(child)); print(body(child))
cat("child(model='child'):\n")
print(tryCatch(child(model = "child"), error = conditionMessage))
cat("custom direct constructor child_direct(model=child):\n")
print(tryCatch(child_direct(model = "child"), error = conditionMessage))
cat("child() then assignment:\n")
x <- child()
print(tryCatch({x@model <- "child"; x}, error = conditionMessage))
