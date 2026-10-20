args <- commandArgs(trailingOnly = TRUE)
tree <- args[[1]]
repo <- "/Users/tomasz/github/RConsortium/S7"
.libPaths(c(normalizePath(file.path(repo, "revdep/library.noindex/S7", tree)), .libPaths()))
library(S7)
cat("S7 ", as.character(packageVersion("S7")), "\n", sep = "")
Parent <- new_class("Parent", package = "probe", properties = list(
  model = new_property(class = class_character, default = "parent"),
  model_args = new_property(class = class_list, default = quote(list()))
))
model_property <- new_property(class = class_character, setter = function(self, value) {
  if (!is.null(self@model)) stop("@model is read-only", call. = FALSE)
})
Child <- new_class("Child", package = "probe", parent = Parent, properties = list(model = model_property))
cat("generated Child constructor body:\n")
print(body(Child))
cat("Child(model='child'): ")
obj <- tryCatch(Child(model = "child"), error = conditionMessage)
print(if (inherits(obj, "S7_object")) paste("success, model=", obj@model) else obj)
if (packageVersion("S7") >= "0.2.2.9000") {
  ChildSafe <- new_class("ChildSafe", package = "probe", parent = Parent,
    properties = list(model = model_property),
    constructor = function(model = "child", model_args = list()) {
      new_object(Parent(model = model, model_args = model_args))
    }
  )
  cat("custom parent-once constructor: ")
  safe <- ChildSafe(model = "child")
  print(paste("success, model=", safe@model))
  cat("post-construction mutation: ")
  print(tryCatch({safe@model <- "other"; "unexpectedly changed"}, error = conditionMessage))
}
