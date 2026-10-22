# Property read benchmarks for S7: the cost of `@` versus `prop()` and
# `attr()`, as the class chain gets deeper.
#
# `x@name` reaches `@.S7_object` through S3 dispatch on `class(x)`, so every
# inheritance level is one method-table miss. Run before and after a change in
# separate R sessions:
#
#     git stash && Rscript bench/property-access.R
#     git stash pop && Rscript bench/property-access.R
#
pkgload::load_all(quiet = TRUE)

# A chain of `depth` classes with one property on the root. Classes belong to a
# package, as they would in real use. Built programmatically, hence
# `new_class(name = )` rather than `:=`.
deep_instance <- function(depth) {
  class <- new_class(
    name = paste0("Prop", depth, "_1"),
    package = "S7bench",
    properties = list(x = class_double)
  )
  for (i in seq_len(depth - 1) + 1) {
    class <- new_class(
      name = paste0("Prop", depth, "_", i),
      parent = class,
      package = "S7bench"
    )
  }
  class(x = 1)
}

# Batch 1000 reads per iteration so each timing is in microseconds.
reads <- function(expr) {
  expr <- substitute(expr)
  eval(bquote(function(obj) {
    for (i in 1:1000) {
      .(expr)
    }
  }))
}
read_at <- reads(obj@x)
read_prop <- reads(prop(obj, "x"))
read_attr <- reads(attr(obj, "x", exact = TRUE))

for (depth in c(2, 5, 8)) {
  obj <- deep_instance(depth)
  cat(
    "\ndepth ",
    depth,
    " (class vector length ",
    length(class(obj)),
    ")\n",
    sep = ""
  )
  res <- bench::mark(
    `@` = read_at(obj),
    `prop()` = read_prop(obj),
    `attr()` = read_attr(obj),
    min_iterations = 200,
    check = FALSE
  )
  print(res[c("expression", "min", "median", "mem_alloc")])
}
