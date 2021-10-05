R7_object <<- new_class(
  name = "R7_object",
  parent = NULL,
  constructor = function() {
     out <- .Call(R7_object_)
     class(out) <- "R7_object"
     out
  }
)

lgl <<- new_class("logical", constructor = function(x = logical()) new_object(x))
int <<- new_class("integer", constructor = function(x = integer()) new_object(x))
dbl <<- new_class("double", constructor = function(x = double()) new_object(x))
num <<- new_class("numeric", constructor = function(x = numeric()) new_object(x))
cpl <<- new_class("complex", constructor = function(x = complex()) new_object(x))
chr <<- new_class("character", constructor = function(x = character()) new_object(x))
fct <<- new_class("factor", constructor = function(x = factor()) new_object(x))
raw <<- new_class("raw", constructor = function(x = raw()) new_object(x))
fn <<- new_class("function", constructor = function(x = function() NULL) new_object(x))
lst <<- new_class("list", constructor = function(x = list()) new_object(x))
df <<- new_class("data.frame", constructor = function(x = data.frame()) new_object(x))
env <<- new_class("environment", constructor = function(x = emptyenv()) new_object(x))

R7_generic <<- new_class(
  name = "R7_generic",
  properties = list(name = chr, methods = env, signature = new_property(name = "signature", getter = function(x) formals(x@.data))),
  parent = fn,
  constructor = function(name, signature, fun) {
    new_object(name = name, signature = signature, methods = new.env(parent = emptyenv(), hash = TRUE), .data = fun)
  }
)

R7_method <<- new_class(
  name = "R7_method",
  properties = list(generic = R7_generic, signature = lst, fun = fn),
  parent = fn,
  constructor = function(generic, signature, fun) {
    if (is.character(signature)) {
      signature <- list(signature)
    }
    new_object(generic = generic, signature = signature, .data = fun)
  }
)

R7_union <<- new_class(
  name = "R7_union",
  properties = list(classes = lst),
  validator = function(x) {
    for (val in x@classes) {
      if (!inherits(val, "R7_class")) {
        return(sprintf("All classes in an <R7_union> must be R7 classes:\n - <%s> is not an <R7_class>", class(val)[[1]]))
      }
    }
  },
  constructor = function(...) {
    classes <- list(...)
    for (i in seq_along(classes)) {
      if (is.character(classes[[i]])) {
        classes[[i]] <- class_get(classes[[i]])
      }
    }

    new_object(classes = classes)
  }
)

new_union <<- R7_union
