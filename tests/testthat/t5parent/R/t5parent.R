T5IdentityParent <- methods::setClass(
  "T5IdentityParent",
  slots = list(x = "numeric")
)

identity_parent_class <- function() {
  methods::getClass("T5IdentityParent", where = topenv())
}

new_identity_parent <- function(x) {
  methods::new(identity_parent_class(), x = x)
}
