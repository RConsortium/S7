text <- class_new("text", parent = "character", constructor = function(text = character()) object_new(.data = text))
number <- class_new("number", parent = "numeric", constructor = function(x) object_new(.data = x))

range <- class_new("range",
  constructor = function(start, end) {
    object_new(start = start, end = end, length = accessor(function(x) x@end - x@start))
  },
  validator = function(x) {
    if (property(x, "end") < property(x, "start")) {
      "`end` must be greater than or equal to `start`"
    }
  },
  properties = list(start = "numeric", end = "numeric", property_new(name = "length", accessor = function(x) x@end - x@start))
)
