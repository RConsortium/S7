test_that("native callers can retrieve S7 classes", {
  api <- local_native_api()
  Parent := new_class(properties = list(value = class_double))
  Child := new_class(parent = Parent)

  expect_identical(api$get_class(Parent(value = 1)), Parent)
  expect_identical(api$get_class(Child(value = 2)), Child)
  expect_null(api$get_class(1))
  expect_null(api$get_class(NULL))
})

test_that("native callers can use S7 objects with S4 parents", {
  api <- local_native_api()
  local_S4_classes()
  methods::setClass("NativeS4", slots = c(value = "numeric"))
  Child := new_class(parent = methods::getClass("NativeS4"))
  x <- Child(value = 2)

  expect_identical(api$get_class(x), Child)
  expect_identical(api$get_prop(x, "value"), 2)
  expect_null(api$get_class(methods::new("NativeS4")))
})

test_that("native property access uses S7 getters and storage", {
  api <- local_native_api()
  Parent := new_class(
    properties = list(
      value = class_double,
      names = class_character,
      optional = class_any,
      doubled = new_property(class_double, getter = \(self) self@value * 2),
      adjusted = new_property(
        class_double,
        getter = \(self) self@adjusted + 1,
        setter = function(self, value) {
          self@adjusted <- value
          self
        }
      )
    )
  )
  Child := new_class(parent = Parent)
  x <- Child(value = 2, names = "value", optional = NULL, adjusted = 3)

  expect_identical(api$get_prop(x, "value"), 2)
  expect_identical(api$get_prop(x, "names"), "value")
  expect_null(api$get_prop(x, "optional"))
  expect_identical(api$get_prop(x, "doubled"), 4)
  expect_identical(api$get_prop(x, "adjusted"), 4)
})

test_that("native property access reports errors", {
  api <- local_native_api()
  Foo := new_class(
    properties = list(
      value = class_double,
      broken = new_property(class_double, getter = \(self) stop("broken"))
    )
  )
  x <- Foo(value = 1)

  expect_snapshot(error = TRUE, api$get_prop(x, "missing"))
  expect_snapshot(error = TRUE, api$get_prop(x, "broken"))
  expect_snapshot(error = TRUE, api$get_prop(1, "value"))
  expect_snapshot(error = TRUE, api$get_prop(x, NULL))
  expect_snapshot(error = TRUE, api$get_prop(x, character()))
  expect_snapshot(error = TRUE, api$get_prop(x, c("value", "broken")))
  expect_snapshot(error = TRUE, api$get_prop(x, NA_character_))
})

test_that("the C++ header and registered functions expose the same API", {
  api <- local_native_api()
  Foo := new_class(
    properties = list(
      value = class_double,
      doubled = new_property(class_double, getter = \(self) self@value * 2)
    )
  )
  x <- Foo(value = 2)

  for (interface in c("cpp", "registered")) {
    expect_identical(api$get_class(x, interface), Foo)
    expect_identical(api$get_prop(x, "doubled", interface), 4)
  }
})
