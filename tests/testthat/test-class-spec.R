test_that("can work with S7 classes", {
  klass <- new_class("klass")
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "S7")
  expect_equal(class_dispatch(klass), c("klass", "S7_object"))
  expect_equal(class_register(klass), "klass")
  expect_equal(class_construct(klass), klass())
  expect_equal(class_desc(klass), "<klass>")
  expect_equal(class_deparse(klass), "klass")

  obj <- klass()
  expect_equal(obj_type(obj), "S7")
  expect_equal(obj_desc(obj), "<klass>")
  expect_equal(obj_dispatch(obj), c("klass", "S7_object"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("can work with S7 classes in packages", {
  klass <- new_class("klass", package = "pkg")
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "S7")
  expect_equal(class_dispatch(klass), c("pkg::klass", "S7_object"))
  expect_equal(class_register(klass), "pkg::klass")
  expect_equal(class_construct(klass), klass())
  expect_equal(class_desc(klass), "<pkg::klass>")
  expect_equal(class_deparse(klass), "pkg::klass")

  obj <- klass()
  expect_equal(obj_type(obj), "S7")
  expect_equal(obj_desc(obj), "<pkg::klass>")
  expect_equal(obj_dispatch(obj), c("pkg::klass", "S7_object"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("can work with unions", {
  text <- new_class("text", class_character)
  number <- new_class("number", class_double)
  klass <- new_union(text, number)
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "S7_union")
  expect_error(class_dispatch(klass), "Unsupported")
  expect_error(class_register(klass))
  expect_equal(class_construct(klass), text())
  expect_equal(class_desc(klass), "<text> or <number>")
  expect_equal(class_deparse(klass), "new_union(text, number)")

  # Can't have an instance of a union so no obj_ tests

  expect_equal(class_inherits(text("x"), klass), TRUE)
  expect_equal(class_inherits(number(1), klass), TRUE)
})

test_that("handles NULL", {
  expect_equal(as_class(NULL), NULL)

  expect_equal(class_type(NULL), "NULL")
  expect_equal(class_dispatch(NULL), "NULL")
  expect_equal(class_register(NULL), "NULL")
  expect_equal(class_construct(NULL), NULL)
  expect_equal(class_desc(NULL), "<NULL>")
  expect_equal(class_deparse(NULL), "NULL")

  expect_equal(obj_type(NULL), "base")
  expect_equal(obj_desc(NULL), "<NULL>")
  expect_equal(obj_dispatch(NULL), "NULL")
  expect_equal(class_inherits("x", NULL), FALSE)
  expect_equal(class_inherits(NULL, NULL), TRUE)
})

# base --------------------------------------------------------------------

test_that("can work with base types", {
  klass <- class_character
  expect_equal(class_type(klass), "S7_base")
  expect_equal(class_dispatch(klass), c("character", "S7_object"))
  expect_equal(class_register(klass), "character")
  expect_equal(class_desc(klass), "<character>")
  expect_equal(class_construct(klass, "x"), "x")
  expect_equal(class_deparse(klass), 'class_character')

  obj <- "x"
  expect_equal(obj_type(obj), "base")
  expect_equal(obj_desc(obj), "<character>")
  expect_equal(obj_dispatch(obj), "character")
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("class_inherits handles variation in class names", {
  expect_true(class_inherits(1, class_double))
  expect_false(class_inherits("x", class_double))

  expect_true(class_inherits(1L, class_numeric))
  expect_true(class_inherits(1, class_numeric))
  expect_false(class_inherits("x", class_numeric))

  expect_true(class_inherits(function() {}, class_function))
  expect_true(class_inherits(sum, class_function))
  expect_true(class_inherits(`[`, class_function))
  expect_false(class_inherits("x", class_function))
})

test_that("dispatch for base objects use underlying type", {
  expect_equal(obj_dispatch(1), "double")
  expect_equal(obj_dispatch(1L), "integer")

  expect_equal(obj_dispatch(matrix(1)), "double")
  expect_equal(obj_dispatch(matrix(1L)), "integer")

  expect_equal(obj_dispatch(array(1)), "double")
  expect_equal(obj_dispatch(array(1L)), "integer")

  expect_equal(obj_dispatch(function() {}), "function")
  expect_equal(obj_dispatch(sum), "function")
  expect_equal(obj_dispatch(`[`), "function")

  expect_equal(obj_dispatch(quote({})), "call")
})

# S3 ----------------------------------------------------------------------

test_that("can work with S3 classes", {
  klass <- new_S3_class(c("ordered", "factor"),
    constructor = function(.data = numeric(), levels) ordered(.data, levels)
  )
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "S7_S3")
  expect_equal(class_dispatch(klass), c("ordered", "factor", "S7_object"))
  expect_equal(class_register(klass), "ordered")
  expect_equal(class_desc(klass), "S3<ordered/factor>")
  expect_equal(class_construct(klass), ordered(numeric()))
  expect_equal(class_deparse(klass), 'new_S3_class(c("ordered", "factor"))')

  obj <- ordered(integer())
  expect_equal(obj_type(obj), "S3")
  expect_equal(obj_desc(obj), "S3<ordered/factor>")
  expect_equal(obj_dispatch(obj), c("ordered", "factor"))
  expect_equal(class_inherits(obj, klass), TRUE)
  expect_equal(class_inherits(factor(), klass), FALSE)
})

test_that("can work with S7 classes that extend S3 classes", {
  Date <- new_S3_class("Date", constructor = function(.data = numeric()) .Date(.data))
  Date2 <- new_class("Date2", parent = Date, properties = list(x = class_numeric))

  expect_equal(class_type(Date2), "S7")
  expect_equal(class_dispatch(Date2), c("Date2", "Date", "S7_object"))
  expect_equal(class_register(Date2), "Date2")

  obj <- Date2(x = 1)
  expect_equal(obj_type(obj), "S7")
  expect_equal(obj_desc(obj), "<Date2>")
  expect_equal(obj_dispatch(obj), c("Date2", "Date", "S7_object"))
  expect_equal(class_inherits(.Date(1), Date), TRUE)
  expect_equal(class_inherits(obj, Date), TRUE)
  expect_equal(class_inherits(obj, Date2), TRUE)
})

# S4 ----------------------------------------------------------------------

test_that("can work with S4 classes", {
  on.exit(S4_remove_classes(c("Foo1", "Foo2", "Foo3", "Foo4")))

  methods::setClass("Foo1", contains = "character")
  methods::setClass("Foo2", contains = "Foo1")
  methods::setClass("Foo3", slots = list(x = "numeric"))
  methods::setClass("Foo4", contains = c("Foo2", "Foo3"))

  klass <- methods::getClass("Foo4")

  expect_equal(class_type(klass), "S4")
  expect_equal(class_dispatch(klass), c("S4/S7::Foo4", "S4/S7::Foo2", "S4/S7::Foo3", "S4/S7::Foo1", "character"))
  expect_equal(class_register(klass), "S4/S7::Foo4")
  expect_s4_class(class_construct(klass, 1, x = 2), "Foo4")
  expect_equal(class_desc(klass), "S4<Foo4>")
  expect_equal(class_deparse(klass), "Foo4")

  obj <- methods::new(klass, 1, x = 2)
  expect_equal(obj_type(obj), "S4")
  expect_equal(obj_desc(obj), "S4<Foo4>")
  expect_equal(obj_dispatch(obj), class_dispatch(klass))
  expect_equal(class_inherits(obj, klass), TRUE)
})

# input validation -------------------------------------------------------------

test_that("as_class gives informative errors", {
  expect_snapshot(error = TRUE, {
    as_class("foo")
    as_class(TRUE)
  })
})
