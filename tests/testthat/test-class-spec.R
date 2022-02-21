test_that("can work with R7 classes", {
  klass <- new_class("klass")
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "R7")
  expect_equal(class_dispatch(klass), c("klass", "R7_object", "ANY"))
  expect_equal(class_register(klass), "klass")
  expect_equal(class_construct(klass), klass())
  expect_equal(class_desc(klass), "<klass>")
  expect_equal(class_deparse(klass), "klass")

  obj <- klass()
  expect_equal(obj_type(obj), "R7")
  expect_equal(obj_desc(obj), "<klass>")
  expect_equal(obj_dispatch(obj), c("klass", "R7_object", "ANY"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("can work with unions", {
  text <- new_class("text", character)
  number <- new_class("number", double)
  klass <- new_union(text, number)
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "R7_union")
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
  expect_equal(class_dispatch(NULL), c("NULL", "ANY"))
  expect_equal(class_register(NULL), "NULL")
  expect_equal(class_construct(NULL), NULL)
  expect_equal(class_desc(NULL), "<NULL>")
  expect_equal(class_deparse(NULL), "NULL")

  expect_equal(obj_type(NULL), "base")
  expect_equal(obj_desc(NULL), "<NULL>")
  expect_equal(obj_dispatch(NULL), c("NULL", "ANY"))
  expect_equal(class_inherits("x", NULL), FALSE)
  expect_equal(class_inherits(NULL, NULL), TRUE)
})

# base --------------------------------------------------------------------

test_that("can work with base types", {
  expect_equal(as_class("character"), base_classes$character)
  expect_equal(as_class(character), base_classes$character)
  expect_equal(as_class(double), base_classes$double)

  klass <- as_class("character")
  expect_equal(class_type(klass), "R7_base")
  expect_equal(class_dispatch(klass), c("R7_object", "character", "ANY"))
  expect_equal(class_register(klass), "character")
  expect_equal(class_desc(klass), "<character>")
  expect_equal(class_construct(klass, "x"), "x")
  expect_equal(class_deparse(klass), '"character"')

  obj <- "x"
  expect_equal(obj_type(obj), "base")
  expect_equal(obj_desc(obj), "<character>")
  expect_equal(obj_dispatch(obj), c("character", "ANY"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("class_inherits handles variation in class names", {
  expect_true(class_inherits(1, base_classes$double))
  expect_false(class_inherits("x", base_unions$double))

  expect_true(class_inherits(1L, base_unions$numeric))
  expect_true(class_inherits(1, base_unions$numeric))
  expect_false(class_inherits("x", base_unions$numeric))

  expect_true(class_inherits(function() {}, base_classes$`function`))
  expect_true(class_inherits(sum, base_classes$`function`))
  expect_true(class_inherits(`[`, base_classes$`function`))
  expect_false(class_inherits("x", base_classes$`function`))
})

test_that("can get class from base constructor", {
  expect_equal(as_class(character), base_classes$character)
  expect_equal(as_class(`function`), base_classes$`function`)

  expect_snapshot_error(as_class(mean))
})

# S3 ----------------------------------------------------------------------

test_that("can work with S3 classes", {
  klass <- new_S3_class(c("ordered", "factor"),
    constructor = function(.data = numeric(), levels) ordered(.data, levels)
  )
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "R7_S3")
  expect_equal(class_dispatch(klass), c("R7_object", "ordered", "factor", "ANY"))
  expect_equal(class_register(klass), "ordered")
  expect_equal(class_desc(klass), "S3<ordered/factor>")
  expect_equal(class_construct(klass), ordered(numeric()))
  expect_equal(class_deparse(klass), 'new_S3_class(c("ordered", "factor"))')

  obj <- ordered(integer())
  expect_equal(obj_type(obj), "S3")
  expect_equal(obj_desc(obj), "S3<ordered/factor>")
  expect_equal(obj_dispatch(obj), c("ordered", "factor", "ANY"))
  expect_equal(class_inherits(obj, klass), TRUE)
  expect_equal(class_inherits(factor(), klass), FALSE)
})

test_that("can work with R7 classes that extend S3 classes", {
  Date <- new_S3_class("Date", constructor = function(.data = numeric()) .Date(.data))
  Date2 <- new_class("Date2", parent = Date, properties = list(x = "numeric"))

  expect_equal(class_type(Date2), "R7")
  expect_equal(class_dispatch(Date2), c("Date2", "R7_object", "Date", "ANY"))
  expect_equal(class_register(Date2), "Date2")

  obj <- Date2(x = 1)
  expect_equal(obj_type(obj), "R7")
  expect_equal(obj_desc(obj), "<Date2>")
  expect_equal(obj_dispatch(obj), c("Date2", "R7_object", "Date", "ANY"))
  expect_equal(class_inherits(.Date(1), Date), TRUE)
  expect_equal(class_inherits(obj, Date), TRUE)
  expect_equal(class_inherits(obj, Date2), TRUE)
})

# S4 ----------------------------------------------------------------------

test_that("can work with S4 classes", {
  methods::setClass("Range", slots = c(start = "numeric", end = "numeric"))
  klass <- methods::getClass("Range")

  expect_equal(class_type(klass), "S4")
  expect_equal(class_dispatch(klass), c("Range", "ANY"))
  expect_equal(class_register(klass), "Range")
  expect_s4_class(class_construct(klass, start = 1, end = 2), "Range")
  expect_equal(class_desc(klass), "S4<Range>")
  expect_equal(class_deparse(klass), "Range")

  obj <- methods::new(klass, start = 1, end = 1)
  expect_equal(obj_type(obj), "S4")
  expect_equal(obj_desc(obj), "S4<Range>")
  expect_equal(obj_dispatch(obj), c("Range", "ANY"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("can work with S4 subclasses", {
  methods::setClass("Foo1", slots = c(start = "numeric", end = "numeric"))
  methods::setClass("Foo2", contains = "Foo1")
  klass <- methods::getClass("Foo2")

  expect_equal(class_type(klass), "S4")
  expect_equal(class_dispatch(klass), c("Foo2", "Foo1", "ANY"))
  expect_equal(class_register(klass), "Foo2")

  obj <- methods::new(klass, start = 1, end = 1)
  expect_equal(obj_dispatch(obj), c("Foo2", "Foo1", "ANY"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("can work with S4 subclasses of base classes", {
  methods::setClass("Foo3", contains = "character")
  klass <- methods::getClass("Foo3")

  expect_equal(class_type(klass), "S4")
  expect_equal(class_dispatch(klass), c("Foo3", "character", "ANY"))
  expect_equal(class_register(klass), "Foo3")

  obj <- methods::new(klass, "x")
  expect_equal(obj_dispatch(obj), c("Foo3", "character", "ANY"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("can work with S4 multiple inheritance", {
  methods::setClass("Foo4", contains = "character")
  methods::setClass("Foo5")
  methods::setClass("Foo6", contains = c("Foo4", "Foo5"))
  klass <- methods::getClass("Foo6")

  expect_equal(class_type(klass), "S4")
  expect_equal(class_dispatch(klass), c("Foo6", "Foo4", "Foo5", "character", "ANY"))
  expect_equal(class_register(klass), "Foo6")

  obj <- methods::new(klass, "x")
  expect_equal(obj_dispatch(obj), c("Foo6", "Foo4", "Foo5", "character", "ANY"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

# input validation -------------------------------------------------------------

test_that("as_class gives informative errors", {
  expect_snapshot(error = TRUE, {
    as_class("foo")
    as_class(TRUE)
  })
})
