test_that("can work with R7 classes", {
  klass <- new_class("klass")
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "r7")
  expect_equal(class_dispatch(klass), c("klass", "R7_object"))
  expect_equal(class_register(klass), "klass")
  expect_equal(class_construct(klass), klass())
  expect_equal(class_desc(klass), "<klass>")
  expect_equal(class_deparse(klass), "klass")

  obj <- klass()
  expect_equal(obj_type(obj), "r7")
  expect_equal(obj_desc(obj), "<klass>")
  expect_equal(obj_dispatch(obj), c("klass", "R7_object"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("can work with unions", {
  klass <- new_union(text, number)
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "r7_union")
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
  expect_equal(class_dispatch(NULL), NULL)
  expect_equal(class_register(NULL), "NULL")
  expect_equal(class_construct(NULL), NULL)
  expect_equal(class_desc(NULL), "<ANY>")
  expect_equal(class_deparse(NULL), "")

  expect_equal(obj_type(NULL), "NULL")
  expect_equal(obj_desc(NULL), "NULL")
  expect_equal(obj_dispatch(NULL), "NULL")
  expect_equal(class_inherits("x", NULL), TRUE)
})

test_that("can work with S4 classes", {
  methods::setClass("Range", slots = c(start = "numeric", end = "numeric"))
  klass <- methods::getClass("Range")

  expect_equal(class_type(klass), "s4")
  expect_equal(class_dispatch(klass), "Range")
  expect_equal(class_register(klass), "Range")
  expect_s4_class(class_construct(klass, start = 1, end = 2), "Range")
  expect_equal(class_desc(klass), "S4<Range>")
  expect_equal(class_deparse(klass), "Range")

  obj <- methods::new(klass, start = 1, end = 1)
  expect_equal(obj_type(obj), "s4")
  expect_equal(obj_desc(obj), "S4<Range>")
  expect_equal(obj_dispatch(obj), "Range")
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("can work with S4 subclasses", {
  methods::setClass("Foo1", slots = c(start = "numeric", end = "numeric"))
  methods::setClass("Foo2", contains = "Foo1")
  klass <- methods::getClass("Foo2")

  expect_equal(class_type(klass), "s4")
  expect_equal(class_dispatch(klass), c("Foo2", "Foo1"))
  expect_equal(class_register(klass), "Foo2")

  obj <- methods::new(klass, start = 1, end = 1)
  expect_equal(obj_dispatch(obj), c("Foo2", "Foo1"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("can work with S4 subclasses of base classes", {
  methods::setClass("Foo3", contains = "character")
  klass <- methods::getClass("Foo3")

  expect_equal(class_type(klass), "s4")
  expect_equal(class_dispatch(klass), c("Foo3", "character"))
  expect_equal(class_register(klass), "Foo3")

  obj <- methods::new(klass, "x")
  expect_equal(obj_dispatch(obj), c("Foo3", "character"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("can work with S4 multiple inheritance", {
  methods::setClass("Foo4", contains = "character")
  methods::setClass("Foo5")
  methods::setClass("Foo6", contains = c("Foo4", "Foo5"))
  klass <- methods::getClass("Foo6")

  expect_equal(class_type(klass), "s4")
  expect_equal(class_dispatch(klass), c("Foo6", "Foo4", "Foo5", "character"))
  expect_equal(class_register(klass), "Foo6")

  obj <- methods::new(klass, "x")
  expect_equal(obj_dispatch(obj), c("Foo6", "Foo4", "Foo5", "character"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("converts S4 base classes to R7 base classes", {
  expect_equal(as_class(getClass("NULL")), base_classes[["NULL"]])
  expect_equal(as_class(getClass("character")), base_classes$character)
})

test_that("converts S4 unions to R7 unions", {
  Foo1 <- setClass("Foo1", slots = "x")
  Foo2 <- setClass("Foo2", slots = "x")
  Foo3 <- setClass("Foo3", slots = "x")

  Union1 <- setClassUnion("Union1", c("Foo1", "Foo2"))
  expect_equal(as_class(Union1), new_union(getClass("Foo1"), getClass("Foo2")))

  Union2 <- setClassUnion("Union2", c("Union1", "Foo3"))
  expect_equal(as_class(Union2), new_union(getClass("Foo1"), getClass("Foo2"), getClass("Foo3")))
})

test_that("can work with simple S3 classes", {
  klass <- s3_data.frame
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "r7_s3")
  expect_equal(class_dispatch(klass), c("R7_object", "data.frame"))
  expect_equal(class_register(klass), "data.frame")
  expect_equal(class_desc(klass), "S3<data.frame>")
  expect_equal(class_construct(klass, list(x = 1)), data.frame(x = 1))
  expect_equal(class_deparse(klass), 's3_class("data.frame")')

  obj <- data.frame()
  expect_equal(obj_type(obj), "s3")
  expect_equal(obj_desc(obj), "S3<data.frame>")
  expect_equal(obj_dispatch(obj), "data.frame")
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("can work with s3 subclasses", {
  klass <- s3_class(
    class = c("ordered", "factor"),
    constructor = function(.data = numeric(), levels) ordered(.data, levels)
  )
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "r7_s3")
  expect_equal(class_dispatch(klass), c("R7_object", "ordered", "factor"))
  expect_equal(class_register(klass), "ordered")
  expect_equal(class_desc(klass), "S3<ordered>")
  expect_equal(class_construct(klass), ordered(numeric()))
  expect_equal(class_deparse(klass), 's3_class("ordered", "factor")')

  obj <- ordered(integer())
  expect_equal(obj_type(obj), "s3")
  expect_equal(obj_desc(obj), "S3<ordered>")
  expect_equal(obj_dispatch(obj), c("ordered", "factor"))
  expect_equal(class_inherits(obj, klass), TRUE)
  expect_equal(class_inherits(factor(), klass), FALSE)
})

test_that("can work with R7 classes that extend S3 classes", {
  Date <- s3_class("Date", constructor = function(.data = numeric()) .Date(.data))
  Date2 <- new_class("Date2", parent = Date, properties = list(x = "numeric"))

  expect_equal(class_type(Date2), "r7")
  expect_equal(class_dispatch(Date2), c("Date2", "R7_object", "Date"))
  expect_equal(class_register(Date2), "Date2")

  obj <- Date2(x = 1)
  expect_equal(obj_type(obj), "r7")
  expect_equal(obj_desc(obj), "<Date2>")
  expect_equal(obj_dispatch(obj), c("Date2", "R7_object", "Date"))
  expect_equal(class_inherits(obj, Date2), TRUE)
})

test_that("can work with base types", {
  expect_equal(as_class("character"), base_classes$character)
  expect_equal(as_class(character), base_classes$character)
  expect_equal(as_class(double), base_classes$double)

  klass <- as_class("character")
  expect_equal(class_type(klass), "r7_base")
  expect_equal(class_dispatch(klass), c("R7_object", "character"))
  expect_equal(class_register(klass), "character")
  expect_equal(class_desc(klass), "<character>")
  expect_equal(class_construct(klass, "x"), base_classes$character("x"))
  expect_equal(class_deparse(klass), '"character"')

  obj <- "x"
  expect_equal(obj_type(obj), "base")
  expect_equal(obj_desc(obj), "<character>")
  expect_equal(obj_dispatch(obj), "character")
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("class_inherits handles variation in class names", {
  expect_true(class_inherits(1, base_classes$double))

  expect_true(class_inherits(1L, base_classes$numeric))
  expect_true(class_inherits(1, base_classes$numeric))

  expect_true(class_inherits(function() {}, base_classes$`function`))
  expect_true(class_inherits(sum, base_classes$`function`))
  expect_true(class_inherits(`[`, base_classes$`function`))
})

test_that("can get class from base constructor", {
  expect_equal(as_class(character), base_classes$character)
  expect_equal(as_class(`function`), base_classes$`function`)

  expect_snapshot_error(as_class(mean))
})

# input validation -------------------------------------------------------------

test_that("as_class gives informative errors", {
  expect_snapshot(error = TRUE, {
    as_class("foo")
    as_class(TRUE)
  })
})
