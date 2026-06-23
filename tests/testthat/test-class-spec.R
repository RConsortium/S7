test_that("can work with S7 classes", {
  klass := new_class(package = NULL)
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
  klass := new_class(package = "pkg")
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
  text := new_class(class_character, package = NULL)
  number := new_class(class_double, package = NULL)
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

test_that("class_extends checks subclass relationship between classes", {
  Parent := new_class(package = NULL)
  Child := new_class(parent = Parent, package = NULL)

  expect_true(class_extends(Child, Parent))
  expect_true(class_extends(Child, Child))
  expect_false(class_extends(Parent, Child))

  # base types
  expect_true(class_extends(class_integer, class_integer))
  expect_false(class_extends(class_integer, class_character))
})

test_that("class_extends handles unions, any, and NULL", {
  # union parent accepts any member; union child must have all members extend
  expect_true(class_extends(class_double, class_numeric))
  expect_false(class_extends(class_numeric, class_double))
  expect_true(class_extends(class_numeric, class_numeric))

  # class_any is the top type
  expect_true(class_extends(class_integer, class_any))
  expect_false(class_extends(class_any, class_integer))
  expect_true(class_extends(class_any, class_any))

  # NULL only extends NULL
  expect_true(class_extends(NULL, NULL))
  expect_false(class_extends(NULL, class_integer))
  expect_false(class_extends(class_integer, NULL))
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
  klass <- new_S3_class(
    c("ordered", "factor"),
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

test_that("class_inherits() requires S3 classes to be contiguous and ordered", {
  klass <- new_S3_class(c("a", "b"))

  # `klass`'s classes all present, but not contiguously
  gappy <- structure(list(), class = c("a", "x", "b"))
  expect_equal(class_inherits(gappy, klass), FALSE)

  # `klass`'s classes all present, but in the wrong order
  reversed <- structure(list(), class = c("b", "a"))
  expect_equal(class_inherits(reversed, klass), FALSE)

  # A genuine contiguous, ordered run succeeds
  ok <- structure(list(), class = c("z", "a", "b"))
  expect_equal(class_inherits(ok, klass), TRUE)
})

test_that("can work with S7 classes that extend S3 classes", {
  Date <- new_S3_class("Date", constructor = function(.data = numeric()) {
    .Date(.data)
  })
  Date2 := new_class(
    parent = Date,
    properties = list(x = class_numeric),
    package = NULL
  )

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
  Foo1 := local_S4_class(contains = "character")
  Foo2 := local_S4_class(contains = "Foo1")
  Foo3 := local_S4_class(slots = list(x = "numeric"))
  Foo4 := local_S4_class(contains = c("Foo2", "Foo3"))

  klass <- methods::getClass("Foo4")

  expect_equal(class_type(klass), "S4")
  expect_equal(
    class_dispatch(klass),
    c("S4/S7::Foo4", "S4/S7::Foo2", "S4/S7::Foo3", "S4/S7::Foo1", "character")
  )
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

# external ----------------------------------------------------------------

test_that("can work with external classes", {
  dep := local_package({
    Ext := new_class(properties = list(x = class_integer))
  })
  klass <- new_external_class(package = "dep", name = "Ext")
  expect_equal(as_class(klass), klass)

  expect_equal(class_type(klass), "S7_external")
  expect_equal(class_dispatch(klass), c("dep::Ext", "S7_object"))
  expect_equal(class_register(klass), "dep::Ext")
  expect_s3_class(class_construct(klass, x = 1L), "dep::Ext")
  expect_equal(class_desc(klass), "<dep::Ext>")
  expect_equal(class_deparse(klass), 'new_external_class("dep", "Ext")')

  obj <- dep$Ext(x = 1L)
  expect_equal(obj_type(obj), "S7")
  expect_equal(obj_desc(obj), "<dep::Ext>")
  expect_equal(obj_dispatch(obj), c("dep::Ext", "S7_object"))
  expect_equal(class_inherits(obj, klass), TRUE)
})

test_that("class_deparse() includes external class version", {
  klass <- new_external_class("pkg", "Klass", version = "1.0")
  expect_equal(
    class_deparse(klass),
    'new_external_class("pkg", "Klass", version = "1.0")'
  )
})

test_that("S7_class_desc() formats every supported class spec", {
  Foo := new_class(package = NULL)

  expect_equal(S7_class_desc(Foo), "<Foo>")
  expect_equal(S7_class_desc(class_integer), "<integer>")
  expect_equal(S7_class_desc(new_S3_class("data.frame")), "S3<data.frame>")
  expect_equal(
    S7_class_desc(class_integer | class_double),
    "<integer> or <double>"
  )
  expect_equal(S7_class_desc(NULL), "<NULL>")
  expect_equal(S7_class_desc(class_missing), "<MISSING>")
  expect_equal(S7_class_desc(class_any), "<ANY>")

  # non-class object errors via as_class()
  expect_snapshot(S7_class_desc(1L), error = TRUE)
})

# input validation -------------------------------------------------------------

test_that("as_class gives informative errors", {
  expect_snapshot(error = TRUE, {
    as_class("foo")
    as_class(TRUE)
  })
})
