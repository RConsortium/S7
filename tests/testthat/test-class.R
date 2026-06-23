test_that("S7 classes possess expected properties", {
  foo := new_class(package = "S7", validator = function(self) NULL)

  expect_equal(prop_names(foo), setdiff(names(attributes(foo)), "class"))
  expect_type(foo@name, "character")
  expect_equal(foo@parent, S7_object)
  expect_type(foo@constructor, "closure")
  expect_type(foo@validator, "closure")
  expect_type(foo@properties, "list")
})

test_that("S7 classes print nicely", {
  foo1 := new_class(
    properties = list(x = class_integer, y = class_integer),
    package = NULL
  )
  foo2 := new_class(foo1, package = NULL)

  expect_snapshot({
    foo2

    str(foo2)
    # Omit details when nested
    str(list(foo2))
  })
})

test_that("S7 classes print @package and @abstract details", {
  foo := new_class(package = "S7", abstract = TRUE)
  expect_snapshot(foo)
})

test_that("S7 classes show property defaults and read-only annotations", {
  Address := new_class(package = "S7")
  Person := new_class(
    properties = list(
      implicit_default = new_property(class_character),
      implicit_complex = new_property(class_Date),
      implicit_S7 = new_property(Address),
      default_value = new_property(class_character, default = ""),
      default_expr = new_property(class_Date, default = quote(Sys.Date())),
      read_only = new_property(getter = \(self) Sys.Date() - self@birthdate)
    ),
    package = "S7"
  )
  expect_snapshot(Person)
})

test_that("S7 classes check inputs", {
  expect_snapshot(error = TRUE, {
    new_class(1)
    new_class("foo", 1)

    new_class("foo", package = 1)

    new_class("foo", constructor = 1)
    new_class("foo", constructor = function() {})

    new_class("foo", validator = function() {})
  })
})

test_that("S7 classes can't inherit from S4 or class unions", {
  parentS4 := local_S4_class(slots = c(x = "numeric"))
  expect_snapshot(error = TRUE, {
    new_class("test", parent = parentS4)
    new_class("test", parent = new_union("character"))
  })
})

test_that("inheritance combines properties for parent classes", {
  foo1 := new_class(properties = list(x = class_double))
  foo2 := new_class(foo1, properties = list(y = class_double))
  expect_equal(names(foo2@properties), c("x", "y"))
})

test_that("inheritance lets child properties override parent", {
  foo1 := new_class(properties = list(x = class_numeric))
  foo2 := new_class(foo1, properties = list(x = class_double))
  expect_equal(names(foo2@properties), "x")
  expect_equal(foo2@properties$x$class, class_double)
})

test_that("inheritance lets child properties narrow the parent's type", {
  Parent := new_class(package = NULL)
  Child := new_class(parent = Parent, package = NULL)
  foo1 := new_class(properties = list(x = Parent))
  expect_no_error(new_class("foo2", foo1, properties = list(x = Child)))
  expect_no_error(new_class(
    "foo3",
    foo1,
    properties = list(x = new_property(Child, default = quote(Child())))
  ))
})

test_that("inheritance lets child properties narrow with S4 inheritance", {
  S4PropertyParent := local_S4_class(slots = c(x = "numeric"))
  S4PropertyChild := local_S4_class(contains = "S4PropertyParent")

  Parent := new_class(
    properties = list(x = S4PropertyParent),
    package = NULL
  )
  Child := new_class(
    parent = Parent,
    properties = list(x = S4PropertyChild),
    package = NULL
  )

  x <- methods::new("S4PropertyChild", x = 1)
  expect_s4_class(Child(x = x)@x, "S4PropertyChild")
})

test_that("inheritance doesn't let child properties narrow S7_object with base or S3 classes", {
  Parent := new_class(
    properties = list(x = S7_object),
    package = NULL,
    abstract = TRUE
  )

  expect_error(
    new_class(
      "IntegerChild",
      Parent,
      properties = list(x = class_integer),
      package = NULL
    ),
    "must narrow"
  )
  expect_error(
    new_class(
      "FormulaChild",
      Parent,
      properties = list(x = class_formula),
      package = NULL
    ),
    "must narrow"
  )
})

test_that("inheritance lets child properties narrow parent unions that include any", {
  Parent := new_class(
    properties = list(x = class_any | class_integer),
    package = NULL
  )
  expect_no_error(new_class(
    "Child",
    Parent,
    properties = list(x = class_any),
    package = NULL
  ))
})

test_that("inheritance handles external class property specs", {
  dep <- local_package("dep", {
    External := new_class()
  })
  External <- new_external_class(package = "dep", name = "External")

  ParentObject := new_class(
    properties = list(x = S7_object),
    package = NULL
  )
  ChildObject := new_class(
    parent = ParentObject,
    properties = list(
      x = new_property(class = External, default = quote(dep$External()))
    ),
    package = NULL
  )
  expect_s3_class(ChildObject()@x, "dep::External")

  Ext <- new_external_class(package = "notloaded.pkg", name = "Cls")
  prop <- new_property(
    class = Ext,
    default = quote({
      NULL
    })
  )
  ParentSame := new_class(
    properties = list(x = prop),
    package = NULL
  )
  expect_no_error(new_class(
    name = "ChildSame",
    parent = ParentSame,
    properties = list(x = prop),
    package = NULL
  ))

  Missing <- new_external_class(package = "S7testthatmissing", name = "Missing")
  ParentUnion := new_class(
    properties = list(
      x = new_property(
        class = Missing | S7_object,
        default = quote(S7_object())
      )
    ),
    package = NULL
  )
  ChildUnion := new_class(
    parent = ParentUnion,
    properties = list(x = S7_object),
    package = NULL
  )
  expect_s3_class(ChildUnion(), "ChildUnion")
})

test_that("inheritance lets child properties narrow external parent classes", {
  dep <- local_package("dep_external_subclass", {
    Base := new_class()
    Sub := new_class(parent = Base)
  })
  Base <- new_external_class(package = "dep_external_subclass", name = "Base")

  Parent := new_class(
    properties = list(
      x = new_property(class = Base, default = quote(dep$Base()))
    ),
    package = NULL
  )
  Child := new_class(
    parent = Parent,
    properties = list(
      x = new_property(class = dep$Sub, default = quote(dep$Sub()))
    ),
    package = NULL
  )

  expect_s3_class(Child()@x, "dep_external_subclass::Sub")
})

test_that("inheritance lets child properties narrow optional union properties with NULL", {
  Parent := new_class(
    properties = list(x = NULL | class_numeric),
    package = NULL
  )

  NullChild := new_class(
    parent = Parent,
    properties = list(x = NULL),
    package = NULL
  )
  expect_equal(NullChild()@x, NULL)

  OptionalIntegerChild := new_class(
    parent = Parent,
    properties = list(x = NULL | class_integer),
    package = NULL
  )
  expect_equal(OptionalIntegerChild()@x, NULL)
  expect_equal(OptionalIntegerChild(x = 1L)@x, 1L)
})

test_that("inheritance doesn't let child properties widen or change the parent's type", {
  foo1 := new_class(
    properties = list(x = class_integer),
    package = NULL
  )
  expect_snapshot(error = TRUE, {
    new_class("foo2", foo1, properties = list(x = class_character))
    new_class("foo3", foo1, properties = list(x = class_numeric))
    new_class("foo4", foo1, properties = list(x = class_any))
  })
})


test_that("subclassing an external class requires its package to be loaded", {
  Ext <- new_external_class("notloaded.pkg", "Cls")
  Parent := new_class(properties = list(x = NULL | Ext), package = NULL)

  expect_snapshot(
    new_class("Child", Parent, properties = list(x = Ext)),
    error = TRUE
  )
})

test_that("inheritance lets dynamic child properties override any parent type", {
  foo1 := new_class(properties = list(x = class_integer))
  readonly <- new_property(class_character, getter = function(self) "x")
  expect_no_error(new_class("foo2", foo1, properties = list(x = readonly)))
})

test_that("abstract classes can't be instantiated", {
  expect_snapshot(error = TRUE, {
    foo := new_class(abstract = TRUE)
    foo()
  })
})

test_that("abstract classes can't inherit from concrete class", {
  expect_snapshot(error = TRUE, {
    foo1 := new_class()
    new_class("foo2", parent = foo1, abstract = TRUE)
  })
})

test_that("abstract classes can construct concrete subclasses", {
  foo1 := new_class(abstract = TRUE, package = NULL)
  foo2 := new_class(parent = foo1, package = NULL)
  expect_s3_class(foo2(), "foo2")
})

test_that("abstract classes can use inherited validator from abstract class", {
  foo1 := new_class(
    properties = list(x = class_double),
    abstract = TRUE,
    validator = function(self) {
      if (self@x == 2) "@x has bad value"
    },
    package = NULL
  )
  foo2 := new_class(parent = foo1, package = NULL)
  expect_no_error(foo2(x = 1))
  expect_snapshot(foo2(x = 2), error = TRUE)
})

test_that("new_object() gives useful error if called directly", {
  expect_snapshot(new_object(), error = TRUE)
})

test_that("new_object() errors if `_parent` doesn't inherit from the parent class (#409)", {
  Bar := new_class(package = NULL)
  # `_parent` should be `Bar()`, not the class spec `Bar`
  Foo := new_class(
    parent = Bar,
    package = NULL,
    constructor = function() new_object(class_integer)
  )
  # wrong-type instance
  Baz := new_class(
    parent = class_integer,
    package = NULL,
    constructor = function() new_object("hello")
  )
  expect_snapshot(error = TRUE, {
    Foo()
    Baz()
  })
})

test_that("new_object() allows S7_object placeholder for abstract parents", {
  Abstract := new_class(
    package = NULL,
    properties = list(x = class_integer),
    abstract = TRUE
  )
  Concrete := new_class(parent = Abstract, package = NULL)
  expect_no_error(Concrete(x = 1L))
})

test_that("new_object() allows arbitrary placeholder for abstract S3 parents (#686)", {
  Concrete := new_class(
    parent = class_POSIXt,
    constructor = function(x) new_object(x)
  )
  expect_no_error(Concrete(list(1, "A")))
})

test_that("new_object() has fallback for S3 classes created by older S7 (#686)", {
  old_s3 <- class_POSIXt
  old_s3$abstract <- NULL
  Foo := new_class(parent = old_s3, constructor = \(x) new_object(x))
  expect_no_error(Foo(list(1, "A")))
})

test_that("new_object() errors if `_parent` is supplied but class has no parent", {
  NoParent := new_class(
    package = NULL,
    parent = NULL,
    constructor = function() new_object(42L)
  )
  expect_snapshot(NoParent(), error = TRUE)
})

test_that("new_object() can set a property named `.parent` (#423)", {
  foo := new_class(
    properties = list(.parent = class_double),
    package = NULL,
    constructor = function(.parent) new_object(S7_object(), .parent = .parent)
  )
  obj <- foo(.parent = 1)
  expect_equal(obj@.parent, 1)
})

test_that("new_object() validates object", {
  foo := new_class(
    properties = list(x = new_property(class_double)),
    validator = function(self) if (self@x < 0) "x must be positive",
    package = NULL
  )

  expect_snapshot(error = TRUE, {
    foo("x")
    foo(-1)
  })
})

test_that("new_object() accepts a single unnamed named list of properties (#497)", {
  Foo := new_class(
    properties = list(x = class_double, y = class_double),
    package = NULL,
    constructor = function(props) new_object(S7_object(), props)
  )
  obj <- Foo(list(x = 1, y = 2))
  expect_equal(obj@x, 1)
  expect_equal(obj@y, 2)
})

test_that("new_object() runs each parent validator exactly once", {
  A := new_class(validator = function(self) cat("A "))
  B := new_class(parent = A, validator = function(self) cat("B "))
  C := new_class(parent = B, validator = function(self) cat("C "))

  expect_snapshot({
    . <- A()
    . <- B()
    . <- C()
  })
})

test_that("S7 object has an S7 and S3 class", {
  foo := new_class(package = NULL)
  x <- foo()
  expect_equal(S7_class(x), foo)
  expect_equal(class(x), c("foo", "S7_object"))
})

test_that("S7 object displays nicely", {
  expect_snapshot({
    foo := new_class(
      properties = list(x = class_double, y = class_double),
      package = NULL
    )
    foo()
    str(list(foo()))
  })
})

test_that("S7 object displays objects with data nicely", {
  expect_snapshot({
    text := new_class(class_character, package = NULL)
    text("x")
    str(list(text("x")))
  })
})

test_that("S7 object displays data.frame subclasses without error (#494)", {
  mydf := new_class(class_data.frame, package = NULL)
  expect_snapshot(str(mydf(data.frame(a = 1:2, b = 1:2))))
})

test_that("S7 object displays list objects nicely", {
  foo1 := new_class(
    parent = class_list,
    properties = list(x = class_double, y = class_list),
    package = NULL
  )
  expect_snapshot(
    foo1(
      list(
        x = 1,
        y = list(a = 21, b = 22)
      ),
      x = 3,
      y = list(a = 41, b = 42)
    )
  )
})

test_that("default constructor initializes properties with defaults", {
  foo1 := new_class(properties = list(x = class_double))
  expect_equal(props(foo1()), list(x = double()))

  foo2 := new_class(foo1, properties = list(y = class_double))
  expect_equal(props(foo2()), list(x = double(), y = double()))
})

test_that("default constructor overrides properties with arguments", {
  foo1 := new_class(properties = list(x = class_double))
  foo2 := new_class(foo1, properties = list(y = class_double))
  expect_equal(props(foo2(x = 1)), list(x = 1, y = double()))
  expect_equal(props(foo2(x = 1, y = 2)), list(x = 1, y = 2))
})

test_that("default constructor can initialise a property to NULL", {
  foo := new_class(
    properties = list(
      x = new_property(default = 10)
    )
  )
  x <- foo(x = NULL)
  expect_equal(x@x, NULL)
})

test_that("default constructor initializes data with defaults", {
  text1 := new_class(parent = class_character)
  obj <- text1()
  expect_equal(S7_data(obj), character())
})

test_that("default constructor overrides data with defaults", {
  text1 := new_class(parent = class_character)
  expect_equal(S7_data(text1("x")), "x")
})

test_that("default constructor initializes property with S7 object", {
  foo1 := new_class(package = NULL)
  foo2 := new_class(properties = list(x = foo1), package = NULL)
  x <- foo2()
  expect_s3_class(x@x, "foo1")
})

test_that("c(<S7_class>, ...) gives error", {
  foo1 := new_class()
  expect_snapshot(error = TRUE, {
    c(foo1, foo1)
  })
})

test_that("can round trip to disk and back", {
  eval(
    quote({
      foo1 := new_class(properties = list(y = class_integer))
      foo2 := new_class(properties = list(x = foo1))
      f <- foo2(x = foo1(y = 1L))
    }),
    globalenv()
  )

  f <- globalenv()[["f"]]
  path <- tempfile()
  saveRDS(f, path)
  f2 <- readRDS(path)

  expect_equal(f, f2)
  rm(foo1, foo2, f, envir = globalenv())
})

test_that("objects from a previous version of S7 still work (#677)", {
  # Older versions of S7 stored the class object in the `S7_class` attribute
  # rather than `_S7_class`.
  Foo := new_class(
    parent = class_double,
    properties = list(x = class_numeric)
  )
  obj <- Foo(1, x = 2)
  attr(obj, "S7_class") <- attr(obj, "_S7_class", exact = TRUE)
  attr(obj, "_S7_class") <- NULL

  expect_equal(S7_class(obj), Foo)
  expect_equal(obj@x, 2)
  expect_equal(prop(obj, "x"), 2)

  obj@x <- 3
  expect_equal(obj@x, 3)

  expect_equal(S7_data(obj), 1)
  expect_equal(convert(obj, to = class_double), 1)
})

test_that("can't create class with `...` property name", {
  expect_snapshot(error = TRUE, {
    new_class("foo", properties = list(... = class_character))
  })
})

test_that("S7_class() returns a usable spec for any object (#559)", {
  # base types
  expect_equal(S7_class(1L), class_integer)
  expect_equal(S7_class(NULL), NULL)

  # S3
  expect_equal(S7_class(factor("a")), new_S3_class("factor"))
  expect_equal(S7_class(Sys.time()), new_S3_class(c("POSIXct", "POSIXt")))

  # NULL and missing
  expect_equal(S7_class(quote(expr = )), class_missing)
})

test_that("S7_class() gives informative error if no S7 spec available", {
  expect_snapshot(error = TRUE, S7_class(pairlist(x = 1)))
})
