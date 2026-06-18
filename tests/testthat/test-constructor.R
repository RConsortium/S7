test_that("generates correct arguments from parent + properties", {
  # No arguments
  args <- constructor_args(S7_object)
  expect_equal(args$self, pairlist())
  expect_equal(args$parent, pairlist())

  # Includes properties
  args <- constructor_args(S7_object, as_properties(list(x = class_numeric)))
  expect_equal(args$self, pairlist(x = integer()))
  expect_equal(args$parent, pairlist())

  # test constructor arg defaults
  args <- constructor_args(
    S7_object,
    as_properties(list(
      a = class_any,
      b = class_missing,
      c = NULL | class_character,
      d = class_missing | class_numeric
    ))
  )
  expect_identical(
    args$self,
    as.pairlist(alist(a = NULL, b = , c = NULL, d = ))
  )
  expect_identical(args$parent, pairlist())

  # unless they're dynamic
  args <- constructor_args(
    S7_object,
    as_properties(list(x = new_property(getter = function(self) 10)))
  )
  expect_equal(args$self, pairlist())
  expect_equal(args$parent, pairlist())

  # Includes parent properties
  foo := new_class(properties = list(x = class_numeric))
  args <- constructor_args(foo, as_properties(list(y = class_numeric)))
  expect_equal(args$self, pairlist(y = integer()))
  expect_equal(args$parent, pairlist(x = integer()))

  # But only those in the constructor
  foo := new_class(
    properties = list(x = class_numeric),
    constructor = function() new_object(x = 1)
  )
  args <- constructor_args(foo, as_properties(list(y = class_numeric)))
  expect_equal(args$self, pairlist(y = integer()))
  expect_equal(args$parent, pairlist())
})

test_that("generates meaningful constructors", {
  expect_snapshot(
    {
      new_constructor(S7_object, list())
      new_constructor(
        S7_object,
        as_properties(list(x = class_numeric, y = class_numeric))
      )

      foo := new_class(parent = class_character)
      new_constructor(foo, list())

      foo2 := new_class(parent = foo)
      new_constructor(foo2, list())
    },
    transform = scrub_environment
  )
})

test_that("can generate constructors for S3 classes", {
  expect_snapshot(
    {
      new_constructor(class_factor, list())
      new_constructor(
        class_factor,
        as_properties(list(x = class_numeric, y = class_numeric))
      )
    },
    transform = scrub_environment
  )
})

test_that("can generate constructor for inherited abstract classes", {
  expect_snapshot(
    {
      foo1 := new_class(
        abstract = TRUE,
        properties = list(x = class_double)
      )
      new_constructor(foo1, list())
      new_constructor(foo1, as_properties(list(y = class_double)))
    },
    transform = scrub_environment
  )
  child := new_class(foo1, properties = list(y = class_double))
  expect_no_error(child(y = 0.5))

  # even if it has a read-only property
  prop_readonly <- new_property(getter = function(self) "test")
  child := new_class(foo1, properties = list(x = prop_readonly))
  expect_no_error(child())
})

test_that("can use `...` in parent constructor", {
  foo := new_class(
    properties = list(x = class_list),
    constructor = function(...) new_object(S7_object(), x = list(...))
  )

  expect_snapshot(
    new_constructor(foo, list(y = class_double)),
    transform = scrub_environment
  )

  # And check that arguments matched correctly
  bar := new_class(foo, properties = list(y = class_double))
  expect_equal(bar()@x, list())
  expect_equal(bar(2)@x, list(2))
  expect_equal(bar(y = 2)@x, list())
})

test_that("subclass can override simple parent property defaults", {
  foo := new_class(
    properties = list(x = new_property(class_numeric, default = 1))
  )
  foo2 := new_class(
    parent = foo,
    properties = list(x = new_property(class_numeric, default = 2))
  )
  expect_equal(foo2()@x, 2)
  expect_equal(foo2(x = 3)@x, 3)
})

test_that("subclass can override required parent propertes", {
  need_x <- quote(stop("need x"))
  required := new_class(
    properties = list(x = new_property(class_numeric, default = need_x))
  )
  required2 := new_class(
    parent = required,
    properties = list(x = new_property(class_numeric, default = 2))
  )
  expect_equal(required2()@x, 2)
})

test_that("subclass can override simple parent property when parent uses ...", {
  need_x <- quote(stop("need x"))
  dots := new_class(
    properties = list(x = new_property(class_numeric, default = need_x)),
    constructor = function(...) new_object(S7_object(), ...)
  )
  dots2 := new_class(
    parent = dots,
    properties = list(x = new_property(class_numeric, default = 2))
  )
  expect_equal(dots2()@x, 2)
})

test_that("compatible dynamic settable override is passed to parent", {
  need_x <- quote(stop("need x"))
  foo := new_class(
    properties = list(x = new_property(class_numeric, default = need_x))
  )
  foo2 := new_class(
    parent = foo,
    properties = list(
      x = new_property(
        class_double,
        default = 1,
        getter = function(self) self@x,
        setter = function(self, value) {
          self@x <- value + 1
          self
        }
      )
    )
  )
  expect_equal(foo2(x = 1)@x, 2)
  expect_equal(foo2()@x, 2)
})

test_that("subclass setter is used during construction", {
  foo := new_class(properties = list(x = class_integer, z = class_any))
  foo2 := new_class(
    parent = foo,
    properties = list(
      z = new_property(
        class_double,
        setter = function(self, value) {
          self@z <- value * 2
          self
        }
      )
    )
  )
  expect_equal(foo2(z = 1)@z, 2)
  expect_equal(foo2(x = 1L, z = 1)@x, 1L)
})

test_that("parent validators rerun after inherited properties are reset", {
  validator <- function(self) {
    if (self@x < 0) "@x must be non-negative"
  }

  foo := new_class(
    properties = list(x = class_double),
    constructor = function() new_object(S7_object(), x = 0),
    validator = validator
  )
  foo2 := new_class(
    parent = foo,
    properties = list(x = new_property(class_double, default = -1))
  )

  expect_error(foo2(), "@x must be non-negative")
})

test_that("overridden properties keep their position in the constructor", {
  foo := new_class(
    properties = list(x = class_numeric, y = class_numeric)
  )
  foo2 := new_class(
    parent = foo,
    properties = list(
      x = new_property(class_numeric, default = 2),
      z = class_numeric
    )
  )

  expect_named(formals(foo2), c("x", "y", "z"))
  expect_equal(props(foo2(1, 2, 3)), list(x = 1, y = 2, z = 3))
})

test_that("read-only properties filter only overridden constructor arguments", {
  foo := new_class(properties = list(x = class_character))
  foo2 := new_class(
    parent = foo,
    properties = list(x = new_property(getter = function(self) "fixed"))
  )
  expect_null(formals(foo2))
  expect_equal(foo2()@x, "fixed")

  Factor := new_class(
    parent = class_factor,
    properties = list(
      levels = new_property(getter = function(self) levels(self))
    )
  )
  expect_named(formals(Factor), c(".data", "levels"))

  x <- Factor(c(1L, 2L), levels = c("a", "b"))
  expect_equal(levels(x), c("a", "b"))
  expect_equal(Factor(c(1L, 2L), c("a", "b"))@levels, c("a", "b"))
})

test_that("dynamic settable property overrides are not passed to parent", {
  foo := new_class(properties = list(x = class_integer))
  foo2 := new_class(
    parent = foo,
    properties = list(
      x = new_property(
        class_character,
        getter = function(self) self@x,
        setter = function(self, value) {
          self@x <- value
          self
        }
      )
    )
  )

  expect_named(formals(foo2), "x")
  expect_equal(foo2()@x, character())
  expect_equal(foo2("a")@x, "a")
})

test_that("can create constructors with missing or lazy defaults", {
  Person := new_class(
    properties = list(
      # non-dynamic, default error call (required constructor arg)
      first_name = new_property(
        class_character,
        default = quote(stop(
          'argument "first_name" is missing, with no default'
        ))
      ),

      # non-dynamic, static default (optional constructor arg)
      middle_name = new_property(class_character, default = ""),

      # non-dynamic, nullable character
      last_name = new_property(NULL | class_character),

      # non-dynamic, but defaults to the value of another property
      nick_name = new_property(class_character, default = quote(first_name)),

      # non-dynamic, optional constructor argument, read-only after construction.
      birthdate = new_property(
        class = class_Date,
        default = quote(Sys.Date()),
        setter = function(self, value) {
          if (!is.null(self@birthdate)) {
            stop("Can't set read-only property Person@birthdate")
          }
          self@birthdate <- value
          self
        }
      ),

      # dynamic property, not a constructor argument
      age = new_property(class = class_any, getter = function(self) {
        Sys.Date() - self@birthdate
      })
    )
  )

  expect_equal(
    formals(Person),
    as.pairlist(alist(
      first_name = stop('argument "first_name" is missing, with no default'),
      middle_name = "",
      last_name = NULL,
      nick_name = first_name,
      birthdate = Sys.Date()
    ))
  ) # no age

  expect_error(Person(), 'argument "first_name" is missing, with no default')
  expect_null(Person("Alice")@last_name)

  p <- Person("Alice", , "Smith")

  expect_equal(p@nick_name, "Alice")
  expect_equal(p@middle_name, "")
  expect_equal(p@birthdate, Sys.Date())
  expect_equal(p@age, Sys.Date() - Sys.Date())

  p <- Person(
    "Bob",
    nick_name = "Bobby",
    "Allen",
    "Smith",
    as.Date('1970-01-01')
  )
  expect_equal(p@nick_name, "Bobby")
  expect_equal(p@birthdate, as.Date('1970-01-01'))
  expect_equal(p@age, Sys.Date() - as.Date('1970-01-01'))
  expect_equal(p@middle_name, "Allen")
  expect_error(
    p@birthdate <- as.Date('1970-01-01'),
    "Can\'t set read-only property Person@birthdate"
  )
})

test_that("Dynamic settable properties are included in constructor", {
  Foo := new_class(
    package = NULL,
    properties = list(
      dynamic_settable = new_property(
        class_numeric,
        getter = function(self) self@dynamic_settable,
        setter = function(self, value) {
          self@dynamic_settable <- value
          self
        }
      ),

      dynamic_read_only = new_property(
        class_numeric,
        getter = function(self) 99,
      )
    )
  )

  expect_equal(formals(Foo), pairlist(dynamic_settable = numeric()))
  expect_equal(Foo()@dynamic_settable, numeric())
  expect_equal(Foo(3)@dynamic_settable, 3)

  foo <- Foo()
  expect_error(
    foo@dynamic_read_only <- 1,
    "Can't set read-only property"
  )
  foo@dynamic_settable <- 1
  expect_equal(foo@dynamic_settable, 1)
})
