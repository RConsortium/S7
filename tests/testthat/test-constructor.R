test_that("generates correct arguments from parent + properties",  {
  # No arguments
  args <- constructor_args(S7_object)
  expect_equal(args$self, pairlist())
  expect_equal(args$parent, pairlist())

  # Includes properties
  args <- constructor_args(S7_object, as_properties(list(x = class_numeric)))
  expect_equal(args$self, pairlist(x = integer()))
  expect_equal(args$parent, pairlist())

  # unless they're dynamic
  args <- constructor_args(S7_object,
    as_properties(list(x = new_property(getter = function(self) 10)))
  )
  expect_equal(args$self, pairlist())
  expect_equal(args$parent, pairlist())

  # Includes parent properties
  foo <- new_class("foo", properties = list(x = class_numeric))
  args <- constructor_args(foo, as_properties(list(y = class_numeric)))
  expect_equal(args$self, pairlist(y = integer()))
  expect_equal(args$parent, pairlist(x = integer()))

  # But only those in the constructor
  foo <- new_class("foo",
    properties = list(x = class_numeric),
    constructor = function() new_object(x = 1)
  )
  args <- constructor_args(foo, as_properties(list(y = class_numeric)))
  expect_equal(args$self, pairlist(y = integer()))
  expect_equal(args$parent, pairlist())
})

test_that("generates meaningful constructors", {
  expect_snapshot({
    new_constructor(S7_object, list())
    new_constructor(S7_object, as_properties(list(x = class_numeric, y = class_numeric)))

    foo <- new_class("foo", parent = class_character)
    new_constructor(foo, list())

    foo2 <- new_class("foo2", parent = foo)
    new_constructor(foo2, list())
  }, transform = scrub_environment)
})

test_that("can generate constructors for S3 classes", {
  expect_snapshot({
    new_constructor(class_factor, list())
    new_constructor(class_factor, as_properties(list(x = class_numeric, y = class_numeric)))
  }, transform = scrub_environment)
})

test_that("can generate constructor for inherited abstract classes", {
  expect_snapshot({
    foo1 <- new_class("foo1", abstract = TRUE, properties = list(x = class_double))
    new_constructor(foo1, list())
    new_constructor(foo1, as_properties(list(y = class_double)))
  }, transform = scrub_environment)
  child <- new_class("child", foo1, properties = list(y = class_double))
  expect_no_error(child(y = 0.5))

  # even if it has a read-only property
  prop_readonly <- new_property(getter = function(self) "test")
  child <- new_class("child", foo1, properties = list(x = prop_readonly))
  expect_no_error(child())
})

test_that("can use `...` in parent constructor", {
  foo <- new_class(
    "foo",
    properties = list(x = class_list),
    constructor = function(...) new_object(S7_object(), x = list(...))
  )

  expect_snapshot(
    new_constructor(foo, list(y = class_double)),
    transform = scrub_environment
  )

  # And check that arguments matched correctly
  bar <- new_class("bar", foo, properties = list(y = class_double))
  expect_equal(bar()@x, list())
  expect_equal(bar(2)@x, list(2))
  expect_equal(bar(y = 2)@x, list())
})

test_that("can create constructors with missing or lazy defaults", {

  Person <- new_class(
    name = "Person",
    properties = list(
      # non-dynamic, default missing (required constructor arg)
      first_name = new_property(class_character, default = quote(expr = )),

      # non-dynamic, static default (optional constructor arg)
      middle_name = new_property(class_character, default = ""),

      # non-dynamic, default missing (required constructor arg) (same as first_name)
      last_name = new_property(class_missing | class_character),

      # non-dynamic, but defaults to the value of another property
      nick_name = new_property(class_character, default = quote(first_name)),

      # non-dynamic, optional constructor argument, read-only after construction.
      birthdate = new_property(
        class = class_Date,
        default = quote(Sys.Date()),
        setter = function(self, value) {
          if (!is.null(self@birthdate))
            stop("Can't set read-only property Person@birthdate")
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

  expect_equal(formals(Person), as.pairlist(alist(
    first_name = ,
    middle_name = "",
    last_name = ,
    nick_name = first_name,
    birthdate = Sys.Date()
  ))) # no age

  expect_error(Person(), 'argument "first_name" is missing, with no default')
  expect_error(Person("Alice"), 'argument "last_name" is missing, with no default')

  p <- Person("Alice", ,"Smith")

  expect_equal(p@nick_name, "Alice")
  expect_equal(p@middle_name, "")
  expect_equal(p@birthdate, Sys.Date())
  expect_equal(p@age, Sys.Date() - Sys.Date())

  p <- Person("Bob", nick_name = "Bobby", "Allen" , "Smith", as.Date('1970-01-01'))
  expect_equal(p@nick_name, "Bobby")
  expect_equal(p@birthdate, as.Date('1970-01-01'))
  expect_equal(p@age, Sys.Date() - as.Date('1970-01-01'))
  expect_equal(p@middle_name, "Allen")
  expect_error(p@birthdate <- as.Date('1970-01-01'),
               "Can\'t set read-only property Person@birthdate")
})
