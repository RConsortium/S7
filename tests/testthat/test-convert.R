test_that("can register convert methods", {
  local_methods(convert)
  converttest := new_class(package = NULL)
  method(convert, list(converttest, class_character)) <- function(
    from,
    to,
    ...
  ) {
    "c"
  }
  method(convert, list(converttest, class_integer)) <- function(from, to, ...) {
    "i"
  }

  obj <- converttest()
  expect_equal(convert(obj, to = class_character), "c")
  expect_equal(convert(obj, to = class_integer), "i")

  # Errors if none found
  expect_snapshot(
    convert(obj, to = class_double),
    error = TRUE,
    # for < 4.4.0
    transform = \(x) gsub("'S4'", "'object'", x)
  )
})

test_that("doesn't convert to subclass", {
  local_methods(convert)
  converttest1 := new_class()
  converttest2 := new_class(converttest1)

  method(convert, list(class_integer, converttest1)) <- function(
    from,
    to,
    ...
  ) {
    "i"
  }
  expect_error(convert(class_integer, to = converttest2), "Can't find method")
})

test_that("fallback convert can convert to own class", {
  local_methods(convert)
  foo1 := new_class(package = NULL)
  foo2 := new_class(foo1, package = NULL)

  obj <- convert(foo2(), to = foo2)
  expect_equal(class(obj), c("foo2", "foo1", "S7_object"))
  expect_equal(S7_class(obj), foo2)
})

test_that("fallback convert can convert to super class", {
  local_methods(convert)
  foo1 := new_class(
    properties = list(x = class_double),
    package = NULL
  )
  foo2 := new_class(
    foo1,
    properties = list(y = class_double),
    package = NULL
  )

  obj <- convert(foo2(1, 2), to = foo1)
  expect_equal(class(obj), c("foo1", "S7_object"))
  expect_equal(S7_class(obj), foo1)
  expect_identical(attr(obj, "_S7_version", exact = TRUE), 1L)
  expect_equal(props(obj), list(x = 1))
  expect_equal(attr(obj, "y"), NULL)
})

test_that("fallback convert can convert to subclass", {
  local_methods(convert)
  Foo := new_class(properties = list(x = class_numeric))
  Bar := new_class(Foo, properties = list(y = class_numeric))

  foo <- Foo(x = 1)

  # Basic conversion
  bar <- convert(foo, Bar)
  expect_s3_class(bar, c("Bar", "Foo", "S7_object"))
  expect_equal(S7_class(bar), Bar)
  expect_equal(bar@x, 1)
  expect_equal(bar@y, numeric(0))

  # Overriding existing property
  bar <- convert(foo, Bar, x = 2)
  expect_equal(bar@x, 2)

  # Setting new property
  bar <- convert(foo, Bar, y = 2)
  expect_equal(bar@x, 1)
  expect_equal(bar@y, 2)

  # Setting both properties
  bar <- convert(foo, Bar, y = 2, x = 3)
  expect_equal(bar@x, 3)
  expect_equal(bar@y, 2)

  # Error on converting to unrelated class
  Unrelated := new_class(properties = list(z = class_character))
  expect_error(convert(foo, Unrelated), "Can't find method")
})

test_that("downcasts only splice plain lists of property values", {
  Box := new_class(parent = class_integer, properties = list(x = class_any))
  df <- data.frame(x = 2L)

  expect_snapshot(convert(1L, Box, df), error = TRUE)
  expect_identical(convert(1L, Box, as.list(df))@x, df$x)
  expect_identical(convert(1L, Box, x = df)@x, df)
})

test_that("fallback convert accepts a single unnamed list of overrides when downcasting (#497)", {
  local_methods(convert)
  Foo := new_class(properties = list(x = class_numeric))
  Bar := new_class(Foo, properties = list(y = class_numeric))

  bar <- convert(Foo(x = 1), Bar, list(x = 2, y = 3))
  expect_equal(bar@x, 2)
  expect_equal(bar@y, 3)
})

test_that("downcasts distinguish omitted overrides from supplied missing values", {
  capture <- \(.data, x = 1L) new_object(list(x = x))
  Box := new_class(parent = class_list, constructor = capture)

  expect_identical(S7_data(convert(list(), Box, list())), list(x = 1L))
  expect_identical(S7_data(convert(list(), Box, alist(x = ))), alist(x = ))
})

test_that("downcasts quote calls and symbols and pass ordinary values through", {
  Parent := new_class(
    parent = class_double,
    properties = list(call = class_call, x = class_any)
  )
  label <- new_property(default = quote(deparse(substitute(x))))
  Child := new_class(parent = Parent, properties = list(label = label))
  values <- list(call = quote(f(x)), x = quote(x))
  parent <- convert(1, Parent, c(values, list(.data = 2)))

  expect_identical(S7_data(parent), 1)
  expect_identical(S7_data(convert(1, Parent, .data = 2)), 1)
  expect_identical(props(parent), values)
  expect_identical(props(convert(parent, Child), names(values)), values)

  y <- 2L
  child <- convert(parent, Child, call = quote(g(x)), x = y)
  expect_identical(child@call, quote(g(x)))
  expect_identical(child@label, "2L")
})

test_that("downcasts pass underlying data and honor explicit overrides", {
  Parent := new_class(
    parent = class_double,
    properties = list(label = class_character)
  )
  Child := new_class(parent = Parent)
  data <- c(a = 1, b = 2)
  parent <- Parent(.data = data, label = "source")

  child <- convert(parent, Child)
  expect_identical(S7_data(child), data)
  expect_identical(child@label, "source")
  expect_identical(S7_data(convert(parent, Child, .data = 3)), 3)
})

test_that("downcasts pass S3 objects and calls as underlying data", {
  FactorParent := new_class(parent = class_factor)
  FactorChild := new_class(parent = FactorParent)
  values <- factor(c("a", "b"), levels = c("b", "a", "c"))
  expect_identical(
    S7_data(convert(FactorParent(.data = values), FactorChild)),
    values
  )

  CallParent := new_class(parent = class_call)
  CallChild := new_class(parent = CallParent)
  expr <- quote(f(x))
  expect_identical(S7_data(convert(CallParent(.data = expr), CallChild)), expr)
})

test_that("downcasts keep S3 attributes exposed by getters", {
  Factor := new_class(
    parent = class_factor,
    properties = list(
      levels = new_property(getter = \(self) levels(self))
    )
  )
  Child := new_class(parent = Factor)
  parent <- Factor(.data = 1L, levels = c("a", "b"))
  child <- convert(parent, Child)
  expect_identical(child@levels, c("a", "b"))
  expect_identical(S7_data(child), factor("a", levels = c("a", "b")))
})

test_that("downcasts leave input to handwritten S3 constructors", {
  quantity <- new_S3_class(
    class = "quantity",
    constructor = function(.data = 0) {
      stopifnot(!is.object(.data))
      structure(.data, class = "quantity")
    }
  )
  Quantity := new_class(parent = quantity)
  Child := new_class(parent = Quantity)
  parent <- Quantity(.data = 1)
  expect_identical(as.double(convert(parent, Child)), 0)
  expect_identical(as.double(convert(parent, Child, .data = 3)), 3)
})

test_that("downcasts keep the default for S3 constructors embedded by S7 1.0.0", {
  old_frame <- class_data.frame
  old_frame$accepts_classed_data <- NULL
  Parent := new_class(parent = old_frame)
  Child := new_class(parent = Parent)

  parent <- Parent(.data = list(x = 1))
  expect_identical(S7_data(convert(parent, Child)), data.frame())
})

test_that("downcasts preserve external-pointer sources", {
  fresh_pointer <- \() unserialize(serialize(methods::new("externalptr"), NULL))
  handle <- new_S3_class(
    class = "handle",
    constructor = \(.data = fresh_pointer()) structure(.data, class = "handle")
  )
  Handle := new_class(parent = handle)
  Child := new_class(parent = Handle)
  parent <- Handle()
  child <- convert(parent, Child)
  expect_identical(S7_class(parent), Handle)
  expect_identical(S7_class(child), Child)
  expect_snapshot(error = TRUE, S7_data(parent))
  expect_identical(S7_class(parent), Handle)
})

test_that("downcasts let inherited setters rebuild their data once", {
  Base := new_class(parent = class_list)
  Journal := new_class(
    parent = Base,
    properties = list(
      last = new_property(
        class = class_character,
        setter = function(self, value) {
          self@last <- value
          S7_data(self, check = FALSE) <- c(S7_data(self), list(value))
          self
        }
      )
    )
  )
  Child := new_class(parent = Journal)
  parent <- convert(Base(.data = list("first")), Journal, last = "a")
  expect_identical(S7_data(parent), list("first", "a"))
  child <- convert(parent, Child)
  expect_identical(S7_data(child), list("a"))
  expect_identical(child@last, "a")
  changed <- convert(parent, Child, last = "b")
  expect_identical(S7_data(changed), list("b"))
  expect_identical(changed@last, "b")
})

test_that("downcasts start with fresh accessor and validation state", {
  Parent := new_class(
    parent = class_double,
    properties = list(
      view = new_property(getter = \(self) convert(self, Child))
    )
  )
  Child := new_class(
    parent = Parent,
    properties = list(
      view = new_property(getter = \(self) "child")
    )
  )
  parent <- Parent(.data = 1)
  expect_identical(parent@view@view, "child")

  Invalid := new_class(parent = Parent, validator = \(self) "invalid child")
  expect_snapshot(
    error = TRUE,
    valid_eventually(parent, function(x) {
      convert(x, Invalid)
      x
    })
  )
})

test_that("downcasts use the current constructor of an external parent", {
  dep := local_package({
    Parent := new_class(parent = class_double)
  })
  Child := new_class(parent = dep$Parent, package = "extension")
  Grandchild := new_class(parent = Child, package = "extension")
  expect_identical(S7_data(convert(dep$Parent(.data = 1), Child)), 1)
  expect_identical(S7_data(convert(dep$Parent(.data = 1), Grandchild)), 1)

  evalq(
    {
      Parent := new_class(
        parent = class_double,
        constructor = \(x = 4) new_object(x)
      )
    },
    dep
  )
  expect_identical(S7_data(convert(dep$Parent(x = 1), Child)), 4)
  expect_identical(S7_data(convert(dep$Parent(x = 1), Grandchild)), 4)

  LaterChild := new_class(parent = dep$Parent, package = "extension")
  evalq(Parent := new_class(parent = class_double), dep)
  expect_identical(S7_data(convert(dep$Parent(.data = 7), LaterChild)), 7)

  # The same export can move from the namespace into its imports.
  assign("Parent", dep$Parent, envir = parent.env(dep))
  rm("Parent", envir = dep)
  parent <- getExportedValue("dep", "Parent")(.data = 5)
  expect_identical(S7_data(convert(parent, Child)), 5)
})

test_that("downcasts leave data to custom constructors without a data argument", {
  Parent := new_class(parent = class_double)
  Custom := new_class(
    parent = Parent,
    properties = list(label = class_character),
    constructor = \(...) new_object(Parent(.data = 2), ...)
  )

  child <- convert(Parent(.data = 1), Custom, label = "child")
  expect_identical(S7_data(child), 2)
  expect_identical(child@label, "child")
})

test_that("downcasts stop forwarding `.data` at a custom S7 constructor", {
  Parent := new_class(parent = class_double)
  FromTable := new_class(
    parent = Parent,
    constructor = function(.data = data.frame(value = 0)) {
      new_object(Parent(.data = .data$value))
    }
  )
  Child := new_class(parent = FromTable)
  from <- Parent(.data = 7)

  expect_identical(S7_data(convert(from, FromTable)), 0)
  expect_identical(S7_data(convert(from, Child)), 0)
  override <- data.frame(value = 3)
  expect_identical(S7_data(convert(from, Child, .data = override)), 3)
})

test_that("downcasts from property-only classes leave constructor data defaults alone", {
  Parent := new_class()
  Child := new_class(
    parent = Parent,
    properties = list(label = class_character),
    constructor = \(.data = "child") new_object(Parent(), label = .data)
  )

  expect_identical(convert(Parent(), Child)@label, "child")
})

test_that("downcasts keep the default for a property named `.data`", {
  Parent := new_class(parent = class_double)
  Child := new_class(
    parent = Parent,
    properties = list(.data = new_property(class = class_double, default = 2))
  )

  child <- convert(Parent(.data = 1), Child)
  expect_identical(child@.data, 2)
})

test_that("downcasts can pass data when a `.data` property is read-only", {
  Parent := new_class(parent = class_double)
  Child := new_class(
    parent = Parent,
    properties = list(
      .data = new_property(class_integer, getter = \(self) length(self))
    )
  )
  data <- c(1, 2, 3)

  child <- convert(Parent(.data = data), Child)
  expect_identical(S7_data(child), data)
  expect_identical(child@.data, 3L)
})

test_that("downcasts keep inherited `.data` property defaults under a getter", {
  Base := new_class(parent = class_double)
  Parent := new_class(
    parent = Base,
    properties = list(.data = new_property(class = class_double, default = 2))
  )
  Child := new_class(
    parent = Parent,
    properties = list(
      .data = new_property(class_double, getter = \(self) sum(self))
    )
  )

  child <- convert(Base(.data = 1), Child)
  expect_identical(S7_data(child), 2)
  expect_identical(child@.data, 2)
})

test_that("downcasts construct a subclass without reusing the source environment", {
  Parent := new_class(parent = class_environment)
  Child := new_class(parent = Parent)
  parent <- Parent()

  child <- convert(parent, Child)
  expect_identical(S7_class(child), Child)
  expect_identical(S7_class(parent), Parent)
})

test_that("fallback downcasts use declared ancestry and drop extra S3 classes", {
  Parent := new_class(properties = list(x = class_double), package = NULL)
  Child := new_class(
    parent = Parent,
    properties = list(y = class_double),
    package = NULL
  )
  parent <- Parent(x = 1)
  # ggplot2 adds "gg" to its S7 plot objects for S3 compatibility.
  # That extra tag previously blocked ggplotplus from converting plots
  # to an S7 subclass, despite the declared parent-child relationship.
  class(parent) <- c(class(parent), "extra_s3_class")

  child <- convert(parent, to = Child, y = 2)
  expect_identical(S7_class(child), Child)
  expect_identical(class(child), c("Child", "Parent", "S7_object"))
  expect_equal(props(child), list(x = 1, y = 2))
})

test_that("fallback convert rejects sibling S7 objects with additional S3 classes", {
  Parent := new_class(package = NULL)
  Child := new_class(parent = Parent, package = NULL)
  Sibling := new_class(parent = Parent, package = NULL)
  child <- Child()
  class(child) <- c(class(child), "extra_s3_class")

  expect_snapshot(convert(child, to = Sibling), error = TRUE)
})

test_that("fallback convert can convert to S3 class", {
  local_methods(convert)
  factor2 := new_class(
    parent = class_factor,
    properties = list(x = class_double)
  )
  obj <- factor2(.data = 1L, levels = "x", x = 1)
  attr(obj, "S7_version") <- "user"
  obj <- convert(obj, to = class_factor)
  expect_equal(class(obj), "factor")
  expect_null(attr(obj, "_S7_version", exact = TRUE))
  expect_identical(attr(obj, "S7_version", exact = TRUE), "user")
  expect_false(S7_inherits(obj))
  expect_equal(attr(obj, "x"), NULL)
})

test_that("fallback convert can convert to base type", {
  local_methods(convert)
  character2 := new_class(
    parent = class_character,
    properties = list(x = class_double)
  )
  obj <- character2(.data = "x", x = 1)
  attr(obj, "S7_version") <- "user"
  obj <- convert(obj, to = class_character)
  expect_equal(attr(obj, "class"), NULL)
  expect_null(attr(obj, "_S7_version", exact = TRUE))
  expect_identical(attr(obj, "S7_version", exact = TRUE), "user")
  expect_false(S7_inherits(obj))
  expect_equal(attr(obj, "x"), NULL)
})

test_that("fallback convert can convert_up() an S4-derived S7 object to an S4 object", {
  local_S4_classes()
  setClass("ParentS4", slots = list(x = "numeric"))

  ChildS7 <- new_class(
    "ChildS7",
    parent = getClass("ParentS4"),
    properties = list(y = class_character),
    package = NULL
  )

  child <- ChildS7(x = 10, y = "a")
  parent <- convert(child, to = getClass("ParentS4"))

  expect_true(isS4(parent))
  expect_equal(class(parent)[1L], "ParentS4")
  expect_equal(methods::slot(parent, "x"), 10)
})

test_that("fallback convert can use explicit S4 coercion via methods::as", {
  local_S4_classes()
  setClass("ParentS4", slots = list(x = "numeric"))
  setClass("UnrelatedS4", slots = list(z = "character"))

  ChildS7 <- new_class(
    "ChildS7",
    parent = getClass("ParentS4"),
    properties = list(y = class_character),
    package = NULL
  )

  setAs("ChildS7", "UnrelatedS4", function(from) {
    new("UnrelatedS4", z = as.character(methods::slot(from, "x")))
  })

  child <- ChildS7(x = 42, y = "a")
  res <- convert(child, to = getClass("UnrelatedS4"))

  expect_true(isS4(res))
  expect_equal(class(res)[1L], "UnrelatedS4")
  expect_equal(methods::slot(res, "z"), "42")
})

test_that("is_down_cast() is TRUE only when `to` descends from `from` (#509)", {
  Base := new_class(package = NULL)
  A := new_class(
    Base,
    package = NULL,
    properties = list(x = class_numeric)
  )
  B := new_class(Base, package = NULL)
  B_child := new_class(B, package = NULL)

  # `to` is a descendant of `from`'s class
  expect_equal(is_down_cast(Base(), A), TRUE)
  expect_equal(is_down_cast(B(), B_child), TRUE)

  # base/S3 `from` to an S7 class that extends it
  my_logical := new_class(class_logical, package = NULL)
  my_factor := new_class(class_factor, package = NULL)
  expect_equal(is_down_cast(TRUE, my_logical), TRUE)
  expect_equal(is_down_cast(factor("a"), my_factor), TRUE)

  # Siblings share an ancestor but neither descends from the other
  expect_equal(is_down_cast(B(), A), FALSE)
  expect_equal(is_down_cast(B_child(), A), FALSE)
})

test_that("is_down_cast() requires `from`'s classes to be contiguous and ordered", {
  # All of `from`'s classes are in `to`, but not contiguously
  gappy <- structure(list(), class = c("a", "b"))
  expect_equal(is_down_cast(gappy, new_S3_class(c("a", "x", "b"))), FALSE)

  # All of `from`'s classes are in `to`, but in the wrong order
  reversed <- structure(list(), class = c("b", "a"))
  expect_equal(is_down_cast(reversed, new_S3_class(c("a", "b"))), FALSE)

  # A genuine contiguous, ordered run succeeds
  ok <- structure(list(), class = c("b", "a"))
  expect_equal(is_down_cast(ok, new_S3_class(c("c", "b", "a"))), TRUE)
})

test_that("convert() is idempotent when `from` is an instance of `to` (#429)", {
  local_methods(convert)
  Foo := new_class(package = NULL, properties = list(x = class_numeric))

  # A registered method must not override the idempotent behaviour
  method(convert, list(Foo, Foo)) <- function(from, to, ...) Foo(x = -1)

  foo <- Foo(x = 1)
  expect_identical(convert(foo, Foo), foo)
})

test_that("convert() upcasting ignores inherited downcasting methods (#429)", {
  local_methods(convert)
  A := new_class(package = NULL, properties = list(x = class_numeric))
  B := new_class(A, package = NULL, properties = list(y = class_numeric))
  C := new_class(B, package = NULL, properties = list(z = class_numeric))

  # A method to downcast a superclass `A` to `B`
  method(convert, list(A, B)) <- function(from, to, ...) B(y = -1)

  # Converting a `C` (which is already a `B`) to `B` should upcast, dropping
  # `z`, rather than dispatch to the inherited `A` -> `B` downcasting method.
  b <- convert(C(x = 1, y = 2, z = 3), to = B)
  expect_equal(S7_class(b), B)
  expect_equal(b@x, 1)
  expect_equal(b@y, 2)

  # A more specific upcasting method is still used
  method(convert, list(C, B)) <- function(from, to, ...) B(y = 99)
  expect_equal(convert(C(x = 1, y = 2, z = 3), to = B)@y, 99)
})

test_that("convert() errors when upcasting to an abstract class (#680)", {
  Foo := new_class(abstract = TRUE, package = NULL)
  Bar := new_class(Foo, package = NULL)
  expect_snapshot(convert(Bar(), Foo), error = TRUE)
})

test_that("convert() errors when upcasting to an abstract S3 class (#686)", {
  # An S3 class without a constructor is abstract, so there's no valid instance
  # to convert to (class_POSIXt is the union of POSIXct and POSIXlt)
  expect_snapshot(convert(.POSIXct(1), class_POSIXt), error = TRUE)
})

test_that("convert() falls back to as.*() for base type targets (#472)", {
  expect_identical(convert(1.5, class_character), "1.5")
  expect_identical(convert(c("1", "2"), class_integer), c(1L, 2L))
  expect_identical(convert(0:1, class_logical), c(FALSE, TRUE))
  expect_identical(convert(1:2, class_double), c(1, 2))
  expect_identical(convert(c("a", "b"), class_list), list("a", "b"))
  expect_identical(convert("x", class_name), as.name("x"))
})

test_that("base type fallback sits below user methods and inheritance", {
  local_methods(convert)

  # A registered method wins over the as.*() fallback
  Txt := new_class(class_character, package = NULL)
  method(convert, list(Txt, class_character)) <- function(from, to, ...) {
    "custom"
  }
  expect_equal(convert(Txt("hi"), class_character), "custom")

  # Upcasting to a base type strips the S7 wrapper rather than calling as.*()
  method(convert, list(Txt, class_character)) <- NULL
  obj <- convert(Txt("hi"), class_character)
  expect_false(S7_inherits(obj))
  expect_null(attributes(obj))
  expect_identical(obj, "hi")
})

test_that("convert_lazy() leaves `from` untouched if it inherits from `to` (#428)", {
  foo1 := new_class(properties = list(x = class_double))
  foo2 := new_class(foo1, properties = list(y = class_double))

  obj <- foo2(x = 1, y = 2)
  expect_identical(convert_lazy(obj, to = foo1), obj)
  expect_identical(convert_lazy(obj, to = foo2), obj)
})

test_that("convert_lazy() falls back to convert() when not a subtype", {
  foo1 := new_class(properties = list(x = class_double))
  foo2 := new_class(foo1, properties = list(y = class_double))

  # Downcasting still works
  obj <- convert_lazy(foo1(x = 1), to = foo2, y = 2.5)
  expect_equal(obj, foo2(x = 1, y = 2.5))

  # As does casting to a different class
  expect_identical(convert_lazy(1.5, class_character), "1.5")
})
