describe("property retrieval", {
  it("retrieves the properties that exist & errors otherwise", {
    foo := new_class(
      properties = list(xyz = class_double),
      package = NULL
    )
    obj <- foo(1)
    expect_equal(prop(obj, "xyz"), 1)
    expect_equal(obj@xyz, 1)

    expect_snapshot(error = TRUE, {
      prop(obj, "x")
      obj@x
    })
  })
  it("evalutes dynamic properties", {
    foo := new_class(
      properties = list(
        x = new_property(getter = function(self) 1)
      )
    )
    obj <- foo()
    expect_equal(prop(obj, "x"), 1)
    expect_equal(obj@x, 1)
  })

  it("falls back to `base::@` for non-S7 objects", {
    expect_error("foo"@blah, 'object of.+class.+"character"')
    expect_error(NULL@blah, 'object of.+class.+"NULL"')
  })
})

describe("prop setting", {
  it("can set a property", {
    foo := new_class(properties = list(xyz = class_double))
    obj <- foo(1)

    prop(obj, "xyz") <- 2
    expect_equal(obj@xyz, 2)

    obj@xyz <- 3
    expect_equal(obj@xyz, 3)
  })

  it("can set dynamic properties", {
    foo := new_class(
      properties = list(
        x = new_property(setter = function(self, value) {
          self@x <- value * 2
          self
        })
      )
    )
    obj <- foo()
    obj@x <- 1
    expect_equal(obj@x, 2)
  })

  it("can't set read-only properties", {
    foo := new_class(
      package = NULL,
      properties = list(
        x = new_property(getter = function(self) 1)
      )
    )
    obj <- foo()
    expect_snapshot(obj@x <- 1, error = TRUE)
  })

  it("errors if the property doesn't exist or is wrong class", {
    foo := new_class(properties = list(x = class_double), package = NULL)
    expect_snapshot(error = TRUE, {
      obj <- foo(123)
      obj@foo <- 10
      obj@x <- "x"
    })
  })

  it("validates all attributes if custom setter", {
    foo := new_class(
      package = NULL,
      properties = list(
        x = new_property(
          class_double,
          setter = function(self, value) {
            self@x <- 123
            self@y <- value
            self
          }
        ),
        y = new_property(class_double)
      )
    )
    expect_snapshot(error = TRUE, {
      obj <- foo(y = 123, x = 123)
      obj@x <- "x"
    })
  })

  it("validates once after custom setter", {
    times_validated <- 0L
    `add<-` <- `+`
    custom_setter <- function(self, value) {
      self@x <- as.double(value)
      self
    }
    foo2 := new_class(
      properties = list(x = new_property(class_double, setter = custom_setter)),
      validator = function(self) {
        add(times_validated) <<- 1L
        character()
      }
    )
    obj <- foo2("123")
    expect_equal(times_validated, 1)
    obj@x <- "456"
    expect_equal(times_validated, 2)
  })

  it("validates once with recursive property setters", {
    times_validated <- 0L
    `add<-` <- `+`
    foo := new_class(
      properties = list(
        x = new_property(setter = function(self, value) {
          self@x <- value
          self@y <- paste0(value, "_set_by_x_setter")
          self
        }),
        y = new_property(setter = function(self, value) {
          self@y <- value
          self@z <- paste0(value, "_set_by_y_setter")
          self
        }),
        z = new_property(class_character)
      ),
      validator = function(self) {
        add(times_validated) <<- 1L
        NULL
      }
    )
    out <- foo()
    expect_equal(times_validated, 1L)

    out@x <- "VAL"
    expect_equal(times_validated, 2L)
    expect_equal(out@z, "VAL_set_by_x_setter_set_by_y_setter")
  })

  it("does not run the check or validation functions if check = FALSE", {
    foo := new_class(properties = list(x = class_double))
    obj <- foo(123)
    prop(obj, "x", check = FALSE) <- "foo"
    expect_equal(obj@x, "foo")
  })

  it("falls back to `base::@` for non-S7 objects", {
    x <- "foo"
    expect_error(x@blah <- "bar", "is not a slot in class")
  })

  it("gives informative error if setter doesn't return an S7 object (#416)", {
    foo := new_class(
      package = NULL,
      properties = list(
        x = new_property(
          class = class_integer,
          setter = function(self, value) {
            self@x <- as.integer(value)
          }
        )
      )
    )
    expect_snapshot(foo(x = 1.1), error = TRUE)
  })

  it("setter can receive the property name (#552)", {
    property_colour <- new_property(
      class = class_character,
      setter = function(self, name, value) {
        prop(self, name) <- as.character(value)
        self
      }
    )
    Rectangle := new_class(
      properties = list(colour = property_colour, fill = property_colour),
      package = NULL
    )

    r <- Rectangle(colour = "red", fill = 99L)
    expect_equal(r@colour, "red")
    expect_equal(r@fill, "99")

    r@colour <- 42L
    expect_equal(r@colour, "42")
  })
})

describe("props<-", {
  it("validates after setting all properties", {
    foo := new_class(
      properties = list(x = class_double, y = class_double),
      validator = function(self) if (self@x > self@y) "bad"
    )

    obj <- foo(1, 2)
    props(obj) <- list(x = 5, y = 10)
    expect_equal(obj@x, 5)
    expect_equal(obj@y, 10)
  })

  it("`check = FALSE` skip validation", {
    foo := new_class(
      properties = list(x = class_double, y = class_double),
      validator = function(self) if (self@x > self@y) "bad"
    )

    obj <- foo(1, 2)
    props(obj, check = FALSE) <- list(x = 2, y = 1)
    expect_equal(obj@x, 2)
    expect_equal(obj@y, 1)
    expect_snapshot(validate(obj), error = TRUE)
  })

  it("has ordinary syntax in set_props()", {
    foo := new_class(properties = list(x = class_double))
    obj1 <- foo(1)
    obj2 <- set_props(obj1, x = 2)

    expect_equal(obj1@x, 1)
    expect_equal(obj2@x, 2)
  })

  it("set_props() can set a property named `object` (#423)", {
    Foo := new_class(properties = list(object = class_double))
    obj <- set_props(Foo(1), object = 2)
    expect_equal(obj@object, 2)
  })

  it("set_props() accepts a single unnamed list (#497)", {
    Foo := new_class(properties = list(x = class_double, y = class_double))
    obj <- set_props(Foo(1, 2), list(x = 3, y = 4))
    expect_equal(obj@x, 3)
    expect_equal(obj@y, 4)
  })

  it("set_props() errors if single unnamed list has unnamed elements (#497)", {
    Foo := new_class(properties = list(x = class_double))
    expect_snapshot(set_props(Foo(1), list(2)), error = TRUE)
  })

  it("set_props() skip validation with `.check = FALSE`", {
    foo := new_class(
      properties = list(x = class_double, y = class_double),
      validator = function(self) if (self@x > self@y) "bad"
    )

    obj <- foo(1, 2)
    obj2 <- set_props(obj, x = 2, y = 1, .check = FALSE)
    expect_equal(obj2@x, 2)
    expect_equal(obj2@y, 1)
    expect_snapshot(validate(obj2), error = TRUE)
  })
})

describe("property access", {
  it("access en masse", {
    foo := new_class(
      properties = list(x = class_numeric, y = class_numeric)
    )
    x <- foo(x = 1, y = 2)
    expect_equal(prop_names(x), c("x", "y"))
    expect_equal(props(x), list(x = 1, y = 2))
    expect_true(prop_exists(x, "x"))
    expect_true(prop_exists(x, "y"))
    expect_false(prop_exists(x, "z"))
  })

  it("can access dynamic properties", {
    foo := new_class(
      properties = list(
        x = new_property(getter = function(self) 10),
        y = new_property()
      )
    )
    x <- foo(y = 2)
    expect_equal(props(x), list(x = 10, y = 2))
  })

  it("can with property-less object", {
    x <- new_class("x")()
    expect_equal(prop_names(x), character())
    expect_equal(props(x), named_list())
    expect_equal(prop_exists(x, "y"), FALSE)
  })

  it("ignore attributes that are not properties", {
    x <- new_class("x")()
    attr(x, "extra") <- 1

    expect_equal(prop_names(x), character())
    expect_equal(props(x), named_list())
    expect_false(prop_exists(x, "extra"))
  })
})


test_that("properties can be NULL", {
  foo := new_class(properties = list(x = class_any))
  x <- foo(x = NULL)
  expect_equal(x@x, NULL)
  x@x <- 1
  expect_equal(x@x, 1)
  x@x <- NULL
  expect_equal(x@x, NULL)
  expect_equal(prop_names(x), "x")
  expect_equal(props(x), list(x = NULL))
})

test_that("properties can use names with special base R handlers (#579)", {
  foo := new_class(
    properties = list(
      names = class_character,
      dim = class_integer,
      dimnames = class_list,
      class = class_character,
      comment = class_character,
      row.names = class_character,
      x = class_numeric
    )
  )
  obj <- foo(
    names = c("a", "b"),
    dim = 2L,
    dimnames = list("a"),
    class = "z",
    comment = "hi",
    row.names = "r",
    x = 1
  )

  expect_equal(obj@names, c("a", "b"))
  expect_equal(obj@dim, 2L)
  expect_equal(obj@class, "z")
  expect_equal(obj@comment, "hi")

  # special-named props don't clobber the underlying dispatch class
  expect_equal(class(obj), class_dispatch(foo))

  # props are stored under "_"-prefixed attributes
  expect_equal(attr(obj, "_names"), c("a", "b"))
  expect_null(attr(obj, "names", exact = TRUE))

  # getting and setting via prop() works too
  expect_equal(prop(obj, "names"), c("a", "b"))
  prop(obj, "names") <- "c"
  expect_equal(obj@names, "c")
})

test_that("special-named property is independent of base attribute (#579)", {
  foo := new_class(
    parent = class_double,
    properties = list(names = class_character)
  )
  obj <- foo(c(a = 1, b = 2), names = "label")

  expect_equal(obj@names, "label")
  expect_equal(names(obj), c("a", "b"))
  expect_equal(names(S7_data(obj)), c("a", "b"))
})

describe("prop_info()", {
  it("returns a data frame describing each property", {
    foo := new_class(
      properties = list(
        a = class_character,
        b = new_property(class_numeric, default = 1),
        c = new_property(getter = function(self) 1),
        d = new_property(class_numeric, setter = function(self, value) self),
        e = new_property(
          class_numeric,
          validator = function(value) NULL
        )
      )
    )

    info <- prop_info(foo)
    expect_s3_class(info, "data.frame")
    expect_equal(.row_names_info(info), -5L) # numeric row names
    expect_equal(info$name, c("a", "b", "c", "d", "e"))
    expect_equal(
      info$class,
      c(
        "<character>",
        "<integer> or <double>",
        "<ANY>",
        "<integer> or <double>",
        "<integer> or <double>"
      )
    )
    expect_equal(unname(info$default), I(list(NULL, 1, NULL, NULL, NULL)))
    expect_equal(info$getter, c(FALSE, FALSE, TRUE, FALSE, FALSE))
    expect_equal(info$setter, c(FALSE, FALSE, FALSE, TRUE, FALSE))
    expect_equal(info$validator, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  })

  it("works on instances", {
    foo := new_class(properties = list(x = class_numeric))
    expect_equal(prop_info(foo(1)), prop_info(foo))
  })

  it("returns a zero-row data frame when there are no properties", {
    foo := new_class()
    info <- prop_info(foo)
    expect_s3_class(info, "data.frame")
    expect_equal(nrow(info), 0)
    expect_named(
      info,
      c("name", "default", "class", "getter", "setter", "validator")
    )
  })
})

describe("new_property()", {
  it("validates getter and settor", {
    expect_snapshot(error = TRUE, {
      new_property(getter = function(x) {})
      new_property(setter = function(x, y, z) {})
    })
  })

  it("validates default", {
    expect_snapshot(error = TRUE, {
      new_property(class_integer, default = "x")
    })
  })

  it("displays nicely", {
    x <- new_property(class_integer, name = "foo")
    expect_snapshot({
      print(x)
      str(list(x))
    })
  })
})

test_that("properties can be base, S3, S4, S7, or S7 union", {
  class_S7 := new_class(package = NULL)
  class_S4 <- methods::setClass("class_S4", slots = c(x = "numeric"))

  my_class := new_class(
    package = NULL,
    properties = list(
      anything = class_any,
      null = NULL,
      base = class_integer,
      S3 = class_factor,
      S4 = class_S4,
      S7 = class_S7,
      S7_union = new_union(class_integer, class_logical)
    )
  )
  expect_snapshot(my_class)
  my_obj <- my_class(
    anything = TRUE,
    null = NULL,
    base = 1L,
    S3 = factor(),
    S4 = class_S4(x = 1),
    S7 = class_S7(),
    S7_union = 1L
  )

  # First check that we can set with out error
  expect_error(my_obj@base <- 2L, NA)
  expect_error(my_obj@S3 <- factor("x"), NA)
  expect_error(my_obj@S4 <- class_S4(x = 2), NA)
  expect_error(my_obj@S7 <- class_S7(), NA)
  expect_error(my_obj@S7_union <- 2L, NA)
  expect_error(my_obj@S7_union <- TRUE, NA)

  # Then capture the error messages for human inspection
  expect_snapshot(error = TRUE, {
    my_obj@null <- "x"
    my_obj@base <- "x"
    my_obj@S3 <- "x"
    my_obj@S4 <- "x"
    my_obj@S7 <- "x"
    my_obj@S7_union <- "x"
  })
})

test_that("as_properties normalises properties", {
  expect_equal(as_properties(NULL), list())
  expect_equal(
    as_properties(list(x = class_numeric)),
    list(x = new_property(class_numeric, name = "x"))
  )
  expect_equal(
    as_properties(list(x = new_property(class = class_numeric))),
    list(x = new_property(class_numeric, name = "x"))
  )
  expect_equal(
    as_properties(list(new_property(name = "y"))),
    list(y = new_property(name = "y"))
  )

  # list name wins
  expect_equal(
    as_properties(list(x = new_property(name = "y"))),
    list(x = new_property(name = "x"))
  )
})

test_that("as_properties() gives useful error messages", {
  expect_snapshot(error = TRUE, {
    as_properties(1)
    as_properties(list(1))
    as_properties(list(new_property(class_character)))
    as_properties(list(x = 1))
    as_properties(list(x = class_character, x = class_character))
  })
})

test_that("can validate with custom validator", {
  validate_scalar <- function(value) {
    if (length(value) != 1) {
      "must be length 1"
    }
  }
  prop <- new_property(class_integer, validator = validate_scalar)
  foo := new_class(package = NULL, properties = list(x = prop))
  expect_snapshot(error = TRUE, {
    f <- foo(x = 1L)
    f@x <- 1:2

    foo(x = 1:2)
  })
})

test_that("property validation runs the class's own validator", {
  Foo := new_class(package = NULL, properties = list(x = class_factor))

  # A malformed factor passes the structural check (its class is "factor")
  # but fails the factor validator because it has too few levels.
  bad <- structure(1:3, levels = "a", class = "factor")
  expect_snapshot(Foo(x = bad), error = TRUE)
})

test_that("property validation runs an S4 class's validity method", {
  PosNum <- methods::setClass(
    "PosNum",
    slots = c(n = "numeric"),
    validity = function(object) {
      if (object@n <= 0) "n must be positive" else TRUE
    }
  )
  on.exit(S4_remove_classes("PosNum"))
  Foo := new_class(package = NULL, properties = list(x = PosNum))

  # An S4 object that passes the structural check but fails its own validity
  # method is rejected
  bad <- PosNum(n = 1)
  bad@n <- -5
  expect_snapshot(Foo(x = bad), error = TRUE)
})

test_that("prop<- won't infinitly recurse on a custom setter", {
  chattily_sync_ab <- function(self, value) {
    cat("Starting syncup with value:", value, "\n")
    a_value <- paste0("a_", value)
    b_value <- paste0("b_", value)

    cat(sprintf('setting @a <- "%s"\n', a_value))
    self@a <- a_value

    cat(sprintf('setting @b <- "%s"\n', b_value))
    self@b <- b_value

    self
  }

  foo := new_class(
    properties = list(
      a = new_property(setter = chattily_sync_ab),
      b = new_property(setter = chattily_sync_ab)
    )
  )

  expect_snapshot({
    obj <- foo()
    obj@a <- "val"
  })
})

test_that("custom setters can invoke setters on non-self objects", {
  Transmitter := new_class(
    properties = list(
      message = new_property(setter = function(self, value) {
        cat("[tx] sending: ", value, "\n")
        receiver@message <<- value
        cat("[tx] saving last sent message.\n")
        self@message <- value
        cat("[tx] finished transmitting.\n")
        self
      })
    )
  )

  Receiver := new_class(
    properties = list(
      message = new_property(setter = function(self, value) {
        cat("[rx] receiving: ", value, "\n")
        self@message <- value
        cat("[rx] finished receiving.\n")
        self
      })
    )
  )

  expect_snapshot({
    receiver <- Receiver()
    transmitter <- Transmitter()

    transmitter@message <- "hello"
    expect_equal(receiver@message, "hello")

    transmitter@message <- "goodbye"
    expect_equal(receiver@message, "goodbye")
  })
})


test_that("custom getters don't infinitely recurse", {
  # https://github.com/RConsortium/S7/issues/403

  someclass := new_class(
    properties = list(
      someprop = new_property(
        class_character,
        getter = function(self) self@someprop,
        setter = function(self, value) {
          self@someprop <- toupper(value)
          self
        }
      )
    )
  )

  expect_equal(someclass("foo")@someprop, "FOO")
  x <- someclass()
  expect_equal(x@someprop, character())
  x@someprop <- "foo"
  expect_equal(x@someprop, "FOO")
})


test_that("custom setters can call custom getters", {
  # https://github.com/RConsortium/S7/issues/403

  someclass := new_class(
    properties = list(
      someprop = new_property(
        class_character,
        getter = function(self) self@someprop,
        setter = function(self, value) {
          self@someprop <- paste0(self@someprop, toupper(value))
          self
        }
      )
    )
  )

  x <- someclass("foo")
  expect_equal(x@someprop, "FOO")

  x <- someclass()
  expect_equal(x@someprop, character())

  x@someprop <- "foo"
  expect_equal(x@someprop, "FOO")

  x@someprop <- "foo"
  expect_equal(x@someprop, "FOOFOO")
})


test_that("custom getters don't evaulate call objects", {
  QuotedCall := new_class(
    class_call,
    properties = list(
      name = new_property(getter = function(self) {
        stopifnot(is.call(self))
        as.character(self[[1]])
      }),
      args = new_property(getter = function(self) {
        stopifnot(is.call(self))
        as.list(self)[-1]
      })
    ),
    constructor = function(x) {
      new_object(substitute(x))
    }
  )

  cl <- QuotedCall(stop("boom"))
  expect_equal(cl@name, "stop")
  expect_equal(cl@args, list("boom"))
})


test_that("custom setters don't evaulate call objects", {
  Call := new_class(
    class_call,
    properties = list(
      name = new_property(
        getter = function(self) {
          stopifnot(is.call(self))
          as.character(self[[1]])
        },
        setter = function(self, value) {
          stopifnot(is.call(self), is.name(value))
          self[[1]] <- value
          self
        }
      ),
      args = new_property(
        getter = function(self) {
          stopifnot(is.call(self))
          as.list(self)[-1]
        },
        setter = function(self, value) {
          stopifnot(is.call(self), is.list(value) || is.pairlist(value))
          # self[seq(2, length.out = length(value))] <- value
          # names(self) <- c("", names(value))
          # self
          out <- as.call(c(self[[1]], value))
          attributes(out) <- attributes(self)
          out
        }
      )
    ),
    constructor = function(name, ...) {
      new_object(as.call(c(as.name(name), ...)))
    }
  )

  cl <- Call("stop", "boom")
  expect_identical(cl@name, "stop")
  expect_identical(cl@args, list("boom"))

  abort <- stop
  cl@name <- quote(abort)
  expect_identical(cl@name, "abort")
  expect_identical(cl[[1]], quote(abort))

  cl@args <- pairlist("boom2")
  expect_identical(cl[[2]], "boom2")
  expect_identical(cl@args, list("boom2"))
  expect_identical(drop_attributes(cl), quote(abort("boom2")))

  cl@args <- alist(msg = "boom3", foo = bar, baz)
  expect_identical(cl@args, alist(msg = "boom3", foo = bar, baz))

  expect_identical(
    drop_attributes(cl),
    quote(abort(msg = "boom3", foo = bar, baz))
  )
})


test_that("errors from custom property accessors include a call that shows the class and prop name", {
  error <- FALSE
  foo := new_class(
    properties = list(
      x = new_property(
        setter = \(self, value) if (error) stop("nope") else self,
        getter = \(self) if (error) stop("nope") else 1
      )
    )
  )

  x <- foo()
  error <- TRUE
  getter_error <- tryCatch(x@x, error = identity)
  setter_error <- tryCatch(x@x <- 1, error = identity)
  expect_match(
    deparse1(conditionCall(getter_error)[[1]]),
    "<foo>@x",
    fixed = TRUE
  )
  expect_match(
    deparse1(conditionCall(setter_error)[[1]]),
    "<foo>@x",
    fixed = TRUE
  )
})

test_that("erroring getter/setter doesn't leave object in broken state", {
  # https://github.com/RConsortium/S7/issues/520

  Test := new_class(
    properties = list(
      a = new_property(
        getter = function(self) {
          if (self@a == 10) {
            stop("a is 10")
          }
          self@a
        },
        setter = function(self, value) {
          if (value == 11) {
            stop("value is 11")
          }
          self@a <- value
          self
        }
      )
    )
  )

  t <- Test(a = 10)
  expect_error(t@a, "a is 10")
  expect_error(t@a, "a is 10")

  expect_error(t@a <- 11, "value is 11")
  expect_error(t@a <- 11, "value is 11")

  t@a <- 1
  expect_equal(t@a, 1)
})

test_that("prop<- doesn't evaluate language values (#511)", {
  Cls := new_class(properties = list(r = class_any))

  foo <- Cls()
  foo@r <- as.symbol("x")
  expect_equal(foo@r, quote(x))

  foo@r <- quote(x + 1)
  expect_equal(foo@r, quote(x + 1))
})
