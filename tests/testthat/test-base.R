test_that("validation uses typeof", {
  expect_equal(class_integer$validator(1L), NULL)
  expect_equal(class_integer$validator(factor()), NULL)
  expect_snapshot(class_integer$validator(TRUE))

  expect_equal(class_function$validator(`[`), NULL)
  expect_equal(class_function$validator(sum), NULL)
  expect_equal(class_function$validator(mean), NULL)
})

test_that("base class display as expected", {
  expect_snapshot({
    class_integer
    str(class_integer)
  })
})

test_that("classes can inherit from base types", {
  base_classes <- c(class_vector$classes, list(class_function))

  for (class in base_classes) {
    foo <- new_class("foo", parent = class)
    expect_error(foo(), NA)
  }
})


test_that("Base classes can be a parent class", {

  expect_no_error({
    Foo := new_class(class_logical)
    Foo()
    Foo(TRUE)
  })
  expect_error(Foo(1), "must be <logical> not <double>")

  expect_no_error({
    Foo := new_class(class_integer)
    Foo()
    Foo(1L)
  })
  expect_error(Foo(1), "must be <integer> not <double>")

  expect_no_error({
    Foo := new_class(class_double)
    Foo()
    Foo(1)
  })
  expect_error(Foo(1L), "must be <double> not <integer>")

  expect_no_error({
    Foo := new_class(class_complex)
    Foo()
    Foo(1 + 1i)
  })
  expect_error(Foo(1), "must be <complex> not <double>")

  expect_no_error({
    Foo := new_class(class_character)
    Foo()
    Foo("a")
  })
  expect_error(Foo(1), "must be <character> not <double>")

  expect_no_error({
    Foo := new_class(class_raw)
    Foo()
    Foo(charToRaw("a"))
  })
  expect_error(Foo(1), "must be <raw> not <double>")

  expect_no_error({
    Foo := new_class(class_list)
    Foo()
    Foo(list())
  })
  expect_error(Foo(1), "must be <list> not <double>")

  expect_no_error({
    Foo := new_class(class_expression)
    Foo()
    Foo(expression(1))
  })
  expect_error(Foo(1), "must be <expression> not <double>")

  expect_no_error({
    Foo := new_class(class_call)
    Foo()
    Foo(quote(a()))
  })
  expect_error(Foo(1), "must be <call> not <double>")

  expect_no_error({
    Foo := new_class(class_function)
    Foo()
    Foo(identity)
  })
  expect_error(Foo(1), "must be <function> not <double>")

  # union types cannot be a parent:
  #
  # class_numeric
  # class_atomic
  # class_vector
  # class_language

  # class_name cannot be a parent because:
  #   'Error: cannot set attribute on a symbol'

  # class_environment cannot currently be a parent
  # (this is expected to change in the future)

})


test_that("All base classes can be a property class", {
  expect_no_error({
    Foo := new_class(properties = list(x = class_logical))
    Foo(x = TRUE)
  })
  expect_error(Foo(x = 1), "@x must be <logical>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_integer))
    Foo(x = 1L)
  })
  expect_error(Foo(x = 1), "@x must be <integer>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_double))
    Foo(x = 1)
  })
  expect_error(Foo(x = 1L), "@x must be <double>, not <integer>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_complex))
    Foo(x = 1 + 1i)
  })
  expect_error(Foo(x = 1), "@x must be <complex>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_character))
    Foo(x = "a")
  })
  expect_error(Foo(x = 1), "@x must be <character>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_raw))
    Foo(x = charToRaw("a"))
  })
  expect_error(Foo(x = 1), "@x must be <raw>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_list))
    Foo(x = list())
  })
  expect_error(Foo(x = 1), "@x must be <list>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_expression))
    Foo(x = expression(1))
  })
  expect_error(Foo(x = 1), "@x must be <expression>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_call))
    Foo(x = quote(a()))
  })
  expect_error(Foo(x = 1), "@x must be <call>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_function))
    Foo(x = identity)
  })
  expect_error(Foo(x = 1), "@x must be <function>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_name))
    Foo(x = quote(a))
  })
  expect_error(Foo(x = 1), "@x must be <name>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_environment))
    Foo(x = new.env())
  })
  expect_error(Foo(x = 1), "@x must be <environment>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_atomic))
    Foo(x = 1)
  })
  expect_error(Foo(x = list(TRUE)), "@x must be .*, not <list>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_vector))
    Foo(x = 1)
  })
  expect_error(Foo(x = quote(x)), "@x must be .*, not <symbol>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_language))
    Foo(x = quote(a()))
  })
  expect_error(Foo(x = 1), "@x must be .*, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_numeric))
    Foo(x = 1)
  })
  expect_error(Foo(x = TRUE), "@x must be .*, not <logical>")

})


test_that("Base S3 classes can be parents", {

  expect_no_error({
    Foo := new_class(class_factor)
    Foo()
    Foo(1L, levels = letters[1:3])
    Foo(factor(letters[1:3]))
  })

  expect_no_error({
    Foo := new_class(class_Date)
    Foo()
    Foo(Sys.Date())
    Foo(rep(Sys.Date(), 3))
    Foo(1)
  })
  expect_error(Foo("a"), "Underlying data must be numeric")

  expect_no_error({
    Foo := new_class(class_POSIXct)
    Foo()
    Foo(Sys.time())
    Foo(rep(Sys.time(), 3))
    Foo(1)
  })
  expect_error(Foo("a"), "Underlying data must be numeric")

  expect_no_error({
    Foo := new_class(class_data.frame)
    Foo()
    Foo(data.frame(x = 1))
    Foo(list(x = 1))
    Foo(list(x = 1), "rowname")
  })
  expect_error(Foo(list(x = 1:3, y = 1:4)),
               "all variables should have the same length")

  # expect_no_error({
  #   Foo := new_class(class_matrix)
  #   Foo(1:4, nrow = 2)
  #   Foo(NA)
  #   Foo(matrix(1:4, nrow = 2))
  # })

  # expect_no_error({
  #   Foo := new_class(class_array)
  #
  #   Foo(array(1:4, dim = c(2, 2)))
  #   Foo(1:4, dim = c(2, 2))
  #
  #   Foo(array(1:24, dim = c(2, 3, 4)))
  #   Foo(1:24, dim = c(2, 3, 4))
  #
  #   Foo(array(1))
  #   Foo(1)
  # })

  expect_no_error({
    Foo := new_class(class_formula)
    Foo(~ x)
    Foo("~ x")
    Foo(call("~", 1, 2))
    Foo(quote(~x))
  })

})

test_that("Base S3 classes can be properties", {

  expect_no_error({
    Foo := new_class(properties = list(x = class_factor))
    Foo(x = factor())
  })
  expect_error(Foo(x = 1), "@x must be S3<factor>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_data.frame))
    Foo(x = data.frame())
  })
  expect_error(Foo(x = 1), "@x must be S3<data.frame>, not <double>")

  # expect_no_error({
  #   Foo := new_class(properties = list(x = class_matrix))
  #   Foo(x = matrix())
  # })
  # expect_error(Foo(x = 1), "@x must be S3<matrix>, not <double>")

  # expect_no_error({
  #   Foo := new_class(properties = list(x = class_array))
  #   Foo(x = array())
  # })
  # expect_error(Foo(x = 1), "@x must be S3<array>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_formula))
    Foo(x = ~ x)
  })
  expect_error(Foo(x = 1), "@x must be S3<formula>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_Date))
    Foo(x = Sys.Date())
  })
  expect_error(Foo(x = 1), "@x must be S3<Date>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_POSIXct))
    Foo(x = Sys.time())
  })
  expect_error(Foo(x = 1), "@x must be S3<POSIXct/POSIXt>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_POSIXlt))
    Foo(x = as.POSIXlt(Sys.time()))
  })
  expect_error(Foo(x = 1), "@x must be S3<POSIXlt/POSIXt>, not <double>")

  expect_no_error({
    Foo := new_class(properties = list(x = class_POSIXt))
    Foo(x = Sys.time())
    Foo(x = as.POSIXlt(Sys.time()))
  })
  expect_error(Foo(x = 1), "@x must be S3<POSIXt>, not <double>")

})


test_that("inherits() works with S7_base_class", {
  # nameOfClass() introduced in R 4.3
  skip_if(getRversion() < "4.3")

  # test nameOfClass.S7_base_class
  expect_true(inherits("foo", class_character))

  Foo := new_class(class_character)
  expect_true(inherits(Foo(), "character"))
  expect_true(inherits(Foo(), class_character))
})
