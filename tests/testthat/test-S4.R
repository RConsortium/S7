test_that("can work with classGenerators", {
  Foo := local_S4_class()
  expect_equal(S4_to_S7_class(Foo), getClass("Foo"))
})

test_that("S4_register registers an S7 class so it can be used with S4 methods", {
  on.exit(S4_remove_classes("S4regS7"))
  S4regS7 := new_class(package = NULL)
  S4regS7_S4 <- S4_register(S4regS7)
  expect_equal(S4regS7_S4, "S4regS7")
  expect_contains(methods::extends("S4regS7"), c("S4regS7", "S7_object"))
})

test_that("S4_register registers S7 classes with base parents", {
  defer(S4_remove_classes(c("S4regS7IntegerChild", "S4regS7Integer")))

  S4regS7Integer := new_class(parent = class_integer, package = NULL)

  expect_equal(S4_register(S4regS7Integer), "S4regS7Integer")
  expect_contains(methods::extends("S4regS7Integer"), "integer")

  object <- S4regS7Integer(.data = 1L)
  expect_equal(class(object), c("S4regS7Integer", "integer", "S7_object"))
  expect_true(methods::is(object, "integer"))
  expect_true(methods::is(object, "S7_object"))
  expect_true(methods::validObject(object))

  methods::setClass(
    "S4regS7IntegerChild",
    contains = S4_contains(S4regS7Integer),
    slots = list(y = "character")
  )
  child <- methods::new("S4regS7IntegerChild", .Data = 2L, y = "child")

  expect_equal(S7_class(child), S4regS7Integer)
  expect_equal(S7_inherits(child, S4regS7Integer), TRUE)
  expect_equal(methods::is(child, "S7_object"), TRUE)
  expect_equal(S7_data(child), 2L)
  expect_equal(methods::slot(child, "y"), "child")
  expect_equal(methods::validObject(child), TRUE)
})

test_that("S4_register constructs S4 subclasses with S3 data-part parents", {
  defer(S4_remove_classes(c("S4regS7FactorChild", "S4regS7Factor")))

  S4regS7Factor := new_class(parent = class_factor, package = NULL)
  S4_register(S4regS7Factor)
  methods::setClass(
    "S4regS7FactorChild",
    contains = S4_contains(S4regS7Factor),
    slots = list(y = "character")
  )
  child <- methods::new(
    "S4regS7FactorChild",
    .Data = 1L,
    levels = "a",
    y = "child"
  )

  expect_equal(S7_class(child), S4regS7Factor)
  expect_equal(S7_inherits(child, S4regS7Factor), TRUE)
  expect_equal(methods::is(child, "S7_object"), TRUE)
  expect_equal(methods::slot(child, ".Data"), 1L)
  expect_equal(methods::slot(child, "levels"), "a")
  expect_equal(methods::slot(child, "y"), "child")
})

test_that("S4_register preserves S3 data attributes for S4 subclasses", {
  defer(S4_remove_classes(c(
    "S4regS7DataFrameChild",
    "S4regS7DataFrame",
    "S4regS7FactorAttrsChild",
    "S4regS7FactorAttrs"
  )))

  S4regS7FactorAttrs := new_class(parent = class_factor, package = NULL)
  S4_register(S4regS7FactorAttrs)
  methods::setClass(
    "S4regS7FactorAttrsChild",
    contains = S4_contains(S4regS7FactorAttrs),
    slots = list(y = "character")
  )
  factor_child <- methods::new(
    "S4regS7FactorAttrsChild",
    .Data = 1L,
    levels = "a",
    y = "child"
  )

  expect_equal(S7_data(factor_child), factor("a"))

  S7_data(factor_child) <- factor("b")

  expect_equal(S7_data(factor_child), factor("b"))
  expect_equal(methods::slot(factor_child, "y"), "child")

  S4regS7DataFrame := new_class(parent = class_data.frame, package = NULL)
  S4_register(S4regS7DataFrame)
  methods::setClass(
    "S4regS7DataFrameChild",
    contains = S4_contains(S4regS7DataFrame),
    slots = list(y = "character")
  )
  df_child <- methods::new(
    "S4regS7DataFrameChild",
    .Data = list(a = 1),
    names = "a",
    row.names = 1L,
    y = "child"
  )

  expect_equal(S7_data(df_child), data.frame(a = 1))

  S7_data(df_child) <- data.frame(b = 2)

  expect_equal(S7_data(df_child), data.frame(b = 2))
  expect_equal(methods::slot(df_child, "y"), "child")
})

test_that("S4_register registers S7 classes with custom S3 data parents", {
  defer(S4_remove_classes("S4regS7S3Integer"))

  S4regS3Integer <- new_S3_class(
    c("S4regS3Integer", "integer"),
    constructor = function(.data = integer()) {
      structure(.data, class = c("S4regS3Integer", "integer"))
    }
  )
  S4regS7S3Integer := new_class(parent = S4regS3Integer, package = NULL)

  expect_equal(S4_register(S4regS7S3Integer), "S4regS7S3Integer")
  expect_contains(methods::slotNames("S4regS7S3Integer"), ".S3Class")
})

test_that("S4_register registers S7 property classes", {
  defer(S4_remove_classes(c(
    "S4regS7PropParent",
    "S4regS7PropChild",
    "S4regS7PropOther2",
    "S4regS7PropFoo",
    "S4regS7PropOther1"
  )))

  S4regS7PropOther1 := new_class(package = NULL)
  S4regS7PropFoo := new_class(
    properties = list(other = S4regS7PropOther1),
    package = NULL
  )

  expect_false(methods::isClass("S4regS7PropOther1"))
  expect_equal(S4_register(S4regS7PropFoo), "S4regS7PropFoo")
  expect_true(methods::isClass("S4regS7PropOther1"))
  expect_equal(
    as.character(methods::getClass("S4regS7PropFoo")@slots$other),
    "S4regS7PropOther1"
  )
  expect_true(methods::validObject(S4regS7PropFoo(
    other = S4regS7PropOther1()
  )))

  setClass("S4regS7PropParent", slots = list(x = "numeric"))
  S4regS7PropOther2 := new_class(package = NULL)
  S4regS7PropChild := new_class(
    parent = getClass("S4regS7PropParent"),
    properties = list(other = S4regS7PropOther2),
    package = NULL
  )
  object <- S4regS7PropChild(
    x = 1,
    other = S4regS7PropOther2()
  )

  expect_true(methods::isClass("S4regS7PropOther2"))
  expect_true(methods::validObject(object))
})

test_that("S4_register refreshes stale S7 property classes", {
  defer(S4_remove_classes(c(
    "S4regS7PropStaleHolder",
    "S4regS7PropStale"
  )))

  S4regS7PropStale := new_class(package = NULL)
  S4_register(S4regS7PropStale)

  S4regS7PropStale := new_class(
    properties = list(x = new_property(class_numeric, default = 1)),
    package = NULL
  )
  S4regS7PropStaleHolder := new_class(
    properties = list(prop = S4regS7PropStale),
    package = NULL
  )

  expect_no_error(S4_register(S4regS7PropStaleHolder))
  expect_identical(
    methods::validObject(S4regS7PropStaleHolder(
      prop = S4regS7PropStale()
    )),
    TRUE
  )
})

test_that("S4 parent slots declared as registered S7 classes use S7 classes", {
  defer(S4_remove_classes(c(
    "S4regSlotS7Child",
    "S4regSlotS7Holder",
    "S4regSlotS7Foo"
  )))

  S4regSlotS7Foo := new_class(package = NULL)
  S4_register(S4regSlotS7Foo)
  methods::setClass(
    Class = "S4regSlotS7Holder",
    slots = list(foo = "S4regSlotS7Foo")
  )
  S4regSlotS7Child := new_class(
    parent = methods::getClass("S4regSlotS7Holder"),
    package = NULL
  )

  object <- S4regSlotS7Child(foo = S4regSlotS7Foo())

  expect_equal(S4regSlotS7Child@properties$foo$class, S4regSlotS7Foo)
  expect_equal(S7_inherits(prop(object, "foo"), S4regSlotS7Foo), TRUE)
  expect_equal(methods::validObject(object), TRUE)
})

test_that("S4_contains requires prior S4 registration", {
  on.exit(S4_remove_classes("S4regContainsUnregistered"))
  S4regContainsUnregistered := new_class(package = NULL)

  expect_error(
    S4_contains(S4regContainsUnregistered),
    "has not been registered"
  )
  expect_false(methods::isClass("S4regContainsUnregistered"))

  S4_register(S4regContainsUnregistered)
  expect_equal(
    S4_contains(S4regContainsUnregistered),
    "S4regContainsUnregistered"
  )
})

test_that("S4_contains rejects old-style S7 registrations", {
  defer(S4_remove_classes("S4regContainsOldStyle"))

  S4regContainsOldStyle := new_class(
    properties = list(x = class_numeric),
    package = NULL
  )
  methods::setOldClass(c("S4regContainsOldStyle", "S7_object"))

  expect_snapshot(error = TRUE, {
    S4_contains(S4regContainsOldStyle)
  })
})

test_that("S4_contains rejects unrelated S4 classes with the same name", {
  defer(S4_remove_classes("S4regContainsCollision"))
  setClass("S4regContainsCollision", slots = list(x = "numeric"))
  S4regContainsCollision := new_class(package = NULL)

  expect_snapshot(
    S4_contains(S4regContainsCollision),
    error = TRUE
  )
})

test_that("S4_contains rejects stale S4 registrations", {
  defer(S4_remove_classes(c(
    "S4regStaleChild",
    "S4regStale"
  )))

  S4regStale := new_class(
    properties = list(x = class_numeric),
    package = NULL
  )
  S4_register(S4regStale)

  S4regStale := new_class(
    properties = list(y = class_character),
    package = NULL
  )
  expect_snapshot(error = TRUE, {
    S4_contains(S4regStale)
  })

  S4_register(S4regStale)
  methods::setClass(
    Class = "S4regStaleChild",
    contains = S4_contains(S4regStale)
  )
  object <- methods::new("S4regStaleChild", y = "new")

  expect_equal(prop(object, "y"), "new")
  expect_equal(
    intersect("x", methods::slotNames("S4regStaleChild")),
    character()
  )
})

test_that("S4_register registers S4 old classes as virtual S7_object descendants", {
  on.exit(S4_remove_classes(c("S4regParent", "S4regS7New")))
  setClass("S4regParent", slots = list(x = "numeric"))
  S4regS7New <- new_class(
    "S4regS7New",
    parent = getClass("S4regParent"),
    properties = list(x = class_numeric),
    package = NULL
  )

  expect_error(
    methods::new("S4regS7New"),
    "trying to generate an object from a virtual class"
  )
  expect_true(methods::extends("S4regS7New", "S7_object"))
  expect_true("_S7_class" %in% methods::slotNames("S4regS7New"))
})

test_that("S4_register registers an S3 class so it can be used with S4 methods", {
  on.exit(S4_remove_classes(c("S4regS3a", "S4regS3b")))
  S4regS3_S4 <- S4_register(new_S3_class(c("S4regS3a", "S4regS3b")))
  expect_equal(S4regS3_S4, "S4regS3a")
  # Must not extend S7_object — that was a silent bug pre-fix
  expect_equal(
    methods::extends("S4regS3a"),
    c("S4regS3a", "S4regS3b", "oldClass")
  )
})

test_that("S4_register registers an S7 union so it can be used with S4 methods", {
  on.exit({
    if (methods::isGeneric("S4regUnionGeneric")) {
      methods::removeGeneric("S4regUnionGeneric")
    }
    S4_remove_classes(c(
      "S4regUnionFoo",
      "S4regUnionFoo_OR_character"
    ))
  })

  S4regUnionFoo <- new_class("S4regUnionFoo", package = NULL)
  S4_register(S4regUnionFoo)
  S4regUnion <- S4regUnionFoo | class_character
  S4regUnion_S4 <- S4_register(S4regUnion)
  expect_equal(S4regUnion_S4, "S4regUnionFoo_OR_character")

  methods::setGeneric(
    "S4regUnionGeneric",
    function(x) standardGeneric("S4regUnionGeneric")
  )
  method(S4regUnionGeneric, S4regUnion) <- function(x) "union"

  expect_equal(S4regUnionGeneric(S4regUnionFoo()), "union")
  expect_equal(S4regUnionGeneric("x"), "union")
})

test_that("S4_register registers unregistered S7 and S3 union members", {
  defer(S4_remove_classes(c(
    "S4regUnionAutoS7",
    "S4regUnionAutoS7_OR_character",
    "S4regUnionAutoS3",
    "S4regUnionAutoS3_OR_character"
  )))

  S4regUnionAutoS7 := new_class(
    package = NULL
  )
  S4regUnionAutoS3 <- new_S3_class(class = "S4regUnionAutoS3")

  expect_false(methods::isClass("S4regUnionAutoS7"))
  expect_false(methods::isClass("S4regUnionAutoS3"))

  expect_equal(
    S4_register(S4regUnionAutoS7 | class_character),
    "S4regUnionAutoS7_OR_character"
  )
  expect_equal(
    S4_register(S4regUnionAutoS3 | class_character),
    "S4regUnionAutoS3_OR_character"
  )
  expect_true(methods::isClass("S4regUnionAutoS7"))
  expect_true(methods::isClass("S4regUnionAutoS3"))
})

test_that("S4_register preserves package-qualified S4 classes", {
  pkg_env <- local_package("s4regpkgclasses")
  defer({
    if (methods::isGeneric("S4regPackageClassGeneric")) {
      methods::removeGeneric("S4regPackageClassGeneric")
    }
    suppressMessages({
      S4_remove_classes(c(
        "S4regPackageClassHolder",
        "S4/s4regpkgclasses::S4regPackageClassFoo",
        "S4regPackageClassFoo_OR_character",
        "S4regPackageClassFoo"
      ))
      S4_remove_classes("S4regPackageClassFoo", pkg_env)
    })
  })

  setClass("S4regPackageClassFoo", slots = list(x = "character"))
  pkg_foo <- methods::setClass(
    "S4regPackageClassFoo",
    slots = list(x = "numeric"),
    where = pkg_env
  )
  S4regPackageClassHolder := new_class(
    properties = list(x = pkg_foo),
    package = NULL
  )

  S4_register(S4regPackageClassHolder)
  expect_identical(
    methods::getClass("S4regPackageClassHolder")@slots$x,
    pkg_foo@className
  )

  union_name <- S4_register(pkg_foo | class_character)
  pkg_object <- methods::new(pkg_foo@className, x = 1)
  global_object <- methods::new(
    methods::getClass("S4regPackageClassFoo"),
    x = "wrong"
  )
  expect_equal(methods::is(pkg_object, union_name), TRUE)
  expect_equal(methods::is(global_object, union_name), FALSE)

  methods::setGeneric(
    "S4regPackageClassGeneric",
    function(x) standardGeneric("S4regPackageClassGeneric")
  )
  method(S4regPackageClassGeneric, pkg_foo) <- function(x) "package"

  expect_equal(S4regPackageClassGeneric(pkg_object), "package")
})

test_that("S4_register allows S4 subclasses of package union members", {
  parent_env <- local_package("s4regunionparentpkg")
  child_env <- local_package("s4regunionchildpkg")
  defer({
    suppressMessages({
      S4_remove_classes(c(
        "s4regunionparentpkg::S4regUnionParent_OR_character",
        "S4/s4regunionparentpkg::S4regUnionParent"
      ))
      S4_remove_classes("S4regUnionParent", parent_env)
      S4_remove_classes("S4regUnionChild", child_env)
    })
  })

  parent <- methods::setClass(
    "S4regUnionParent",
    slots = list(x = "numeric"),
    where = parent_env
  )
  parent_def <- methods::getClass(parent@className, where = parent_env)
  union_name <- S4_register(parent_def | class_character)
  child <- methods::setClass(
    "S4regUnionChild",
    contains = parent_def@className,
    where = child_env
  )

  parent_object <- parent(x = 1)
  child_object <- child(x = 2)

  expect_equal(methods::is(parent_object, union_name), TRUE)
  expect_equal(methods::is(child_object, union_name), TRUE)
})

test_that("S4 compatibility works from an installed S7 namespace", {
  skip_if(getRversion() < "4.1" && Sys.info()[["sysname"]] == "Windows")
  skip_if(quick_test())

  tmp_lib <- local_libpath()
  opts <- c(
    "--data-compress=none",
    "--no-byte-compile",
    "--no-data",
    "--no-demo",
    "--no-docs",
    "--no-help",
    "--no-html",
    "--use-vanilla"
  )
  install.packages(
    pkgs = normalizePath(test_path("../..")),
    lib = tmp_lib,
    repos = NULL,
    type = "source",
    quiet = TRUE,
    INSTALL_opts = paste(opts, collapse = " ")
  )
  quick_install(test_path("t5parent"), tmp_lib)

  expect_no_error(callr::r(
    function(lib) {
      .libPaths(c(lib, .libPaths()))
      library(S7)

      methods::setClass("T5ValidityParent", slots = list(x = "numeric"))
      methods::setValidity("T5ValidityParent", function(object) {
        if (identical(as.character(class(object)), "T5ValidityParent")) {
          TRUE
        } else {
          "parent validity must see parent class"
        }
      })
      T5ValidityChild <- S7::new_class(
        name = "T5ValidityChild",
        parent = methods::getClass("T5ValidityParent"),
        package = NULL
      )
      stopifnot(methods::validObject(T5ValidityChild(x = 1)))

      union <- S7::S4_register(
        t5parent::identity_parent_class() | S7::class_character
      )
      stopifnot(methods::is(t5parent::new_identity_parent(x = 1), union))

      NULL
    },
    args = list(lib = tmp_lib)
  ))
})

test_that("S4_register keeps global S4 union members distinct", {
  pkg_env <- local_package("s4regglobalunionpkg")
  defer({
    suppressMessages({
      S4_remove_classes(c(
        "S4regGlobalUnionFoo_OR_character",
        "S4/S4regGlobalUnionFoo",
        "S4regGlobalUnionFoo"
      ))
      S4_remove_classes("S4regGlobalUnionFoo", pkg_env)
    })
  })

  global_foo <- methods::setClass(
    "S4regGlobalUnionFoo",
    slots = list(x = "character")
  )
  pkg_foo <- methods::setClass(
    "S4regGlobalUnionFoo",
    slots = list(x = "numeric"),
    where = pkg_env
  )
  global_def <- methods::getClass(global_foo@className)

  union_name <- S4_register(global_def | class_character)
  global_object <- global_foo(x = "x")
  pkg_object <- pkg_foo(x = 1)

  expect_equal(methods::is(global_object, union_name), TRUE)
  expect_equal(methods::is(pkg_object, union_name), FALSE)
})

test_that("S4_register gives package S4 unions distinct names", {
  pkg_a_env <- local_package("s4regunionnamea")
  pkg_b_env <- local_package("s4regunionnameb")
  defer({
    suppressMessages({
      S4_remove_classes(c(
        "s4regunionnamea::S4regUnionNameFoo_OR_character",
        "s4regunionnameb::S4regUnionNameFoo_OR_character",
        "S4/s4regunionnamea::S4regUnionNameFoo",
        "S4/s4regunionnameb::S4regUnionNameFoo",
        "S4regUnionNameFoo_OR_character"
      ))
      S4_remove_classes("S4regUnionNameFoo", pkg_a_env)
      S4_remove_classes("S4regUnionNameFoo", pkg_b_env)
    })
  })

  pkg_a_foo <- methods::setClass(
    "S4regUnionNameFoo",
    slots = list(x = "numeric"),
    where = pkg_a_env
  )
  pkg_b_foo <- methods::setClass(
    "S4regUnionNameFoo",
    slots = list(x = "character"),
    where = pkg_b_env
  )

  union_a <- S4_register(pkg_a_foo | class_character)
  union_b <- S4_register(pkg_b_foo | class_character)
  pkg_a_object <- pkg_a_foo(x = 1)
  pkg_b_object <- pkg_b_foo(x = "x")

  expect_equal(
    union_a,
    "s4regunionnamea::S4regUnionNameFoo_OR_character"
  )
  expect_equal(
    union_b,
    "s4regunionnameb::S4regUnionNameFoo_OR_character"
  )
  expect_equal(methods::is(pkg_a_object, union_a), TRUE)
  expect_equal(methods::is(pkg_b_object, union_a), FALSE)
  expect_equal(methods::is(pkg_b_object, union_b), TRUE)
  expect_equal(methods::is(pkg_a_object, union_b), FALSE)
})

test_that("S4_register keeps package-local S4 union members distinct", {
  pkg_env <- local_package("s4regunionlocalpkg")
  defer({
    suppressMessages({
      S4_remove_classes("S4regUnionLocalFoo")
      S4_remove_classes(
        c(
          "s4regunionlocalpkg::S4regUnionLocalFoo_OR_character",
          "S4/s4regunionlocalpkg::S4regUnionLocalFoo",
          "S4regUnionLocalFoo_OR_character"
        ),
        pkg_env
      )
      S4_remove_classes("S4regUnionLocalFoo", pkg_env)
    })
  })

  global_foo <- methods::setClass(
    "S4regUnionLocalFoo",
    slots = list(x = "character")
  )
  pkg_foo <- methods::setClass(
    "S4regUnionLocalFoo",
    slots = list(x = "numeric"),
    where = pkg_env
  )

  union_name <- S4_register(pkg_foo | class_character, env = pkg_env)
  pkg_object <- pkg_foo(x = 1)
  global_object <- global_foo(x = "x")

  expect_equal(
    union_name,
    "s4regunionlocalpkg::S4regUnionLocalFoo_OR_character"
  )
  expect_equal(methods::is(pkg_object, union_name), TRUE)
  expect_equal(methods::is(global_object, union_name), FALSE)
})

test_that("S4_register dispatches union methods for same-package S4 subclasses", {
  pkg_env <- local_package("s4regunionlocalsubpkg")
  defer({
    if (methods::isGeneric("S4regUnionLocalSubGeneric", where = pkg_env)) {
      methods::removeGeneric("S4regUnionLocalSubGeneric", where = pkg_env)
    }
    suppressMessages({
      S4_remove_classes("S4regUnionLocalSubFoo")
      S4_remove_classes(
        c(
          "s4regunionlocalsubpkg::S4regUnionLocalSubFoo_OR_character",
          "S4/s4regunionlocalsubpkg::S4regUnionLocalSubFoo",
          "S4regUnionLocalSubChild",
          "S4regUnionLocalSubFoo_OR_character"
        ),
        pkg_env
      )
      S4_remove_classes("S4regUnionLocalSubFoo", pkg_env)
    })
  })

  global_foo <- methods::setClass(
    "S4regUnionLocalSubFoo",
    slots = list(x = "character")
  )
  pkg_foo <- methods::setClass(
    "S4regUnionLocalSubFoo",
    slots = list(x = "numeric"),
    where = pkg_env
  )

  union_name <- S4_register(pkg_foo | class_character, env = pkg_env)
  child <- methods::setClass(
    "S4regUnionLocalSubChild",
    contains = "S4regUnionLocalSubFoo",
    where = pkg_env
  )
  methods::setGeneric(
    "S4regUnionLocalSubGeneric",
    function(x) standardGeneric("S4regUnionLocalSubGeneric"),
    where = pkg_env
  )
  methods::setMethod(
    "S4regUnionLocalSubGeneric",
    union_name,
    function(x) "union",
    where = pkg_env
  )

  generic <- get("S4regUnionLocalSubGeneric", envir = pkg_env)
  pkg_object <- child(x = 1)
  global_object <- global_foo(x = "x")

  expect_equal(generic(pkg_object), "union")
  expect_equal(methods::is(global_object, union_name), FALSE)
})

test_that("S4_register dispatches union methods for existing S4 subclasses", {
  pkg_env <- local_package("s4regunionexistingsubpkg")
  defer({
    if (methods::isGeneric("S4regUnionExistingSubGeneric", where = pkg_env)) {
      methods::removeGeneric("S4regUnionExistingSubGeneric", where = pkg_env)
    }
    suppressMessages({
      S4_remove_classes(
        c(
          "s4regunionexistingsubpkg::S4regUnionExistingSubFoo_OR_character",
          "S4/s4regunionexistingsubpkg::S4regUnionExistingSubFoo",
          "S4regUnionExistingSubChild",
          "S4regUnionExistingSubFoo_OR_character"
        ),
        pkg_env
      )
      S4_remove_classes("S4regUnionExistingSubFoo", pkg_env)
    })
  })

  pkg_foo <- methods::setClass(
    "S4regUnionExistingSubFoo",
    slots = list(x = "numeric"),
    where = pkg_env
  )
  child <- methods::setClass(
    "S4regUnionExistingSubChild",
    contains = "S4regUnionExistingSubFoo",
    where = pkg_env
  )

  union_name <- S4_register(pkg_foo | class_character, env = pkg_env)
  methods::setGeneric(
    "S4regUnionExistingSubGeneric",
    function(x) standardGeneric("S4regUnionExistingSubGeneric"),
    where = pkg_env
  )
  methods::setMethod(
    "S4regUnionExistingSubGeneric",
    union_name,
    function(x) "union",
    where = pkg_env
  )

  generic <- get("S4regUnionExistingSubGeneric", envir = pkg_env)
  child_object <- child(x = 1)

  expect_equal(methods::is(child_object, union_name), TRUE)
  expect_equal(generic(child_object), "union")
})

test_that("S7 dispatch preserves package-qualified S4 parents", {
  pkg_env <- local_package("s4regdispatchpkg")
  defer({
    S4_remove_classes(c(
      "S4regDispatchChild",
      "S4regDispatchParent"
    ))
    S4_remove_classes("S4regDispatchParent", pkg_env)
  })

  methods::setClass("S4regDispatchParent", slots = list(x = "character"))
  pkg_parent <- methods::setClass(
    "S4regDispatchParent",
    slots = list(x = "numeric"),
    where = pkg_env
  )
  S4regDispatchChild := new_class(
    parent = pkg_parent,
    properties = list(y = class_character),
    package = NULL
  )
  S4regDispatchGeneric := new_generic("x")
  method(S4regDispatchGeneric, methods::getClass("S4regDispatchParent")) <-
    function(x) "global"
  method(S4regDispatchGeneric, pkg_parent) <- function(x) "package"

  object <- S4regDispatchChild(x = 1, y = "child")

  expect_contains(
    class(object),
    "S4/s4regdispatchpkg::S4regDispatchParent"
  )
  expect_equal(S4regDispatchGeneric(object), "package")
})

test_that("S4 union matching preserves package-qualified class keys", {
  pkg_env <- local_package("s4regunionkeypkg")
  defer({
    S4_remove_classes(
      c(
        "S4regUnionKeyHolder",
        "S4regUnionKeyFoo_OR_character"
      ),
      pkg_env
    )
    S4_remove_classes("S4regUnionKeyFoo")
    S4_remove_classes("S4regUnionKeyFoo", pkg_env)
  })

  methods::setClass(
    "S4regUnionKeyFoo",
    slots = list(x = "character")
  )
  pkg_foo <- methods::setClass(
    "S4regUnionKeyFoo",
    slots = list(x = "numeric"),
    where = pkg_env
  )
  global_foo <- methods::getClass("S4regUnionKeyFoo")
  suppressWarnings(methods::setClassUnion(
    "S4regUnionKeyFoo_OR_character",
    list(global_foo@className, "character"),
    where = pkg_env
  ))
  S4regUnionKeyHolder := new_class(
    properties = list(x = pkg_foo | class_character),
    package = NULL
  )

  expect_snapshot(error = TRUE, {
    S4_register(S4regUnionKeyHolder, env = pkg_env)
  })
})

test_that("S4_register can reify S7 properties as slots for S4 subclasses", {
  on.exit({
    if (methods::isGeneric("S4regContainsGeneric")) {
      methods::removeGeneric("S4regContainsGeneric")
    }
    S4_remove_classes(c(
      "S4regContainsS4Child",
      "S7::S4regContains",
      "S7::S4regContainsChild"
    ))
  })

  S4regContains <- new_class(
    "S4regContains",
    properties = list(x = class_numeric),
    package = "S7"
  )
  S4regContainsChild <- new_class(
    "S4regContainsChild",
    parent = S4regContains,
    properties = list(y = class_character),
    validator = function(self) {
      if (identical(self@y, "bad")) "bad y"
    },
    package = "S7"
  )

  S4regContainsChild_old <- S4_register(S4regContainsChild)
  S4regContainsChild_S4 <- S4_contains(S4regContainsChild)
  expect_equal(S4regContainsChild_S4, "S7::S4regContainsChild")
  expect_equal(
    methods::slotNames(S4regContainsChild_S4),
    c("y", "x", "_S7_class", ".S3Class")
  )
  expect_contains(
    methods::extends(S4regContainsChild_S4),
    S4regContainsChild_old
  )

  methods::setClass(
    "S4regContainsS4Child",
    contains = S4regContainsChild_S4,
    slots = list(z = "logical")
  )
  object <- methods::new(
    "S4regContainsS4Child",
    x = 1,
    y = "a",
    z = TRUE
  )

  expect_equal(methods::slot(object, "x"), 1)
  expect_equal(methods::slot(object, "y"), "a")
  expect_equal(methods::slot(object, "z"), TRUE)
  expect_equal(methods::slot(object, "_S7_class"), S4regContainsChild)
  expect_equal(prop_names(object), c("x", "y"))
  expect_equal(prop(object, "x"), 1)
  expect_equal(prop(object, "y"), "a")
  expect_true(methods::validObject(object))
  expect_equal(S7_class(object), S4regContainsChild)
  expect_match(obj_desc(object), "^S4<")

  expect_equal(
    S4_to_S7_class(getClass("S4regContainsS4Child")),
    getClass("S4regContainsS4Child")
  )
  expect_contains(
    obj_dispatch(object),
    c(
      S4_class_name(getClass("S4regContainsS4Child")),
      S4regContainsChild_old
    )
  )

  S4regContainsDispatch <- new_generic("S4regContainsDispatch", "x")
  method(S4regContainsDispatch, S4regContainsChild) <- function(x) {
    "S7"
  }
  method(
    S4regContainsDispatch,
    getClass("S4regContainsS4Child")
  ) <- function(x) {
    "S4"
  }
  expect_equal(S4regContainsDispatch(object), "S4")

  invalid <- object
  methods::slot(invalid, "y") <- "bad"
  expect_error(methods::validObject(invalid), "bad y")

  methods::setGeneric(
    "S4regContainsGeneric",
    function(x) standardGeneric("S4regContainsGeneric")
  )
  method(S4regContainsGeneric, S4regContainsChild) <- function(x) {
    methods::slot(x, "x")
  }
  expect_equal(S4regContainsGeneric(object), 1)
})

test_that("S4 subclasses reject internal S7/S4 slots during initialization", {
  defer(S4_remove_classes(c(
    "S4regInternalSlotChild",
    "S4regInternalSlots"
  )))

  S4regInternalSlots := new_class(
    properties = list(x = class_numeric),
    package = NULL
  )
  S4_register(S4regInternalSlots)
  S4regInternalSlots_S4 <- S4_contains(S4regInternalSlots)
  methods::setClass(
    "S4regInternalSlotChild",
    contains = S4regInternalSlots_S4
  )

  S4regInternalOther := new_class(package = NULL)

  expect_snapshot(error = TRUE, {
    methods::new(
      "S4regInternalSlotChild",
      x = 1,
      `_S7_class` = S4regInternalOther
    )
  })
  expect_snapshot(error = TRUE, {
    methods::new(
      "S4regInternalSlotChild",
      x = 1,
      .S3Class = "bogus"
    )
  })
})

test_that("S4_register constructs S4 subclasses of S7 classes that extend S4 classes", {
  on.exit(S4_remove_classes(c(
    "S4regNewParent",
    "S4regNewMiddle",
    "S4regNewChild",
    "S4regNewGrandChild"
  )))
  setClass(
    "S4regNewParent",
    slots = list(assays = "list", rowData = "character")
  )
  methods::setValidity("S4regNewParent", function(object) {
    if (!identical(methods::slot(object, "assays"), list())) {
      "assays slot was stripped during parent coercion"
    } else {
      TRUE
    }
  })

  S4regNewMiddle <- new_class(
    "S4regNewMiddle",
    parent = getClass("S4regNewParent"),
    properties = list(metadata = class_character),
    validator = function(self) {
      if (identical(self@metadata, "bad")) "bad metadata"
    },
    package = NULL
  )
  S4regNewChild <- new_class(
    "S4regNewChild",
    parent = S4regNewMiddle,
    properties = list(status = class_character),
    validator = function(self) {
      if (identical(self@status, "bad")) "bad status"
    },
    package = NULL
  )
  S4regNewChild_S4 <- S4_contains(S4regNewChild)
  expect_equal(
    methods::slotNames("S4regNewChild"),
    c("status", "metadata", "_S7_class", "assays", "rowData", ".S3Class")
  )
  expect_contains(
    methods::slotNames(S4regNewChild_S4),
    c("assays", "rowData", "metadata", "status", "_S7_class")
  )
  setClass(
    "S4regNewGrandChild",
    contains = S4regNewChild_S4
  )

  object <- methods::new("S4regNewGrandChild")

  expect_true(isS4(object))
  expect_true(methods::is(object, "S7_object"))
  expect_true(methods::is(object, "S4regNewParent"))
  expect_true(methods::is(object, S4regNewChild_S4))
  expect_true(S7_inherits(object, S4regNewChild))
  expect_equal(methods::slot(object, "_S7_class"), S4regNewChild)
  expect_equal(methods::slot(object, "assays"), list())
  expect_equal(methods::slot(object, "rowData"), character())
  expect_equal(methods::slot(object, "metadata"), character())
  expect_equal(methods::slot(object, "status"), character())
  object_reified <- methods::as(object, S4regNewChild_S4)
  object_old <- methods::as(object_reified, "S4regNewChild")
  expect_equal(methods::slot(object_old, "metadata"), character())
  expect_equal(methods::slot(object_old, "status"), character())
  object_old <- methods::as(object, "S4regNewChild")
  expect_equal(methods::slot(object_old, "metadata"), character())
  expect_equal(methods::slot(object_old, "status"), character())
  expect_equal(
    prop_names(object),
    c("assays", "rowData", "metadata", "status")
  )
  expect_equal(prop(object, "assays"), list())
  expect_equal(prop(object, "rowData"), character())
  expect_equal(prop(object, "metadata"), character())
  expect_equal(prop(object, "status"), character())
  expect_true(methods::validObject(object))

  object <- methods::new(
    "S4regNewGrandChild",
    metadata = "m",
    status = "s"
  )
  expect_equal(methods::slot(object, "metadata"), "m")
  expect_equal(methods::slot(object, "status"), "s")
  expect_true(methods::validObject(object))

  invalid <- object
  methods::slot(invalid, "metadata") <- "bad"
  expect_error(methods::validObject(invalid), "bad metadata")

  invalid <- object
  methods::slot(invalid, "status") <- "bad"
  expect_error(methods::validObject(invalid), "bad status")
})

test_that("S4_validate_class validates only matching S7 classes for S4 upcasts", {
  on.exit(S4_remove_classes(c(
    "S4regShimRoot",
    "S4regShimParent",
    "S4regShimChild",
    "S4regShimGrandChild",
    "S7::S4regShimParent",
    "S7::S4regShimChild"
  )))

  setClass("S4regShimRoot", slots = list(root = "numeric"))
  S4regShimParent <- new_class(
    "S4regShimParent",
    parent = getClass("S4regShimRoot"),
    properties = list(x = class_numeric),
    package = "S7"
  )
  S4regShimChild <- new_class(
    "S4regShimChild",
    parent = S4regShimParent,
    properties = list(y = class_character),
    package = "S7"
  )

  S4regShimParent_S4 <- S4_contains(S4regShimParent)
  S4regShimChild_S4 <- S4_contains(S4regShimChild)
  setClass("S4regShimParent", contains = S4regShimParent_S4)
  setClass(
    "S4regShimChild",
    contains = c(S4regShimChild_S4, "S4regShimParent")
  )
  setClass("S4regShimGrandChild", contains = "S4regShimChild")

  object <- methods::new("S4regShimGrandChild", root = 1, x = 2, y = "a")
  parent_object <- methods::as(object, S4regShimParent_S4)

  expect_equal(methods::slot(parent_object, "y"), "a")
  expect_equal(S7_class(parent_object), S4regShimChild)
  expect_true(methods::validObject(object))
})

test_that("S4_register registers abstract S7 classes as virtual S4 classes", {
  on.exit({
    try(methods::removeMethod("dim", "S4regAbstractConcrete"), silent = TRUE)
    S4_remove_classes(c(
      "S4regAbstractParent",
      "S4regAbstract",
      "S4regAbstractConcrete",
      "S4regAbstractShim"
    ))
  })

  setClass("S4regAbstractParent", contains = "VIRTUAL")
  methods::setValidity("S4regAbstractParent", function(object) {
    if (!identical(dim(object), c(1L, 2L))) {
      "dim() did not dispatch to the concrete S7 class"
    } else {
      TRUE
    }
  })

  S4regAbstract <- new_class(
    "S4regAbstract",
    parent = getClass("S4regAbstractParent"),
    abstract = TRUE,
    package = NULL
  )
  S4regAbstractConcrete <- new_class(
    "S4regAbstractConcrete",
    parent = S4regAbstract,
    properties = list(x = class_integer),
    package = NULL
  )
  method(dim, S4regAbstractConcrete) <- function(x) c(x@x, 2L)
  S4regAbstractConcrete_S4 <- S4_contains(S4regAbstractConcrete)
  setClass("S4regAbstractShim", contains = S4regAbstractConcrete_S4)

  object <- methods::new("S4regAbstractShim", x = 1L)
  abstract_prototype <- methods::getClass("S4regAbstract")@prototype
  concrete_prototype <- methods::getClass("S4regAbstractConcrete")@prototype

  expect_true(methods::isVirtualClass("S4regAbstract"))
  expect_true("_S7_class" %in% methods::slotNames("S4regAbstract"))
  expect_equal(methods::slot(abstract_prototype, "_S7_class"), S4regAbstract)
  expect_false(methods::extends("S4regAbstract", "oldClass"))
  expect_false(methods::extends("S4regAbstract", "S7_object"))
  expect_equal(
    methods::slot(concrete_prototype, "_S7_class"),
    S4regAbstractConcrete
  )
  expect_false("S4regAbstract" %in% attr(concrete_prototype, ".S3Class"))
  expect_equal(dim(object), c(1L, 2L))
  expect_true(methods::validObject(object))
})

test_that("S4_register preserves S7 dispatch through abstract S7 parents", {
  defer(S4_remove_classes(c(
    "S4regAbstractPlainChild",
    "S4regAbstractPlainConcrete",
    "S4regAbstractPlain"
  )))

  S4regAbstractPlain := new_class(
    abstract = TRUE,
    package = NULL
  )
  S4regAbstractPlainConcrete := new_class(
    parent = S4regAbstractPlain,
    package = NULL
  )
  S4_register(S4regAbstractPlainConcrete)
  S4regAbstractPlainConcrete_S4 <- S4_contains(S4regAbstractPlainConcrete)
  setClass(
    "S4regAbstractPlainChild",
    contains = S4regAbstractPlainConcrete_S4
  )

  object <- methods::new("S4regAbstractPlainChild")
  S4regAbstractPlainGeneric <- new_generic("S4regAbstractPlainGeneric", "x")
  method(S4regAbstractPlainGeneric, S4regAbstractPlain) <- function(x) {
    "abstract"
  }

  expect_equal(S4regAbstractPlainGeneric(object), "abstract")
  expect_contains(
    obj_dispatch(object),
    c("S4regAbstractPlainConcrete", "S4regAbstractPlain")
  )
})

test_that("S4_register preserves S7 dispatch through S4-rooted abstract S7 parents", {
  defer(S4_remove_classes(c(
    "S4regAbstractRootChild",
    "S4regAbstractRootConcrete",
    "S4regAbstractRoot",
    "S4regAbstractRootParent"
  )))

  setClass("S4regAbstractRootParent", contains = "VIRTUAL")
  S4regAbstractRoot := new_class(
    parent = getClass("S4regAbstractRootParent"),
    abstract = TRUE,
    package = NULL
  )
  S4regAbstractRootConcrete := new_class(
    parent = S4regAbstractRoot,
    package = NULL
  )
  S4_register(S4regAbstractRootConcrete)
  S4regAbstractRootConcrete_S4 <- S4_contains(S4regAbstractRootConcrete)
  setClass(
    "S4regAbstractRootChild",
    contains = S4regAbstractRootConcrete_S4
  )

  object <- methods::new("S4regAbstractRootChild")
  S4regAbstractRootGeneric <- new_generic("S4regAbstractRootGeneric", "x")
  method(S4regAbstractRootGeneric, S4regAbstractRoot) <- function(x) {
    "abstract"
  }

  expect_equal(
    S4regAbstractRootGeneric(S4regAbstractRootConcrete()),
    "abstract"
  )
  expect_equal(S4regAbstractRootGeneric(object), "abstract")
  expect_contains(
    obj_dispatch(object),
    c("S4regAbstractRootConcrete", "S4regAbstractRoot")
  )
})

test_that("S4_contains rejects abstract S7 classes", {
  defer(S4_remove_classes(c(
    "S4regAbstractContainsParent",
    "S4regAbstractContains"
  )))

  setClass("S4regAbstractContainsParent", contains = "VIRTUAL")
  S4regAbstractContains <- new_class(
    "S4regAbstractContains",
    parent = getClass("S4regAbstractContainsParent"),
    abstract = TRUE,
    package = NULL
  )

  expect_error(
    S4_contains(S4regAbstractContains),
    "abstract"
  )
})

test_that("S4_register uses S7 property defaults for S4 objects", {
  on.exit(S4_remove_classes(c(
    "S4regPrototype",
    "S4regPrototypeChild",
    "NULL_OR_character"
  )))

  S4_register(NULL | class_character)
  S4regPrototype <- new_class(
    "S4regPrototype",
    properties = list(
      x = new_property(class_numeric, default = quote(1)),
      y = new_property(class_character, default = quote("a")),
      z = new_property(NULL | class_character, default = NULL)
    ),
    package = NULL
  )
  S4_register(S4regPrototype)
  S4regPrototype_S4 <- S4_contains(S4regPrototype)
  methods::setClass("S4regPrototypeChild", contains = S4regPrototype_S4)

  object <- methods::new("S4regPrototypeChild")

  expect_equal(methods::slot(object, "x"), 1)
  expect_equal(methods::slot(object, "y"), "a")
  expect_equal(methods::slot(object, "z"), NULL)
  expect_equal(prop(object, "x"), 1)
  expect_equal(prop(object, "y"), "a")
  expect_equal(prop(object, "z"), NULL)
  expect_true(methods::validObject(object))
})

test_that("S4_register evaluates expression defaults for each S4 object", {
  defer(S4_remove_classes(c(
    "S4regPrototypeExpr",
    "S4regPrototypeExprChild"
  )))

  S4regPrototypeExpr <- new_class(
    "S4regPrototypeExpr",
    properties = list(
      x = new_property(
        class_environment,
        default = quote(new.env(parent = emptyenv()))
      )
    ),
    package = NULL
  )
  S4_register(S4regPrototypeExpr)
  S4regPrototypeExpr_S4 <- S4_contains(S4regPrototypeExpr)
  methods::setClass("S4regPrototypeExprChild", contains = S4regPrototypeExpr_S4)

  object1 <- methods::new("S4regPrototypeExprChild")
  object2 <- methods::new("S4regPrototypeExprChild")

  prop(object1, "x")$value <- 1
  expect_null(prop(object2, "x")$value)
})

test_that("S4 subclass prototypes override deferred S7 defaults", {
  defer(S4_remove_classes(c(
    "S4regPrototypeDeferred",
    "S4regPrototypeDeferredChild"
  )))

  S4regPrototypeDeferred := new_class(
    properties = list(
      x = new_property(
        class = class_numeric,
        default = quote({
          1
        })
      )
    ),
    package = NULL
  )
  S4_register(S4regPrototypeDeferred)
  methods::setClass(
    Class = "S4regPrototypeDeferredChild",
    contains = S4_contains(S4regPrototypeDeferred),
    prototype = list(x = 5)
  )

  object <- methods::new("S4regPrototypeDeferredChild")

  expect_equal(methods::slot(object, "x"), 5)
  expect_equal(prop(object, "x"), 5)
})

test_that("S4_register evaluates deferred defaults in the constructor environment", {
  defer(S4_remove_classes(c(
    "S4regDefaultEnv",
    "S4regDefaultEnvChild"
  )))

  S4regDefaultEnv <- local({
    x <- 1
    S4regDefaultEnv := new_class(
      properties = list(x = new_property(class_numeric, default = quote(x))),
      package = NULL
    )
  })
  S4_register(S4regDefaultEnv)
  S4regDefaultEnv_S4 <- S4_contains(S4regDefaultEnv)
  methods::setClass("S4regDefaultEnvChild", contains = S4regDefaultEnv_S4)

  object <- methods::new("S4regDefaultEnvChild")

  expect_equal(methods::slot(object, "x"), 1)
  expect_equal(prop(object, "x"), 1)
})

test_that("S4_register validates deferred NULL defaults", {
  defer(S4_remove_classes(c(
    "S4regNullDefault",
    "S4regNullDefaultChild"
  )))

  S4regNullDefault := new_class(
    properties = list(
      x = new_property(
        class_numeric,
        default = quote({
          NULL
        })
      )
    ),
    package = NULL
  )
  S4_register(S4regNullDefault)
  S4regNullDefault_S4 <- S4_contains(S4regNullDefault)
  methods::setClass("S4regNullDefaultChild", contains = S4regNullDefault_S4)

  expect_snapshot(error = TRUE, {
    methods::new("S4regNullDefaultChild")
  })
})

test_that("S4 parents preserve slot prototypes in S7 constructors", {
  defer(S4_remove_classes(c(
    "S4regSlotPrototypeParent",
    "S4regSlotPrototypeChild"
  )))

  setClass(
    "S4regSlotPrototypeParent",
    slots = list(x = "numeric"),
    prototype = list(x = 10)
  )
  S4regSlotPrototypeChild <- new_class(
    "S4regSlotPrototypeChild",
    parent = getClass("S4regSlotPrototypeParent"),
    package = NULL
  )

  object <- S4regSlotPrototypeChild()

  expect_equal(methods::slot(object, "x"), 10)
  expect_equal(prop(object, "x"), 10)
})

test_that("S4 parent prototype defaults preserve package-qualified classes", {
  pkg_env <- local_package("s4regpkgdefaults")
  defer({
    suppressMessages({
      S4_remove_classes(c(
        "S4regPackageDefaultParent",
        "S4regPackageDefaultChild"
      ))
      S4_remove_classes("S4regPackageDefaultParent", pkg_env)
    })
  })

  setClass(
    "S4regPackageDefaultParent",
    slots = list(x = "character"),
    contains = "VIRTUAL",
    prototype = list(x = "wrong")
  )
  pkg_parent <- methods::setClass(
    "S4regPackageDefaultParent",
    slots = list(x = "numeric"),
    contains = "VIRTUAL",
    prototype = list(x = 10),
    where = pkg_env
  )
  S4regPackageDefaultChild := new_class(
    parent = pkg_parent,
    package = NULL
  )

  child_def <- methods::getClass("S4regPackageDefaultChild")
  expect_identical(
    child_def@contains$S4regPackageDefaultParent@superClass,
    pkg_parent@className
  )

  suppressMessages(object <- S4regPackageDefaultChild())
  expect_equal(prop(object, "x"), 10)
  expect_equal(methods::slot(object, "x"), 10)
})

test_that("S4_register resolves S4 prototypes from object class package", {
  pkg_env <- local_package("s4regprototypepkg")
  defer({
    suppressMessages({
      S4_remove_classes("S4regPkgPrototypeChild")
      S4_remove_classes(
        c(
          "S4regPkgPrototype",
          "S4regPkgPrototypeChild"
        ),
        pkg_env
      )
    })
  })

  setClass(
    "S4regPkgPrototypeChild",
    slots = list(x = "numeric"),
    prototype = list(x = 1)
  )
  evalq(
    {
      S4regPkgPrototype <- new_class(
        "S4regPkgPrototype",
        properties = list(
          x = new_property(
            class_numeric,
            default = quote({
              10
            })
          )
        ),
        package = NULL
      )
      S4_register(S4regPkgPrototype)
      S4regPkgPrototype_S4 <- S4_contains(S4regPkgPrototype)
      methods::setClass(
        "S4regPkgPrototypeChild",
        contains = S4regPkgPrototype_S4
      )
    },
    pkg_env
  )

  child_def <- methods::getClass("S4regPkgPrototypeChild", where = pkg_env)
  suppressWarnings(object <- methods::new(child_def@className))

  expect_equal(prop(object, "x"), 10)
  expect_equal(methods::slot(object, "x"), 10)
})

test_that("S7 constructors delegate to S4 parent initialize methods", {
  defer(S4_remove_classes(c(
    "S4regInitializeParent",
    "S4regInitializeChild"
  )))

  setClass("S4regInitializeParent", slots = list(x = "numeric"))
  methods::setMethod(
    "initialize",
    "S4regInitializeParent",
    function(.Object, x = 1, ...) {
      .Object <- callNextMethod(.Object, ...)
      .Object@x <- x * 10
      .Object
    }
  )
  S4regInitializeChild <- new_class(
    "S4regInitializeChild",
    parent = getClass("S4regInitializeParent"),
    properties = list(y = class_character),
    package = NULL
  )

  object <- S4regInitializeChild(x = 2, y = "a")

  expect_equal(prop(object, "x"), 20)
  expect_equal(methods::slot(object, "x"), 20)
  expect_equal(prop(object, "y"), "a")

  object <- S4regInitializeChild(y = "b")
  expect_equal(prop(object, "x"), 10)
  expect_equal(prop(object, "y"), "b")
})

test_that("S7 constructors delegate to virtual S4 parent initialize methods", {
  defer(S4_remove_classes(c(
    "S4regVirtualInitializeParent",
    "S4regVirtualInitializeChild"
  )))

  setClass(
    "S4regVirtualInitializeParent",
    slots = list(x = "numeric"),
    contains = "VIRTUAL",
    prototype = list(x = -1)
  )
  methods::setMethod(
    "initialize",
    "S4regVirtualInitializeParent",
    function(.Object, x = 1, ...) {
      .Object <- callNextMethod(.Object, ...)
      .Object@x <- x * 10
      .Object
    }
  )
  S4regVirtualInitializeChild := new_class(
    parent = getClass("S4regVirtualInitializeParent"),
    properties = list(y = class_character),
    package = NULL
  )

  object <- S4regVirtualInitializeChild(x = 2, y = "a")

  expect_equal(prop(object, "x"), 20)
  expect_equal(methods::slot(object, "x"), 20)
  expect_equal(prop(object, "y"), "a")

  object <- S4regVirtualInitializeChild(y = "b")
  expect_equal(prop(object, "x"), 10)
  expect_equal(prop(object, "y"), "b")
})

test_that("S7 constructors validate virtual S4 parents after initialize", {
  defer(S4_remove_classes(c(
    "S4regVirtualRepairParent",
    "S4regVirtualRepairChild"
  )))

  setClass(
    "S4regVirtualRepairParent",
    slots = list(x = "numeric"),
    contains = "VIRTUAL",
    prototype = list(x = numeric())
  )
  methods::setValidity("S4regVirtualRepairParent", function(object) {
    if (length(methods::slot(object, "x")) != 1) {
      "x must have length 1"
    } else {
      TRUE
    }
  })
  methods::setMethod(
    "initialize",
    "S4regVirtualRepairParent",
    function(.Object, x = 1, ...) {
      .Object <- callNextMethod(.Object, ...)
      .Object@x <- x
      .Object
    }
  )
  S4regVirtualRepairChild := new_class(
    parent = getClass("S4regVirtualRepairParent"),
    properties = list(y = class_character),
    package = NULL
  )

  object <- S4regVirtualRepairChild(y = "a")

  expect_equal(prop(object, "x"), 1)
  expect_equal(prop(object, "y"), "a")
  expect_true(methods::validObject(object))
})

test_that("S7 constructors support callNextMethod in virtual S4 parents", {
  defer(S4_remove_classes(c(
    "S4regVirtualNextParent",
    "S4regVirtualNextChild"
  )))

  setClass(
    "S4regVirtualNextParent",
    slots = list(x = "numeric"),
    contains = "VIRTUAL",
    prototype = list(x = -1)
  )
  methods::setMethod(
    "initialize",
    "S4regVirtualNextParent",
    function(.Object, x = 1, ...) {
      .Object <- callNextMethod()
      .Object@x <- x * 10
      .Object
    }
  )
  S4regVirtualNextChild := new_class(
    parent = getClass("S4regVirtualNextParent"),
    properties = list(y = class_character),
    package = NULL
  )

  object <- S4regVirtualNextChild(x = 2, y = "a")

  expect_equal(prop(object, "x"), 20)
  expect_equal(methods::slot(object, "x"), 20)
  expect_equal(prop(object, "y"), "a")
})

test_that("S4 prototypes use overridden inherited S7 property defaults", {
  defer(S4_remove_classes(c(
    "S4regOverrideParent",
    "S4regOverrideChild",
    "S4regOverrideShim"
  )))

  setClass(
    "S4regOverrideParent",
    slots = list(x = "numeric"),
    prototype = list(x = 1)
  )
  S4regOverrideChild <- new_class(
    "S4regOverrideChild",
    parent = getClass("S4regOverrideParent"),
    properties = list(x = new_property(class_numeric, default = 2)),
    package = NULL
  )
  S4regOverrideChild_S4 <- S4_contains(S4regOverrideChild)
  setClass("S4regOverrideShim", contains = S4regOverrideChild_S4)

  expect_equal(prop(S4regOverrideChild(), "x"), 2)

  object <- methods::new("S4regOverrideShim")
  expect_equal(methods::slot(object, "x"), 2)
  expect_equal(prop(object, "x"), 2)
})

test_that("S4 prototypes preserve overridden NULL S7 property defaults", {
  defer(S4_remove_classes(c(
    "S4regNullOverrideParent",
    "S4regNullOverrideChild",
    "S4regNullOverrideShim",
    "S4regNullOverrideSlot"
  )))

  setClassUnion("S4regNullOverrideSlot", c("character", "NULL"))
  setClass(
    "S4regNullOverrideParent",
    slots = list(x = "S4regNullOverrideSlot"),
    prototype = list(x = "parent")
  )
  S4regNullOverrideChild <- new_class(
    "S4regNullOverrideChild",
    parent = getClass("S4regNullOverrideParent"),
    properties = list(
      x = new_property(NULL | class_character, default = NULL)
    ),
    package = NULL
  )
  S4regNullOverrideChild_S4 <- S4_contains(S4regNullOverrideChild)
  setClass("S4regNullOverrideShim", contains = S4regNullOverrideChild_S4)

  expect_equal(prop(S4regNullOverrideChild(), "x"), NULL)

  object <- methods::new("S4regNullOverrideShim")
  expect_equal(methods::slot(object, "x"), NULL)
  expect_equal(prop(object, "x"), NULL)
})

test_that("S4_register treats S4 NULL slot sentinels as NULL-valued S7 properties", {
  on.exit(S4_remove_classes(c(
    "S4regNullable",
    "S4regNullableChild",
    "NULL_OR_character"
  )))

  S4_register(NULL | class_character)
  S4regNullable <- new_class(
    "S4regNullable",
    properties = list(
      x = new_property(NULL | class_character, default = NULL)
    ),
    package = NULL
  )
  S4_register(S4regNullable)
  S4regNullable_S4 <- S4_contains(S4regNullable)
  methods::setClass("S4regNullableChild", contains = S4regNullable_S4)

  object <- methods::new("S4regNullableChild")

  expect_equal(methods::slot(object, "x"), NULL)
  expect_equal(prop(object, "x"), NULL)
  expect_true(methods::validObject(object))
  expect_no_error(methods::new("S4regNullableChild", x = NULL))

  object_with_value <- methods::new("S4regNullableChild", x = "a")
  prop(object_with_value, "x") <- NULL
  expect_equal(methods::slot(object_with_value, "x"), NULL)
  expect_equal(prop(object_with_value, "x"), NULL)

  plain <- S4regNullable(x = "a")
  prop(plain, "x") <- NULL
  expect_equal(prop(plain, "x"), NULL)
  expect_identical(attr(plain, "x", exact = TRUE), as.name("\001NULL\001"))
})

test_that("S4_register keeps direct S7 nullable property slots valid", {
  defer(S4_remove_classes(c(
    "S4regDirectNullable",
    "NULL_OR_character"
  )))

  S4_register(NULL | class_character)
  S4regDirectNullable <- new_class(
    "S4regDirectNullable",
    properties = list(x = NULL | class_character),
    package = NULL
  )
  S4_register(S4regDirectNullable)

  object <- S4regDirectNullable()

  expect_equal(prop(object, "x"), NULL)
  expect_equal(methods::slot(object, "x"), NULL)
  expect_true(methods::validObject(object))
  expect_identical(attr(object, "x", exact = TRUE), as.name("\001NULL\001"))
})

test_that("S4_contains rejects properties with custom accessors", {
  on.exit(S4_remove_classes(c(
    "S4regContainsDynamicChild",
    "S7::S4regContainsDynamic",
    "S4regContainsSetterChild",
    "S7::S4regContainsSetter"
  )))

  S4regContainsDynamic <- new_class(
    "S4regContainsDynamic",
    properties = list(
      x = new_property(class_numeric, getter = function(self) 1)
    ),
    package = "S7"
  )
  expect_no_error(S4_register(S4regContainsDynamic))
  expect_error(
    S4_contains(S4regContainsDynamic),
    "custom getter"
  )
  methods::setClass(
    "S4regContainsDynamicChild",
    contains = "S7::S4regContainsDynamic"
  )
  expect_error(
    methods::new("S4regContainsDynamicChild"),
    "custom getter"
  )

  S4regContainsSetter <- new_class(
    "S4regContainsSetter",
    properties = list(
      x = new_property(
        class_numeric,
        setter = function(self, value) {
          attr(self, "x") <- value
          self
        }
      )
    ),
    package = "S7"
  )
  expect_no_error(S4_register(S4regContainsSetter))
  expect_error(
    S4_contains(S4regContainsSetter),
    "custom setter"
  )
  methods::setClass(
    "S4regContainsSetterChild",
    contains = "S7::S4regContainsSetter"
  )
  expect_error(
    methods::new("S4regContainsSetterChild"),
    "custom setter"
  )
})

test_that("S4_register excludes dynamic properties from S4 slots", {
  defer(S4_remove_classes("S4regDynamicSlots"))

  S4regDynamicSlots <- new_class(
    "S4regDynamicSlots",
    properties = list(
      x = new_property(class_numeric, getter = function(self) 1),
      y = class_numeric
    ),
    package = NULL
  )

  S4_register(S4regDynamicSlots)
  object <- S4regDynamicSlots(y = 2)

  expect_equal(
    methods::slotNames("S4regDynamicSlots"),
    c("y", "_S7_class", ".S3Class")
  )
  expect_equal(prop(object, "x"), 1)
  expect_true(methods::validObject(object))
})

test_that("S4_register uses registered S7 unions as S4 slots", {
  on.exit(S4_remove_classes(c(
    "S7::S4regContainsUnion",
    "integer_OR_numeric_OR_character"
  )))

  S4regContainsUnion <- new_class(
    "S4regContainsUnion",
    properties = list(x = class_numeric | class_character),
    package = "S7"
  )
  expect_error(
    S4_register(S4regContainsUnion),
    "not been registered"
  )

  S4_register(class_numeric | class_character)
  S4_register(S4regContainsUnion)
  S4regContainsUnion_S4 <- S4_contains(S4regContainsUnion)
  expect_equal(
    as.character(methods::getClass(S4regContainsUnion_S4)@slots$x),
    "integer_OR_numeric_OR_character"
  )
})

test_that("S4_register uses matching S4 unions as S4 slots", {
  env <- topenv(environment())
  on.exit(S4_remove_classes(
    c(
      "S4regContainsExistingUnion",
      "S4regExistingUnion",
      "S4regUnionMember2",
      "S4regUnionMember1"
    ),
    env
  ))

  setClass("S4regUnionMember1", where = env)
  setClass("S4regUnionMember2", where = env)
  setClassUnion(
    "S4regExistingUnion",
    c("S4regUnionMember1", "S4regUnionMember2"),
    where = env
  )

  S4regContainsExistingUnion <- new_class(
    "S4regContainsExistingUnion",
    properties = list(
      x = getClass("S4regUnionMember1", where = env) |
        getClass("S4regUnionMember2", where = env)
    ),
    package = NULL
  )

  S4_register(S4regContainsExistingUnion, env)
  S4regContainsExistingUnion_S4 <- S4_contains(
    S4regContainsExistingUnion,
    env
  )
  expect_equal(
    as.character(
      methods::getClass(
        S4regContainsExistingUnion_S4,
        where = env
      )@slots$x
    ),
    "S4regExistingUnion"
  )
})

test_that("S4_register errors on unsupported inputs", {
  expect_snapshot(error = TRUE, {
    S4_register(1)
    S4_register("foo")
  })
})

test_that("converts S4 base classes to S7 base classes", {
  expect_equal(S4_to_S7_class(getClass("NULL")), NULL)
  expect_equal(S4_to_S7_class(getClass("ANY")), class_any)
  expect_equal(S4_to_S7_class(getClass("character")), class_character)
})

test_that("S4 ANY slots can be narrowed by S7 children", {
  defer(S4_remove_classes(c(
    "S4regAnySlotChild",
    "S4regAnySlotParent"
  )))

  setClass("S4regAnySlotParent", slots = list(x = "ANY"))
  S4regAnySlotChild := new_class(
    parent = getClass("S4regAnySlotParent"),
    properties = list(x = class_numeric),
    package = NULL
  )

  object <- S4regAnySlotChild(x = 1)
  expect_equal(prop(object, "x"), 1)
  expect_true(methods::validObject(object))
})

test_that("converts S4 unions to S7 unions", {
  Foo1 := local_S4_class(slots = "x")
  Foo2 := local_S4_class(slots = "x")

  Union1 := local_S4_union(c("Foo1", "Foo2"))
  expect_equal(
    S4_to_S7_class(getClass("Union1")),
    new_union(getClass("Foo1"), getClass("Foo2"))
  )

  Foo3 := local_S4_class(slots = "x")
  Union2 := local_S4_union(c("Union1", "Foo3"))
  expect_equal(
    S4_to_S7_class(getClass("Union2")),
    new_union(getClass("Foo1"), getClass("Foo2"), getClass("Foo3"))
  )
})

test_that("S4 slot properties convert S4 class unions to S7 unions", {
  env <- topenv(environment())
  on.exit(S4_remove_classes(
    c(
      "S4regUnionSlotChild",
      "S4regUnionSlotParent",
      "S4regUnionSlot"
    ),
    env
  ))

  setClassUnion("S4regUnionSlot", c("character", "NULL"), where = env)
  setClass(
    "S4regUnionSlotParent",
    slots = list(u = "S4regUnionSlot"),
    where = env
  )

  S4regUnionSlotChild <- new_class(
    "S4regUnionSlotChild",
    parent = getClass("S4regUnionSlotParent", where = env),
    properties = list(y = class_character),
    package = NULL
  )

  expect_equal(
    S4regUnionSlotChild@properties$u$class,
    new_union(NULL, class_character)
  )
})

test_that("converts S4 representation of S3 classes to S7 representation", {
  expect_equal(
    S4_to_S7_class(getClass("Date")),
    class_Date,
    ignore_function_env = TRUE
  )
})

test_that("S4 old-class representations dispatch as S3 class vectors", {
  S4regOrderedGeneric := new_generic(dispatch_args = "x")
  method(S4regOrderedGeneric, getClass("ordered")) <- function(x) "ordered"

  expect_equal(S4regOrderedGeneric(ordered("a")), "ordered")
})

test_that("S4 old-class slots accept normal S3 class vectors", {
  defer(S4_remove_classes(c(
    "S4regOldSlotChild",
    "S4regOldSlotParent",
    "S4regOldFoo",
    "S4regOldBar"
  )))

  setOldClass(c("S4regOldFoo", "S4regOldBar"))
  setClass("S4regOldSlotParent", slots = list(x = "S4regOldFoo"))
  S4regOldSlotChild <- new_class(
    "S4regOldSlotChild",
    parent = getClass("S4regOldSlotParent"),
    package = NULL
  )
  x <- structure(list(), class = c("S4regOldFoo", "S4regOldBar"))

  object <- S4regOldSlotChild(x = x)

  expect_equal(prop(object, "x"), x)
})

test_that("S7 classes do not import internal slots from S4 parents", {
  defer(S4_remove_classes(c(
    "S4regImportInternalChild",
    "S4regImportInternalParent",
    "S4regImportInternalBase"
  )))

  S4regImportInternalBase := new_class(
    properties = list(x = class_numeric),
    package = NULL
  )
  S4_register(S4regImportInternalBase)
  setClass(
    "S4regImportInternalParent",
    contains = S4_contains(S4regImportInternalBase),
    slots = list(y = "character")
  )
  S4regImportInternalChild := new_class(
    parent = getClass("S4regImportInternalParent"),
    properties = list(z = class_logical),
    package = NULL
  )

  object <- S4regImportInternalChild(
    x = 1,
    y = "a",
    z = TRUE
  )

  expect_equal(names(formals(S4regImportInternalChild)), c("y", "x", "z"))
  expect_equal(prop_names(object), c("y", "x", "z"))
  expect_true(methods::validObject(object))
})

test_that("errors on non-S4 classes", {
  expect_snapshot(S4_to_S7_class(1), error = TRUE)
})

test_that("S7 classes can extend S4 classes", {
  on.exit(S4_remove_classes(c("Parent", "Child")))
  setClass("Parent", slots = list(x = "numeric"))

  Child <- new_class(
    "Child",
    parent = getClass("Parent"),
    properties = list(y = class_character),
    package = NULL
  )
  child <- Child(x = 1, y = "a")

  expect_false(isS4(child))
  expect_true(S7_inherits(child, Child))
  expect_equal(prop_names(child), c("x", "y"))
  expect_equal(prop(child, "x"), 1)
  expect_equal(prop(child, "y"), "a")

  prop(child, "x") <- 2
  prop(child, "y") <- "b"
  expect_equal(prop(child, "x"), 2)
  expect_equal(prop(child, "y"), "b")

  expect_true(methods::is(child, "Parent"))
  expect_true(methods::validObject(child))
  expect_equal(as.character(methods::getClass("Child")@className), "Child")
  expect_equal(
    methods::slotNames("Child"),
    c("y", "_S7_class", "x", ".S3Class")
  )
  expect_equal(methods::slot(child, "x"), 2)
  expect_equal(methods::slot(child, "y"), "b")

  child <- methods::initialize(child, x = 3, y = "c")
  expect_equal(prop(child, "x"), 3)
  expect_equal(prop(child, "y"), "c")
  expect_true(methods::validObject(child))

  child <- methods::initialize(child, Child(x = 4, y = "d"), y = "e")
  expect_equal(prop(child, "x"), 4)
  expect_equal(prop(child, "y"), "e")

  parent <- methods::new("Parent", x = 5)
  child <- methods::initialize(child, parent, y = "f")
  expect_equal(prop(child, "x"), 5)
  expect_equal(prop(child, "y"), "f")

  child <- methods::initialize(child, x = 6, x = 7)
  expect_equal(prop(child, "x"), 7)

  expect_error(methods::initialize(child, x = "x"), "invalid")
  expect_error(methods::initialize(child, z = 1), "Property not found")

  expect_error(Child(x = "x", y = "a"))
})

test_that("S7 classes run S4 parent validity", {
  defer(S4_remove_classes(c(
    "S4regValidityParent",
    "S4regValidityChild"
  )))

  setClass("S4regValidityParent", slots = list(x = "numeric"))
  methods::setValidity("S4regValidityParent", function(object) {
    if (length(methods::slot(object, "x")) != 1) {
      "x must have length 1"
    } else {
      TRUE
    }
  })
  S4regValidityChild <- new_class(
    "S4regValidityChild",
    parent = getClass("S4regValidityParent"),
    package = NULL
  )

  expect_error(
    S4regValidityChild(x = numeric()),
    "x must have length 1"
  )

  object <- S4regValidityChild(x = 1)
  methods::slot(object, "x") <- numeric()
  expect_error(validate(object), "x must have length 1")
})

test_that("S4 subclasses of S7-over-S4 classes run S4 parent validity", {
  defer(S4_remove_classes(c(
    "S4regSubValidityParent",
    "S4regSubValidityChild",
    "S4regSubValidityShim"
  )))

  setClass("S4regSubValidityParent", slots = list(x = "numeric"))
  methods::setValidity("S4regSubValidityParent", function(object) {
    if (length(methods::slot(object, "x")) != 1) {
      "x must have length 1"
    } else {
      TRUE
    }
  })
  S4regSubValidityChild := new_class(
    parent = getClass("S4regSubValidityParent"),
    properties = list(y = class_character),
    package = NULL
  )
  S4regSubValidityChild_S4 <- S4_contains(S4regSubValidityChild)
  setClass("S4regSubValidityShim", contains = S4regSubValidityChild_S4)

  object <- methods::new("S4regSubValidityShim", x = 1, y = "a")
  expect_snapshot(error = TRUE, {
    prop(object, "x") <- numeric()
  })

  object <- methods::new("S4regSubValidityShim", x = 1, y = "a")
  expect_snapshot(error = TRUE, {
    props(object) <- list(x = numeric())
  })

  object <- methods::new("S4regSubValidityShim", x = 1, y = "a")
  expect_snapshot(error = TRUE, {
    methods::initialize(object, x = numeric())
  })
})

test_that("S7-over-S4 classes run S7 validators inherited through S4", {
  defer(S4_remove_classes(c(
    "S4regS7HopParent",
    "S4regS7HopMid",
    "S4regS7HopChild"
  )))

  S4regS7HopParent := new_class(
    properties = list(x = class_numeric),
    validator = function(self) {
      if (length(prop(self, "x")) != 1) {
        "x must have length 1"
      }
    },
    package = NULL
  )
  S4_register(S4regS7HopParent)
  methods::setClass(
    Class = "S4regS7HopMid",
    contains = S4_contains(S4regS7HopParent)
  )
  S4regS7HopChild := new_class(
    parent = methods::getClass("S4regS7HopMid"),
    package = NULL
  )

  object <- S4regS7HopChild(x = 1)
  expect_equal(S7_inherits(object, S4regS7HopParent), TRUE)

  expect_snapshot(error = TRUE, {
    prop(object, "x") <- numeric()
  })
})

test_that("S4 subclasses of S7 classes run concrete S4 validity", {
  defer(S4_remove_classes(c(
    "S4regConcreteValidityParent",
    "S4regConcreteValidityChild"
  )))

  S4regConcreteValidityParent := new_class(
    properties = list(x = class_numeric),
    package = NULL
  )
  S4_register(S4regConcreteValidityParent)
  setClass(
    "S4regConcreteValidityChild",
    contains = S4_contains(S4regConcreteValidityParent)
  )
  methods::setValidity("S4regConcreteValidityChild", function(object) {
    if (length(prop(object, "x")) != 1) {
      "x must have length 1"
    } else {
      TRUE
    }
  })

  object <- methods::new("S4regConcreteValidityChild", x = 1)
  expect_snapshot(error = TRUE, {
    prop(object, "x") <- numeric()
  })

  object <- methods::new("S4regConcreteValidityChild", x = 1)
  expect_snapshot(error = TRUE, {
    props(object) <- list(x = numeric())
  })
})

test_that("S4 subclasses of S7 classes check concrete slot types", {
  defer(S4_remove_classes(c(
    "S4regConcreteSlotParent",
    "S4regConcreteSlotChild"
  )))

  S4regConcreteSlotParent := new_class(
    properties = list(x = class_numeric),
    package = NULL
  )
  S4_register(S4regConcreteSlotParent)
  setClass(
    "S4regConcreteSlotChild",
    contains = S4_contains(S4regConcreteSlotParent),
    slots = list(x = "integer")
  )

  expect_snapshot(error = TRUE, {
    methods::new("S4regConcreteSlotChild", x = 1)
  })
})

test_that("S4 subclass validation preserves concrete class package", {
  pkg_env <- local_package("s4regvalidpkg")
  defer({
    suppressMessages({
      S4_remove_classes(c(
        "S4regPackageValidityParent",
        "S4regPackageValidityChild"
      ))
      S4_remove_classes("S4regPackageValidityChild", pkg_env)
    })
  })

  S4regPackageValidityParent := new_class(
    properties = list(x = class_numeric),
    package = NULL
  )
  S4_register(S4regPackageValidityParent)
  setClass("S4regPackageValidityChild")
  pkg_child <- methods::setClass(
    "S4regPackageValidityChild",
    contains = S4_contains(S4regPackageValidityParent),
    where = pkg_env
  )
  methods::setValidity(
    pkg_child@className,
    function(object) {
      if (length(prop(object, "x")) != 1) {
        "x must have length 1"
      } else {
        TRUE
      }
    },
    where = pkg_env
  )

  object <- methods::new(pkg_child@className, x = 1)
  methods::slot(object, "x") <- numeric()

  expect_match(
    methods::validObject(object, test = TRUE),
    "x must have length 1"
  )
  expect_snapshot(error = TRUE, {
    validate(object)
  })
})

test_that("S4 subclass validation preserves skipped superclass packages", {
  pkg_a_env <- local_package("s4regvalidskipa")
  pkg_b_env <- local_package("s4regvalidskipb")
  defer({
    suppressMessages({
      S4_remove_classes(c(
        "S4regValiditySkip",
        "S4regValiditySkipS7",
        "S4regValiditySkipChild"
      ))
      S4_remove_classes(
        c(
          "S4regValiditySkip",
          "S4regValiditySkipPkgAChild"
        ),
        pkg_a_env
      )
      S4_remove_classes("S4regValiditySkip", pkg_b_env)
    })
  })

  pkg_a_base <- methods::setClass(
    "S4regValiditySkip",
    slots = list(x = "numeric"),
    prototype = list(x = 1),
    where = pkg_a_env
  )
  pkg_a_parent <- methods::setClass(
    "S4regValiditySkipPkgAChild",
    contains = pkg_a_base@className,
    where = pkg_a_env
  )
  pkg_b_parent <- methods::setClass(
    "S4regValiditySkip",
    slots = list(x = "numeric"),
    prototype = list(x = 1),
    where = pkg_b_env
  )
  methods::setValidity(
    pkg_b_parent@className,
    function(object) {
      if (length(methods::slot(object, "x")) != 1) {
        "package B x must have length 1"
      } else {
        TRUE
      }
    },
    where = pkg_b_env
  )
  S4regValiditySkipS7 := new_class(
    parent = pkg_a_parent,
    package = NULL
  )
  suppressWarnings(methods::setClass(
    "S4regValiditySkipChild",
    contains = list(
      S4_contains(S4regValiditySkipS7),
      pkg_b_parent@className
    ),
    slots = list(x = "numeric"),
    prototype = list(x = 1)
  ))

  object <- methods::new("S4regValiditySkipChild")
  methods::slot(object, "x") <- numeric()

  expect_match(
    methods::validObject(object, test = TRUE),
    "package B x must have length 1"
  )
  expect_snapshot(error = TRUE, {
    validate(object)
  })
})

test_that("S7 validation preserves S4 parent package during coercion", {
  pkg_env <- local_package("s4regvalidcoercepkg")
  defer({
    suppressMessages({
      S4_remove_classes(c(
        "S4regValidityCoerceParent",
        "S4regValidityCoerceChild"
      ))
      S4_remove_classes("S4regValidityCoerceParent", pkg_env)
    })
  })

  setClass("S4regValidityCoerceParent", slots = list(x = "character"))
  pkg_parent <- methods::setClass(
    "S4regValidityCoerceParent",
    contains = "numeric",
    where = pkg_env
  )
  methods::setValidity(
    pkg_parent@className,
    function(object) {
      if (!identical(as.vector(object), 1)) {
        "package parent saw wrong data"
      } else {
        TRUE
      }
    },
    where = pkg_env
  )
  S4regValidityCoerceChild := new_class(
    parent = pkg_parent,
    package = NULL
  )

  expect_no_error(S4regValidityCoerceChild(.Data = 1))
})

test_that("S4 initialization validates after S4-only slots are set", {
  defer(S4_remove_classes(c(
    "S4regInitValidityParent",
    "S4regInitValidityChild"
  )))

  S4regInitValidityParent := new_class(
    properties = list(x = class_numeric),
    package = NULL
  )
  S4_register(S4regInitValidityParent)
  setClass(
    "S4regInitValidityChild",
    contains = S4_contains(S4regInitValidityParent),
    slots = list(z = "numeric"),
    prototype = list(z = NA_real_)
  )
  methods::setValidity("S4regInitValidityChild", function(object) {
    z <- methods::slot(object, "z")
    if (is.na(z) || z < 0) {
      "z must be non-negative"
    } else {
      TRUE
    }
  })

  object <- methods::new("S4regInitValidityChild", x = 1, z = 2)
  expect_equal(prop(object, "x"), 1)
  expect_equal(methods::slot(object, "z"), 2)
  expect_true(methods::validObject(object))

  object <- methods::new("S4regInitValidityChild", z = 2)
  expect_equal(methods::slot(object, "z"), 2)
  expect_true(methods::validObject(object))

  expect_snapshot(error = TRUE, {
    methods::new("S4regInitValidityChild", z = -1)
  })
})

test_that("S4 subclasses of S7 classes run inherited S4 validity", {
  defer(S4_remove_classes(c(
    "S4regInheritedValidityParent",
    "S4regInheritedValidityS7",
    "S4regInheritedValidityChild"
  )))

  setClass("S4regInheritedValidityParent", slots = list(y = "numeric"))
  methods::setValidity("S4regInheritedValidityParent", function(object) {
    y <- methods::slot(object, "y")
    if (length(y) > 0 && any(y < 0)) {
      "y must be non-negative"
    } else {
      TRUE
    }
  })
  S4regInheritedValidityS7 := new_class(
    properties = list(x = class_character),
    package = NULL
  )
  S4_register(S4regInheritedValidityS7)
  setClass(
    "S4regInheritedValidityChild",
    contains = c(
      S4_contains(S4regInheritedValidityS7),
      "S4regInheritedValidityParent"
    )
  )

  object <- methods::new("S4regInheritedValidityChild", x = "a", y = 1)
  methods::slot(object, "y") <- -1
  expect_snapshot(error = TRUE, {
    prop(object, "x") <- "b"
  })

  object <- methods::new("S4regInheritedValidityChild", x = "a", y = 1)
  methods::slot(object, "y") <- -1
  expect_snapshot(error = TRUE, {
    props(object) <- list(x = "b")
  })

  object <- methods::new("S4regInheritedValidityChild", x = "a", y = 1)
  methods::slot(object, "y") <- -1
  expect_snapshot(error = TRUE, {
    methods::initialize(object, x = "b")
  })
})

test_that("S4 initialization sets S4 slots on subclasses of S7 classes", {
  on.exit(S4_remove_classes(c(
    "ParentForSlots",
    "ChildForSlots",
    "S4ChildForSlots"
  )))
  setClass("ParentForSlots", slots = list(x = "numeric"))

  ChildForSlots <- new_class(
    "ChildForSlots",
    parent = getClass("ParentForSlots"),
    properties = list(y = class_character),
    package = NULL
  )

  setClass(
    "S4ChildForSlots",
    slots = list(z = "character"),
    contains = "ChildForSlots"
  )

  parent <- ChildForSlots(x = 1, y = "a")
  child <- methods::new("S4ChildForSlots", parent, z = "b")

  expect_true(isS4(child))
  expect_true(S7_inherits(child, ChildForSlots))
  expect_equal(prop(child, "x"), 1)
  expect_equal(prop(child, "y"), "a")
  expect_equal(methods::slot(child, "z"), "b")

  child@z <- "c"
  child@y <- "d"
  expect_equal(methods::slot(child, "z"), "c")
  expect_equal(prop(child, "y"), "d")

  child <- methods::new("S4ChildForSlots", child)
  expect_equal(methods::slot(child, "z"), "c")
  expect_equal(prop(child, "y"), "d")
})

test_that("@<- sets S4-only slots on subclasses of S7 classes", {
  on.exit(S4_remove_classes(c("ParentForAt", "ChildForAt", "S4ChildForAt")))
  setClass("ParentForAt", slots = list(x = "numeric"))

  ChildForAt <- new_class(
    "ChildForAt",
    parent = getClass("ParentForAt"),
    properties = list(y = class_character),
    package = NULL
  )

  setClass(
    "S4ChildForAt",
    slots = list(z = "character"),
    contains = "ChildForAt"
  )

  child <- methods::new("S4ChildForAt", ChildForAt(x = 1, y = "a"), z = "b")
  expect_equal(child@z, "b")

  child@z <- "c"
  child@y <- "d"

  expect_equal(child@z, "c")
  expect_equal(methods::slot(child, "z"), "c")
  expect_equal(prop(child, "y"), "d")
})

test_that("S4 initialize supports S3 data parts", {
  on.exit(S4_remove_classes(c("ParentNum", "ChildNum")))
  setClass("ParentNum", contains = "numeric", slots = list(y = "character"))

  ChildNum <- new_class(
    "ChildNum",
    parent = getClass("ParentNum"),
    properties = list(z = class_integer),
    package = NULL
  )
  child <- ChildNum(y = "a", z = 1L)

  child <- methods::initialize(child, 2.5, y = "b")
  expect_equal(as.vector(child), 2.5)
  expect_equal(prop(child, ".Data"), 2.5)
  expect_equal(prop(child, "y"), "b")
  expect_true(methods::validObject(child))

  child <- methods::initialize(child, .Data = 3.5)
  expect_equal(as.vector(child), 3.5)
  expect_true(methods::validObject(child))
})

test_that("S4 initialize strips S7 metadata from data parts", {
  defer(S4_remove_classes(c(
    "S4regDataPartSource",
    "S4regDataPartTarget",
    "S4regDataPartParent"
  )))

  setClass("S4regDataPartParent", contains = "numeric")
  S4regDataPartTarget := new_class(
    parent = getClass("S4regDataPartParent"),
    properties = list(y = class_character),
    package = NULL
  )
  S4regDataPartSource := new_class(
    parent = class_double,
    properties = list(z = class_character),
    package = NULL
  )

  target <- S4regDataPartTarget(.Data = 1, y = "target")
  source <- S4regDataPartSource(.data = 2, z = "source")

  out <- methods::initialize(target, .Data = source)

  expect_equal(S7_class(out), S4regDataPartTarget)
  expect_equal(as.vector(out), 2)
  expect_equal(prop(out, "y"), "target")
  expect_identical(methods::validObject(out), TRUE)
})

test_that("S4 data-part initialization preserves data attributes", {
  defer(S4_remove_classes(c(
    "S4regDataPartAttrsChild",
    "S4regDataPartAttrsParent"
  )))

  setClass("S4regDataPartAttrsParent", contains = "numeric")
  S4regDataPartAttrsChild := new_class(
    parent = getClass("S4regDataPartAttrsParent"),
    package = NULL
  )

  object <- S4regDataPartAttrsChild(.Data = c(a = 1))
  expect_equal(S7_data(object), c(a = 1))

  source <- methods::new("S4regDataPartAttrsParent", .Data = c(b = 2))
  out <- methods::initialize(object, source)
  expect_equal(S7_data(out), c(b = 2))
})

test_that("S4 initialize ignores data-part attributes that collide with slots", {
  defer(S4_remove_classes(c(
    "S4regDataAttrParent",
    "S4regDataAttrChild"
  )))

  setClass(
    "S4regDataAttrParent",
    contains = "numeric",
    slots = list(z = "character")
  )
  S4regDataAttrChild := new_class(
    parent = getClass("S4regDataAttrParent"),
    properties = list(y = class_character),
    package = NULL
  )

  target <- S4regDataAttrChild(.Data = 1, y = "target", z = "slot")
  source <- structure(2, y = "source", z = "source-slot", other = "keep")

  out <- methods::initialize(target, .Data = source)

  expect_equal(as.vector(out), 2)
  expect_equal(prop(out, "y"), "target")
  expect_equal(prop(out, "z"), "slot")
  expect_equal(attr(out, "other", exact = TRUE), "keep")
  expect_identical(methods::validObject(out), TRUE)
})

test_that("S4_register keeps direct .Data properties as ordinary properties", {
  defer(S4_remove_classes("S4regDirectDataProperty"))

  S4regDirectDataProperty := new_class(
    properties = list(.Data = class_numeric),
    package = NULL
  )
  S4_register(S4regDirectDataProperty)

  object <- S4regDirectDataProperty(.Data = 1)

  expect_equal(intersect(methods::slotNames(object), ".Data"), character())
  expect_equal(prop(object, ".Data"), 1)
  expect_identical(methods::validObject(object, test = TRUE), TRUE)
})

test_that("S4 data part constructors use the .Data argument", {
  defer(S4_remove_classes(c(
    "S4regDataPartParent",
    "S4regDataPartChild"
  )))

  setClass("S4regDataPartParent", contains = "numeric")
  methods::setValidity("S4regDataPartParent", function(object) {
    data <- as.vector(object)
    if (!identical(data, 1) && !identical(data, 2)) {
      "data part must be 1 or 2"
    } else {
      TRUE
    }
  })
  S4regDataPartChild <- new_class(
    "S4regDataPartChild",
    parent = getClass("S4regDataPartParent"),
    package = NULL
  )

  object <- S4regDataPartChild(.Data = 1)

  expect_equal(as.vector(object), 1)
  expect_equal(prop(object, ".Data"), 1)
  expect_null(attr(object, ".Data", exact = TRUE))

  prop(object, ".Data") <- 2
  expect_equal(as.vector(object), 2)
  expect_equal(prop(object, ".Data"), 2)
  expect_null(attr(object, ".Data", exact = TRUE))
  expect_true(methods::validObject(object))
})

test_that("S4 data part constructors seed abstract descendants with .Data", {
  defer(S4_remove_classes(c(
    "S4regAbstractDataPartChild",
    "S4regAbstractDataPart",
    "S4regAbstractDataPartParent"
  )))

  setClass("S4regAbstractDataPartParent", contains = c("numeric", "VIRTUAL"))
  S4regAbstractDataPart := new_class(
    parent = getClass("S4regAbstractDataPartParent"),
    abstract = TRUE,
    package = NULL
  )
  S4regAbstractDataPartChild := new_class(
    parent = S4regAbstractDataPart,
    package = NULL
  )

  object <- S4regAbstractDataPartChild(.Data = 1)

  expect_equal(as.vector(object), 1)
  expect_equal(prop(object, ".Data"), 1)
  expect_null(attr(object, ".Data", exact = TRUE))
  expect_true(methods::validObject(object))
})

test_that("S7 subclasses validate S4 parents as the parent representation", {
  defer(S4_remove_classes(c(
    "S4regValidityConcreteParent",
    "S4regValidityConcreteChild"
  )))

  setClass("S4regValidityConcreteParent", contains = "numeric")
  methods::setValidity("S4regValidityConcreteParent", function(object) {
    if (identical(as.character(class(object)), "S4regValidityConcreteParent")) {
      TRUE
    } else {
      "parent validity must see parent class"
    }
  })
  S4regValidityConcreteChild := new_class(
    parent = getClass("S4regValidityConcreteParent"),
    package = NULL
  )

  expect_no_error(S4regValidityConcreteChild(.Data = 1))
})

test_that("S7 subclasses validate initialized S4 parents without reinitializing", {
  defer(S4_remove_classes(c(
    "S4regRequiredInitializeParent",
    "S4regRequiredInitializeChild"
  )))

  setClass("S4regRequiredInitializeParent", slots = list(x = "numeric"))
  methods::setMethod(
    "initialize",
    "S4regRequiredInitializeParent",
    function(.Object, x, ...) {
      .Object <- callNextMethod(.Object, ...)
      .Object@x <- x
      .Object
    }
  )
  methods::setValidity("S4regRequiredInitializeParent", function(object) {
    if (
      identical(as.character(class(object)), "S4regRequiredInitializeParent")
    ) {
      if (identical(methods::slot(object, "x"), 1)) {
        return(TRUE)
      }
      return("parent validity must see initialized slot value")
    } else {
      "parent validity must see parent class"
    }
  })
  S4regRequiredInitializeChild := new_class(
    parent = getClass("S4regRequiredInitializeParent"),
    package = NULL
  )

  object <- S4regRequiredInitializeChild(x = 1)

  expect_equal(prop(object, "x"), 1)
})

test_that("S4 data part constructors use overridden .Data defaults", {
  defer(S4_remove_classes(c(
    "S4regDataDefaultParent",
    "S4regDataDefaultChild",
    "S4regDataDefaultShim"
  )))

  setClass(
    "S4regDataDefaultParent",
    contains = "numeric",
    prototype = 1
  )
  S4regDataDefaultChild <- new_class(
    "S4regDataDefaultChild",
    parent = getClass("S4regDataDefaultParent"),
    properties = list(
      .Data = new_property(class_numeric, default = quote(2))
    ),
    package = NULL
  )
  S4regDataDefaultChild_S4 <- S4_contains(S4regDataDefaultChild)
  setClass("S4regDataDefaultShim", contains = S4regDataDefaultChild_S4)

  expect_equal(as.vector(S4regDataDefaultChild()), 2)

  object <- methods::new("S4regDataDefaultShim")
  expect_equal(as.vector(object), 2)
  expect_equal(prop(object, ".Data"), 2)
})

test_that("S4 data part initialization preserves S4 subclasses", {
  defer(S4_remove_classes(c(
    "S4regDataPartGrandChild",
    "S4regDataPartChild",
    "S4regDataPartParent"
  )))

  setClass("S4regDataPartParent", contains = "numeric")
  S4regDataPartChild := new_class(
    parent = getClass("S4regDataPartParent"),
    properties = list(y = class_character),
    package = NULL
  )
  setClass(
    "S4regDataPartGrandChild",
    slots = list(z = "logical"),
    contains = "S4regDataPartChild"
  )

  object <- methods::new(
    "S4regDataPartGrandChild",
    .Data = 1,
    y = "a",
    z = TRUE
  )

  expect_s4_class(object, "S4regDataPartGrandChild")
  expect_equal(as.vector(object), 1)
  expect_equal(prop(object, "y"), "a")
  expect_equal(methods::slot(object, "z"), TRUE)
  expect_true(methods::validObject(object))
})

test_that("S7_data() reads and writes S4 subclass data parts", {
  defer(S4_remove_classes(c(
    "S4regS7DataPartGrandChild",
    "S4regS7DataPartChild",
    "S4regS7DataPartParent"
  )))

  setClass("S4regS7DataPartParent", contains = "numeric")
  S4regS7DataPartChild := new_class(
    parent = getClass("S4regS7DataPartParent"),
    properties = list(y = class_character),
    package = NULL
  )
  setClass(
    "S4regS7DataPartGrandChild",
    slots = list(z = "logical"),
    contains = "S4regS7DataPartChild"
  )

  object <- methods::new(
    "S4regS7DataPartGrandChild",
    .Data = c(a = 1),
    y = "a",
    z = TRUE
  )

  expect_equal(S7_data(object), c(a = 1))

  S7_data(object) <- c(b = 2)

  expect_s4_class(object, "S4regS7DataPartGrandChild")
  expect_equal(S7_data(object), c(b = 2))
  expect_equal(prop(object, "y"), "a")
  expect_equal(methods::slot(object, "z"), TRUE)
  expect_identical(methods::validObject(object), TRUE)
})

test_that("S7_data() preserves S4 data-part attributes on direct S7 children", {
  defer(S4_remove_classes(c(
    "S4regS7FactorChild",
    "S4regS7FactorParent"
  )))

  setClass("S4regS7FactorParent", contains = "factor")
  suppressWarnings({
    S4regS7FactorChild := new_class(
      parent = getClass("S4regS7FactorParent"),
      package = NULL
    )
  })

  object <- S4regS7FactorChild(
    .Data = 1:2,
    levels = c("a", "b"),
    .S3Class = "factor"
  )

  expect_equal(S7_data(object), factor(c("a", "b")))

  S7_data(object) <- factor("b", levels = c("a", "b", "c"))

  expect_equal(S7_data(object), factor("b", levels = c("a", "b", "c")))
})

test_that("S7_data() keeps data attributes that match renamed S7 properties", {
  defer(S4_remove_classes(c(
    "S4regS7DataNameAttrChild",
    "S4regS7DataNameAttrParent"
  )))

  setClass("S4regS7DataNameAttrParent", contains = "numeric")
  S4regS7DataNameAttrChild := new_class(
    parent = getClass("S4regS7DataNameAttrParent"),
    properties = list(names = class_character),
    package = NULL
  )

  object <- S4regS7DataNameAttrChild(.Data = c(a = 1), names = "prop")

  expect_equal(S7_data(object), c(a = 1))
  expect_equal(prop(object, "names"), "prop")

  S7_data(object) <- c(b = 2)

  expect_equal(S7_data(object), c(b = 2))
  expect_equal(prop(object, "names"), "prop")
  expect_identical(methods::validObject(object), TRUE)
})

test_that("S4 subclasses read and write special-named S7 property storage slots", {
  defer(S4_remove_classes(c(
    "S4regSpecialSlotsChild",
    "S4regSpecialSlots"
  )))

  S4regSpecialSlots <- new_class(
    "S4regSpecialSlots",
    properties = list(
      names = class_character,
      dim = class_integer
    ),
    package = NULL
  )
  S4_register(S4regSpecialSlots)
  S4regSpecialSlots_S4 <- S4_contains(S4regSpecialSlots)
  setClass(
    "S4regSpecialSlotsChild",
    contains = S4regSpecialSlots_S4,
    slots = list(names = "character")
  )

  object <- methods::new(
    "S4regSpecialSlotsChild",
    names = "n",
    dim = 2L
  )
  methods::slot(object, "names") <- "s4-only"

  expect_equal(methods::slot(object, "_names"), "n")
  expect_equal(methods::slot(object, "_dim"), 2L)
  expect_equal(methods::slot(object, "names"), "s4-only")
  expect_equal(prop(object, "names"), "n")
  expect_true(methods::validObject(object))

  storage_object <- methods::new(
    "S4regSpecialSlotsChild",
    `_names` = "storage",
    `_dim` = 3L
  )

  expect_equal(methods::slot(storage_object, "_names"), "storage")
  expect_equal(methods::slot(storage_object, "_dim"), 3L)
  expect_equal(prop(storage_object, "names"), "storage")

  prop(object, "names") <- "updated"
  expect_equal(methods::slot(object, "_names"), "updated")
  expect_equal(methods::slot(object, "names"), "s4-only")
  expect_equal(prop(object, "names"), "updated")
})

test_that("S4 initializers copy renamed S7 property storage slots", {
  defer(S4_remove_classes(c(
    "S4regCopySpecialSlotsChild",
    "S4regCopySpecialSlots"
  )))

  S4regCopySpecialSlots := new_class(
    properties = list(names = class_character),
    package = NULL
  )
  S4_register(S4regCopySpecialSlots)
  S4regCopySpecialSlots_S4 <- S4_contains(S4regCopySpecialSlots)
  setClass(
    "S4regCopySpecialSlotsChild",
    contains = S4regCopySpecialSlots_S4,
    slots = list(extra = "character")
  )

  old <- methods::new(
    "S4regCopySpecialSlotsChild",
    names = "n",
    extra = "old"
  )
  new <- methods::new("S4regCopySpecialSlotsChild", old)

  expect_equal(prop(new, "names"), "n")
  expect_equal(methods::slot(new, "_names"), "n")
  expect_equal(methods::slot(new, "extra"), "old")
})

test_that("S4_register keeps direct S7 special-name property slots valid", {
  defer(S4_remove_classes("S4regDirectSpecialSlots"))

  S4regDirectSpecialSlots <- new_class(
    "S4regDirectSpecialSlots",
    properties = list(
      names = class_character,
      dim = class_integer
    ),
    package = NULL
  )
  S4_register(S4regDirectSpecialSlots)

  object <- S4regDirectSpecialSlots(
    names = "n",
    dim = 2L
  )

  expect_equal(methods::slot(object, "_names"), "n")
  expect_equal(methods::slot(object, "_dim"), 2L)
  expect_equal(prop(object, "names"), "n")
  expect_true(methods::validObject(object))
})

test_that("S7 classes preserve special-named properties inherited through S4", {
  defer(S4_remove_classes(c(
    "S4regSpecialSlotGrand",
    "S4regSpecialSlotMiddle",
    "S4regSpecialSlotParent"
  )))

  S4regSpecialSlotParent := new_class(
    properties = list(names = class_character),
    package = NULL
  )
  S4_register(S4regSpecialSlotParent)
  methods::setClass(
    "S4regSpecialSlotMiddle",
    contains = S4_contains(S4regSpecialSlotParent)
  )
  S4regSpecialSlotGrand := new_class(
    parent = methods::getClass("S4regSpecialSlotMiddle"),
    package = NULL
  )

  object <- S4regSpecialSlotGrand(names = "n")

  expect_equal(prop_names(object), "names")
  expect_equal(prop(object, "names"), "n")
  expect_equal(methods::slot(object, "_names"), "n")
  expect_equal(S7_inherits(object, S4regSpecialSlotParent), TRUE)
  expect_identical(methods::validObject(object), TRUE)
})

test_that("S7 classes preserve .S3Class slots inherited from S4 parents", {
  defer(S4_remove_classes(c(
    "S4regS3ClassSlotChild",
    "S4regS3ClassSlotParent",
    "S4regS3ClassSlotOld",
    "S4regS3ClassSlotBase"
  )))

  old_classes <- c("S4regS3ClassSlotOld", "S4regS3ClassSlotBase")
  methods::setOldClass(old_classes)
  methods::setClass(
    "S4regS3ClassSlotParent",
    contains = "S4regS3ClassSlotOld"
  )
  S4regS3ClassSlotChild := new_class(
    parent = methods::getClass("S4regS3ClassSlotParent"),
    package = NULL
  )

  object <- S4regS3ClassSlotChild()

  expect_equal(prop(object, ".S3Class"), old_classes)
  expect_equal(
    S7_inherits(object, methods::getClass("S4regS3ClassSlotParent")),
    TRUE
  )
  parent <- convert(object, to = methods::getClass("S4regS3ClassSlotParent"))
  expect_s4_class(parent, "S4regS3ClassSlotParent")
  expect_equal(methods::slot(parent, ".S3Class"), old_classes)
})

test_that("S4 classes can not extend S7-over-S4 classes with property setters", {
  on.exit(S4_remove_classes(c("Parent2", "Child2", "S4Child2")))
  setClass("Parent2", slots = list(x = "numeric"))

  Child2 <- new_class(
    "Child2",
    parent = getClass("Parent2"),
    properties = list(
      y = new_property(class_character, setter = function(self, value) {
        attr(self, "setter_called") <- TRUE
        attr(self, "y") <- value
        self
      })
    ),
    package = NULL
  )
  expect_true(attr(Child2(x = 1, y = "a"), "setter_called", exact = TRUE))
  expect_error(
    S4_contains(Child2),
    "custom setter"
  )

  methods::setClass("S4Child2", contains = "Child2")
  expect_error(
    methods::new("S4Child2"),
    "custom setter"
  )
})

test_that("S4 parents reject slots that need renamed S7 storage", {
  defer(S4_remove_classes(c(
    "S4regRenamedSlotChild",
    "S4regRenamedSlotParent"
  )))

  setClass("S4regRenamedSlotParent", slots = list(names = "character"))

  expect_snapshot(error = TRUE, {
    new_class(
      "S4regRenamedSlotChild",
      parent = getClass("S4regRenamedSlotParent"),
      package = NULL
    )
  })
})

test_that("S4 parents reject accessor overrides of inherited slots", {
  defer(S4_remove_classes(c(
    "S4regAccessorSlotGetterChild",
    "S4regAccessorSlotParent",
    "S4regAccessorSlotSetterChild"
  )))

  setClass("S4regAccessorSlotParent", slots = list(x = "numeric"))

  expect_snapshot(error = TRUE, {
    new_class(
      "S4regAccessorSlotGetterChild",
      parent = getClass("S4regAccessorSlotParent"),
      properties = list(
        x = new_property(class_numeric, getter = function(self) 1)
      ),
      package = NULL
    )
  })

  expect_snapshot(error = TRUE, {
    new_class(
      "S4regAccessorSlotSetterChild",
      parent = getClass("S4regAccessorSlotParent"),
      properties = list(
        x = new_property(class_numeric, setter = function(self, value) self)
      ),
      package = NULL
    )
  })
})

test_that("S7 subclasses reject accessor overrides of inherited S4 slots", {
  defer(S4_remove_classes(c(
    "S4regS7AccessorSlotGetterChild",
    "S4regS7AccessorSlotParent",
    "S4regS7AccessorSlotSetterChild"
  )))

  setClass("S4regS7AccessorSlotParent", slots = list(x = "numeric"))
  Parent := new_class(
    parent = getClass("S4regS7AccessorSlotParent"),
    package = NULL
  )

  expect_snapshot(error = TRUE, {
    new_class(
      "S4regS7AccessorSlotGetterChild",
      parent = Parent,
      properties = list(
        x = new_property(class_numeric, getter = function(self) 1)
      ),
      package = NULL
    )
  })

  expect_snapshot(error = TRUE, {
    new_class(
      "S4regS7AccessorSlotSetterChild",
      parent = Parent,
      properties = list(
        x = new_property(class_numeric, setter = function(self, value) self)
      ),
      package = NULL
    )
  })
})

test_that("S7 property narrowing rejects unrelated S4 name matches", {
  defer(S4_remove_classes(c(
    "S4regPropertyNameCollisionChild",
    "S4regPropertyNameCollisionParent"
  )))

  setClass("S4regPropertyNameCollisionParent")
  setClass(
    "S4regPropertyNameCollisionChild",
    contains = "S4regPropertyNameCollisionParent"
  )

  S4regPropertyNameCollisionParent := new_class(package = NULL)
  Parent := new_class(
    properties = list(x = S4regPropertyNameCollisionParent),
    package = NULL
  )

  expect_snapshot(error = TRUE, {
    new_class(
      "S4regPropertyNameCollisionOverride",
      parent = Parent,
      properties = list(x = getClass("S4regPropertyNameCollisionChild")),
      package = NULL
    )
  })
})


test_that("S4_class_dispatch returns name of base class", {
  Foo1 := local_S4_class(slots = list("x" = "numeric"))
  expect_equal(S4_class_dispatch("Foo1"), "S4/S7::Foo1")
})

test_that("S4_class_dispatch respects single inheritance hierarchy", {
  Foo1 := local_S4_class(slots = list("x" = "numeric"))
  Foo2 := local_S4_class(contains = "Foo1")
  Foo3 := local_S4_class(contains = "Foo2")
  expect_equal(
    S4_class_dispatch("Foo3"),
    c("S4/S7::Foo3", "S4/S7::Foo2", "S4/S7::Foo1")
  )
})

test_that("S4_class_dispatch performs breadth first search for multiple dispatch", {
  Foo1a := local_S4_class(slots = list("x" = "numeric"))
  Foo1b := local_S4_class(contains = "Foo1a")
  Foo2a := local_S4_class(slots = list("x" = "numeric"))
  Foo2b := local_S4_class(contains = "Foo2a")
  Foo3 := local_S4_class(contains = c("Foo1b", "Foo2b"))
  expect_equal(
    S4_class_dispatch("Foo3"),
    c(
      "S4/S7::Foo3",
      "S4/S7::Foo1b",
      "S4/S7::Foo2b",
      "S4/S7::Foo1a",
      "S4/S7::Foo2a"
    )
  )
})

test_that("S4_class_dispatch handles extensions of base classes", {
  Foo1 := local_S4_class(contains = "character")
  expect_equal(S4_class_dispatch("Foo1"), c("S4/S7::Foo1", "character"))
})

test_that("S4_class_dispatch handles extensions of S3 classes", {
  setOldClass(c("Soo1", "Soo"))
  defer(S4_remove_classes("Soo1"))
  Foo2 := local_S4_class(contains = "Soo1")
  Foo3 := local_S4_class(contains = "Foo2")
  expect_equal(
    S4_class_dispatch("Foo3"),
    c("S4/S7::Foo3", "S4/S7::Foo2", "Soo1", "Soo")
  )
})

test_that("S4_class_dispatch ignores unions", {
  Foo1 := local_S4_class(slots = list("x" = "numeric"))
  Foo2 := local_S4_class(slots = list("x" = "numeric"))
  Foo3 := local_S4_union(c("Foo1", "Foo2"))

  expect_equal(S4_class_dispatch("Foo1"), "S4/S7::Foo1")
  expect_equal(S4_class_dispatch("Foo2"), "S4/S7::Foo2")
})

test_that("S4_class_dispatch dispatches through the full S3 old-class hierarchy", {
  setOldClass(c("S4OldS3a", "S4OldS3b"))
  defer(S4_remove_classes(c("S4OldS3Child", "S4OldS3a", "S4OldS3b")))
  setClass("S4OldS3Child", contains = "S4OldS3a")

  generic <- new_generic("S4OldS3Generic", "x")
  method(generic, new_S3_class("S4OldS3b")) <- function(x) "S3 parent"

  object <- methods::new("S4OldS3Child")

  expect_contains(obj_dispatch(object), "S4OldS3b")
  expect_equal(generic(object), "S3 parent")
})

test_that("S4_class_dispatch includes virtual classes", {
  Foo1 := local_S4_class()
  Foo2 := local_S4_class(contains = "Foo1")

  expect_equal(S4_class_dispatch("Foo1"), "S4/S7::Foo1")
  expect_equal(S4_class_dispatch("Foo2"), c("S4/S7::Foo2", "S4/S7::Foo1"))
})

test_that("S4_class_dispatch captures explicit package name", {
  Foo1 := local_S4_class(package = "pkg")
  expect_equal(S4_class_dispatch("Foo1"), "S4/pkg::Foo1")
})

test_that("S4_class_dispatch captures implicit package name", {
  env <- new.env()
  env$.packageName <- "mypkg"
  Foo1 := local_S4_class(where = env)
  expect_equal(S4_class_dispatch("Foo1"), "S4/mypkg::Foo1")
})

test_that("S7 class extending S4 class with multiple parents works", {
  on.exit(S4_remove_classes(c(
    "MultiParent1",
    "MultiParent2",
    "MultiChild",
    "S7MultiChild",
    "S7MultiChild2"
  )))

  setClass("MultiParent1", slots = list(x = "numeric"))
  setClass("MultiParent2", slots = list(y = "numeric"))
  setClass("MultiChild", contains = c("MultiParent1", "MultiParent2"))

  S7MultiChild <- new_class(
    "S7MultiChild",
    parent = getClass("MultiChild"),
    properties = list(z = class_numeric),
    package = NULL
  )

  S7MultiChild2 <- new_class(
    "S7MultiChild2",
    parent = S7MultiChild,
    properties = list(w = class_numeric),
    package = NULL
  )

  obj <- S7MultiChild2(x = 1, y = 2, z = 3, w = 4)
  expect_true(S7_inherits(obj, S7MultiChild2))
  expect_true(S7_inherits(obj, S7MultiChild))
  expect_equal(prop(obj, "x"), 1)
  expect_equal(prop(obj, "y"), 2)
  expect_equal(prop(obj, "z"), 3)
  expect_equal(prop(obj, "w"), 4)

  expect_true(methods::is(obj, "MultiParent1"))
  expect_true(methods::is(obj, "MultiParent2"))
})

test_that("S4_package_name resolves S4 package name correctly in all cases", {
  stats_ns <- asNamespace("stats")

  # Case 1: Generic in declared package
  show_S4 <- getGeneric("show")
  expect_equal(S4_package_name(show_S4, stats_ns), "methods")

  # Case 2: Current package bypass
  dummy_S4 <- getGeneric("show")
  dummy_S4@package <- "stats"
  expect_equal(S4_package_name(dummy_S4, stats_ns), "stats")

  # Case 3: Originating package in namespace imports
  methods::setGeneric("axTicks")
  on.exit(methods::removeGeneric("axTicks"))
  mock_S4 <- getGeneric("axTicks")
  mock_S4@package <- "base"
  expect_equal(S4_package_name(mock_S4, stats_ns), "graphics")
})

test_that("S4_package_name errors if originating package can't be found", {
  stats_ns <- asNamespace("stats")

  methods::setGeneric("nonexistent_generic", function(x) {
    standardGeneric("nonexistent_generic")
  })
  on.exit(methods::removeGeneric("nonexistent_generic"))
  mock_S4 <- getGeneric("nonexistent_generic")
  mock_S4@package <- "base"
  expect_snapshot(S4_package_name(mock_S4, stats_ns), error = TRUE)
})
