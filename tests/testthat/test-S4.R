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
  expect_true("S7_class" %in% methods::slotNames("S4regS7New"))
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

test_that("S4_register_contains registers S7 properties as slots for S4 subclasses", {
  on.exit({
    if (methods::isGeneric("S4regContainsGeneric")) {
      methods::removeGeneric("S4regContainsGeneric")
    }
    S4_remove_classes(c(
      "S4regContainsS4Child",
      "S7::S4regContains",
      "S7::S4regContainsChild",
      "S7::S4regContainsChild::S4Slots"
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
  S4regContainsChild_S4 <- S4_register_contains(S4regContainsChild)
  expect_equal(S4regContainsChild_S4, "S7::S4regContainsChild::S4Slots")
  expect_equal(
    methods::slotNames(S4regContainsChild_S4),
    c("x", "y", "S7_class", ".S3Class")
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
  expect_equal(methods::slot(object, "S7_class"), S4regContainsChild)
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

test_that("S4_register_contains constructs S4 subclasses of S7 classes that extend S4 classes", {
  on.exit(S4_remove_classes(c(
    "S4regNewParent",
    "S4regNewMiddle",
    "S4regNewChild",
    "S4regNewChild::S4Slots",
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
  S4regNewChild_S4 <- S4_register_contains(S4regNewChild)
  expect_equal(
    methods::slotNames("S4regNewChild"),
    c("status", "metadata", "S7_class", "assays", "rowData", ".S3Class")
  )
  expect_contains(
    methods::slotNames(S4regNewChild_S4),
    c("assays", "rowData", "metadata", "status", "S7_class")
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
  expect_equal(methods::slot(object, "S7_class"), S4regNewChild)
  expect_equal(methods::slot(object, "assays"), list())
  expect_equal(methods::slot(object, "rowData"), character())
  expect_equal(methods::slot(object, "metadata"), character())
  expect_equal(methods::slot(object, "status"), character())
  object_shim <- methods::as(object, S4regNewChild_S4)
  object_old <- methods::as(object_shim, "S4regNewChild")
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

test_that("S4_validate_shim validates only matching S7 classes for S4 shim upcasts", {
  on.exit(S4_remove_classes(c(
    "S4regShimRoot",
    "S4regShimParent",
    "S4regShimChild",
    "S4regShimGrandChild",
    "S7::S4regShimParent",
    "S7::S4regShimChild",
    "S7::S4regShimParent::S4Slots",
    "S7::S4regShimChild::S4Slots"
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

  S4regShimParent_S4 <- S4_register_contains(S4regShimParent)
  S4regShimChild_S4 <- S4_register_contains(S4regShimChild)
  setClass("S4regShimParent", contains = S4regShimParent_S4)
  setClass(
    "S4regShimChild",
    contains = c(S4regShimChild_S4, "S4regShimParent")
  )
  setClass("S4regShimGrandChild", contains = "S4regShimChild")

  object <- methods::new("S4regShimGrandChild", root = 1, x = 2, y = "a")
  parent_shim <- methods::as(object, S4regShimParent_S4)

  expect_false("y" %in% methods::slotNames(parent_shim))
  expect_equal(S7_class(parent_shim), S4regShimChild)
  expect_true(methods::validObject(object))
})

test_that("S4_register registers abstract S7 classes as virtual S4 classes", {
  on.exit({
    try(methods::removeMethod("dim", "S4regAbstractConcrete"), silent = TRUE)
    S4_remove_classes(c(
      "S4regAbstractParent",
      "S4regAbstract",
      "S4regAbstractConcrete",
      "S4regAbstractConcrete::S4Slots",
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
  method(dim, S4regAbstractConcrete) <- function(x) {
    c(methods::slot(x, "x"), 2L)
  }
  S4regAbstractConcrete_S4 <- S4_register_contains(S4regAbstractConcrete)
  setClass("S4regAbstractShim", contains = S4regAbstractConcrete_S4)

  object <- methods::new("S4regAbstractShim", x = 1L)
  concrete_prototype <- methods::getClass("S4regAbstractConcrete")@prototype

  expect_true(methods::isVirtualClass("S4regAbstract"))
  expect_false(methods::extends("S4regAbstract", "oldClass"))
  expect_false(methods::extends("S4regAbstract", "S7_object"))
  expect_false("S4regAbstract" %in% attr(concrete_prototype, ".S3Class"))
  expect_equal(dim(object), c(1L, 2L))
  expect_true(methods::validObject(object))
})

test_that("S4_register_contains uses S7 property defaults as S4 shim prototypes", {
  on.exit(S4_remove_classes(c(
    "S4regPrototype",
    "S4regPrototype::S4Slots",
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
  S4regPrototype_S4 <- S4_register_contains(S4regPrototype)
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

test_that("S4_register_contains treats S4 NULL slot sentinels as NULL-valued S7 properties", {
  on.exit(S4_remove_classes(c(
    "S4regNullable",
    "S4regNullable::S4Slots",
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
  S4regNullable_S4 <- S4_register_contains(S4regNullable)
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

test_that("S4_register_contains rejects properties that can not be represented as slots", {
  on.exit(S4_remove_classes(c(
    "S7::S4regContainsDynamic",
    "S7::S4regContainsSetter",
    "S7::S4regContainsDynamic::S4Slots",
    "S7::S4regContainsSetter::S4Slots"
  )))

  S4regContainsDynamic <- new_class(
    "S4regContainsDynamic",
    properties = list(
      x = new_property(class_numeric, getter = function(self) 1)
    ),
    package = "S7"
  )
  expect_error(
    S4_register_contains(S4regContainsDynamic),
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
  expect_error(
    S4_register_contains(S4regContainsSetter),
    "custom setter"
  )
})

test_that("S4_register_contains uses registered S7 unions as S4 slots", {
  on.exit(S4_remove_classes(c(
    "S7::S4regContainsUnion",
    "S7::S4regContainsUnion::S4Slots",
    "integer_OR_numeric_OR_character"
  )))

  S4regContainsUnion <- new_class(
    "S4regContainsUnion",
    properties = list(x = class_numeric | class_character),
    package = "S7"
  )
  expect_error(
    S4_register_contains(S4regContainsUnion),
    "not been registered"
  )

  S4_register(class_numeric | class_character)
  S4regContainsUnion_S4 <- S4_register_contains(S4regContainsUnion)
  expect_equal(
    as.character(methods::getClass(S4regContainsUnion_S4)@slots$x),
    "integer_OR_numeric_OR_character"
  )
})

test_that("S4_register_contains uses matching S4 unions as S4 slots", {
  env <- topenv(environment())
  on.exit(S4_remove_classes(
    c(
      "S4regContainsExistingUnion::S4Slots",
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

  S4regContainsExistingUnion_S4 <- S4_register_contains(
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
  expect_equal(S4_to_S7_class(getClass("character")), class_character)
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
  expect_equal(methods::slotNames("Child"), c("y", "S7_class", "x", ".S3Class"))
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

test_that("S4 initialization sets S4 slots on subclasses of S7 classes", {
  on.exit(S4_remove_classes(c("ParentForSlots", "ChildForSlots", "S4ChildForSlots")))
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
  child@z <- "c"
  child@y <- "d"

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

test_that("S4 classes can not extend S7-over-S4 classes with property setters", {
  on.exit(S4_remove_classes(c("Parent2", "Child2")))
  setClass("Parent2", slots = list(x = "numeric"))

  expect_error(
    new_class(
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
    ),
    "custom setter"
  )
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
