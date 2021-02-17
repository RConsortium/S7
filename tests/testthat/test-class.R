describe("r7_class", {
  my_class <- class_new("my_class")
  it("has a character name", {
    expect_type(my_class@name, "character")
  })
  it("is an instance of r7_class and object", {
    expect_equal(class(my_class), c("r7_class", "object"))
  })
  it("are first class R7 objects themselves", {
    expect_equal(object_class(my_class), my_class)
  })
  it("has a constructor function", {
    expect_type(my_class@constructor, "closure")
  })
  it("has a validator function", {
    expect_type(my_class@validator, "closure")
  })
  it("has a list of properties", {
    expect_type(my_class@properties, "list")
  })
})
