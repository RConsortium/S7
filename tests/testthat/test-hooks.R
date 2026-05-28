describe("S7_on_build()", {
  it("removes only generic sentinels from the namespace", {
    ns <- new.env(parent = emptyenv())
    ns$keep_fun <- function() {}
    ns$keep_ext <- new_external_generic("pkg", "gen", "x")
    ns$drop_me <- generic_sentinel(new_external_generic("pkg", "gen", "x"))

    strip_generic_sentinels(ns)
    expect_setequal(names(ns), c("keep_ext", "keep_fun"))
  })

  it("is a no-op when there are no sentinels", {
    ns <- new.env(parent = emptyenv())
    ns$x <- 1
    ns$f <- function() {}
    strip_generic_sentinels(ns)
    expect_setequal(names(ns), c("f", "x"))
  })
})
