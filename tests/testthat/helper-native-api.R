local_native_api <- local({
  lib <- NULL
  function(frame = parent.frame()) {
    local_dev_S7_lib(frame = frame)
    if (is.null(lib)) {
      dir.create(lib <<- tempfile("S7-native-api-"))
      install.packages(
        test_path("nativeapi"),
        lib = lib,
        repos = NULL,
        type = "source",
        quiet = TRUE,
        INSTALL_opts = c("--preclean", "--clean", "--no-multiarch")
      )
    }
    ns <- loadNamespace("nativeapi", lib.loc = lib)
    defer(unloadNamespace("nativeapi"), frame = frame)
    ns
  }
})
