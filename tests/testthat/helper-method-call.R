method_context <- function(x) {
  list(
    call = S7_generic_call(skip = "super"),
    sentinel = eval(quote(sentinel), S7_user_frame(skip = "super"))
  )
}
