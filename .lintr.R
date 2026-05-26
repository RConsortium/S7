linters <- lintr::linters_with_defaults(
  object_name_linter = lintr::object_name_linter(
    styles = "symbols",
    regexes = c(symbols = "^(?:\\.)?[[:alnum:]]+(?:[._][[:alnum:]]+)*$")
  )
)
