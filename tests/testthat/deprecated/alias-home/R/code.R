Foo := deprecated_class(
  new = deprecatedAliasCore::Bar,
  when = "2.0.0",
  alias = TRUE
)
Child := new_class(parent = Foo)
Holder := new_class(properties = list(item = Foo))
