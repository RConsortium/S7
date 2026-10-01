BaseBox := new_class(properties = list(item = deprecatedDefaults::Base))
WarnBox := new_class(properties = list(item = deprecatedDefaults::Warn))
StopBox := new_class(properties = list(item = deprecatedDefaults::Stop))

Base := new_external_class(package = "deprecatedDefaults")
Warn := new_external_class(package = "deprecatedDefaults")
Stop := new_external_class(package = "deprecatedDefaults")
BaseExternal := new_class(properties = list(item = Base))
WarnExternal := new_class(properties = list(item = Warn))
StopExternal := new_class(properties = list(item = Stop))

BaseExplicit := new_class(
  properties = list(
    item = new_property(
      deprecatedDefaults::Base,
      default = quote(deprecatedDefaults::Base())
    )
  )
)
WarnExplicit := new_class(
  properties = list(
    item = new_property(
      deprecatedDefaults::Warn,
      default = quote(deprecatedDefaults::Warn())
    )
  )
)
StopExplicit := new_class(
  properties = list(
    item = new_property(
      deprecatedDefaults::Stop,
      default = quote(deprecatedDefaults::Stop())
    )
  )
)
.onLoad <- function(...) S7_on_load()
