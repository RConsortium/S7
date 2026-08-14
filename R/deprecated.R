#' Deprecate a generic
#'
#' @description
#' If you rename a generic, move it to another package, or retire it without
#' a replacement, `deprecated_generic()` lets old code keep working while
#' warning users that they need to update. It creates a function that you
#' export under the old name:
#'
#' * Calling it signals a deprecation warning, then delegates to `new`.
#' * Methods registered on it with [method<-] are silently registered on
#'   `new`, so downstream packages continue to work.
#'
#' To deprecate a generic that has no replacement, supply the generic itself
#' as `old`: it continues to power the deprecated name, but calls warn.
#'
#' @param name The old name of the generic, as a string. As with
#'   [new_generic()], the result should be assigned to a variable with this
#'   name, most easily with [:=].
#' @param new The replacement: an S7 generic, usually the renamed generic, or
#'   a generic that now lives in another package.
#' @param old For a deprecation without a replacement: the existing generic,
#'   which continues to power the deprecated name.
#' @param when The package version when the deprecation began, e.g.
#'   `"1.2.0"`.
#' @param method How to signal the deprecation:
#'
#'   * `"base"` (the default): a [.Deprecated()]-style warning.
#'   * `"lifecycle(warn)"`: [lifecycle::deprecate_warn()], a warning that's
#'     only displayed once every eight hours.
#'   * `"lifecycle(stop)"`: [lifecycle::deprecate_stop()], an error.
#'
#'   The lifecycle options require the lifecycle package to be installed,
#'   and to be a dependency of your package.
#' @returns A function with class `S7_deprecated_generic`.
#' @seealso [deprecated_class()] and [deprecated_property()] to deprecate
#'   other parts of your API.
#' @export
#' @examples
#' # A generic renamed from summarise() to summarize():
#' summarize := new_generic("x")
#' method(summarize, class_double) <- function(x) mean(x)
#' summarise := deprecated_generic(new = summarize, when = "1.1.0")
#' # Calling the old name warns, then delegates:
#' summarise(c(1, 2, 3))
#'
#' # Registering a method on the old name registers it on the new generic:
#' method(summarise, class_character) <- function(x) unique(x)
#' summarize(c("a", "b", "a"))
#'
#' # A generic deprecated without a replacement keeps working:
#' shout := new_generic("x")
#' method(shout, class_character) <- function(x) toupper(x)
#' shout := deprecated_generic(old = shout, when = "2.0.0")
#' shout("hi")
deprecated_generic <- function(
  name,
  new = NULL,
  old = NULL,
  when,
  method = c("base", "lifecycle(warn)", "lifecycle(stop)")
) {
  check_name(name)
  check_when(when)
  method <- check_deprecate_method(method)
  env <- parent.frame()
  package <- topNamespaceName(env)

  if (is.null(new) == is.null(old)) {
    stop2("Must supply exactly one of `new` and `old`.")
  }

  if (is.null(new)) {
    if (!is_S7_generic(old)) {
      msg <- sprintf("`old` must be an S7 generic, not %s.", obj_desc(old))
      stop2(msg)
    }
    if (!identical(old@name, name)) {
      msg <- c(
        sprintf(
          "`old@name` (\"%s\") must match `name` (\"%s\").",
          old@name,
          name
        ),
        "* To deprecate in favor of a renamed generic, use `new`."
      )
      stop2(msg)
    }
    target <- old
    with <- NULL
  } else {
    if (is_deprecated_generic(new)) {
      new <- deprecated_target(new)
    }
    if (!is_S7_generic(new)) {
      msg <- sprintf("`new` must be an S7 generic, not %s.", obj_desc(new))
      stop2(msg)
    }
    target <- new
    with <- target_label(package_name(target), target@name, package)
  }

  new_deprecated_fun(
    target = target,
    what = paste0(name, "()"),
    with = with,
    when = when,
    package = package,
    method = method,
    env = env,
    class = "S7_deprecated_generic"
  )
}

is_deprecated_generic <- function(x) inherits(x, "S7_deprecated_generic")

#' Deprecate a class
#'
#' @description
#' If you rename a class or retire it without a replacement,
#' `deprecated_class()` lets old code keep working while warning users that
#' they need to update. It creates an alias that you export under the old
#' name:
#'
#' * Calling the constructor signals a deprecation warning, then constructs
#'   an instance of `new`.
#' * In every other context (method signatures, `parent`, property classes,
#'   [new_external_class()] references) it is silently treated as `new`.
#'
#' To deprecate a class that has no replacement, supply the class itself as
#' `old`: it continues to power the deprecated name, but constructing an
#' instance warns.
#'
#' @param name The old name of the class, as a string. As with [new_class()],
#'   the result should be assigned to a variable with this name, most easily
#'   with [:=].
#' @param new The replacement: an S7 class, usually the renamed class.
#' @param old For a deprecation without a replacement: the existing class,
#'   which continues to power the deprecated name. Its name must match
#'   `name`.
#' @inheritParams deprecated_generic
#' @returns A function with class `S7_deprecated_class`.
#' @seealso [deprecated_generic()] and [deprecated_property()] to deprecate
#'   other parts of your API.
#' @export
#' @examples
#' # A class renamed from Dog to Pet:
#' Pet := new_class(properties = list(name = class_character))
#' Dog := deprecated_class(new = Pet, when = "2.0.0")
#'
#' # Calling the old constructor warns, then constructs the new class:
#' Dog(name = "Fido")
#'
#' # In method signatures the old name silently means the new class:
#' speak := new_generic("x")
#' method(speak, Dog) <- function(x) "Woof!"
#' speak(Pet(name = "Rex"))
#'
#' # A class deprecated without a replacement keeps working:
#' Cat := new_class(properties = list(lives = class_double))
#' Cat := deprecated_class(old = Cat, when = "3.0.0")
#' Cat(lives = 9)
deprecated_class <- function(
  name,
  new = NULL,
  old = NULL,
  when,
  method = c("base", "lifecycle(warn)", "lifecycle(stop)")
) {
  check_name(name)
  check_when(when)
  method <- check_deprecate_method(method)
  env <- parent.frame()
  package <- topNamespaceName(env)

  if (is.null(new) == is.null(old)) {
    stop2("Must supply exactly one of `new` and `old`.")
  }

  if (is.null(new)) {
    if (!is_class(old)) {
      msg <- sprintf("`old` must be an S7 class, not %s.", obj_desc(old))
      stop2(msg)
    }
    if (!identical(old@name, name)) {
      msg <- c(
        sprintf(
          "`old@name` (\"%s\") must match `name` (\"%s\").",
          old@name,
          name
        ),
        "* To deprecate in favor of a renamed class, use `new`."
      )
      stop2(msg)
    }
    target <- old
    with <- NULL
  } else {
    if (is_deprecated_class(new)) {
      new <- deprecated_target(new)
    }
    if (!is_class(new)) {
      msg <- sprintf("`new` must be an S7 class, not %s.", obj_desc(new))
      stop2(msg)
    }
    target <- new
    with <- target_label(target@package, target@name, package)
  }

  new_deprecated_fun(
    target = target,
    what = paste0(name, "()"),
    with = with,
    when = when,
    package = package,
    method = method,
    env = env,
    class = "S7_deprecated_class"
  )
}

is_deprecated_class <- function(x) inherits(x, "S7_deprecated_class")

#' Deprecate a property
#'
#' @description
#' If you rename a property or retire it without a replacement,
#' `deprecated_property()` lets old code keep working while warning users
#' that they need to update. It creates a property that signals a
#' deprecation warning when it is read or written, delegating storage to the
#' property named `new`.
#'
#' To deprecate a property that has no replacement, omit `new`: the property
#' stores data as usual, but warns when read or written. (Unlike a property
#' with a replacement, supplying a value to the constructor does not warn,
#' because S7 can't distinguish a user-supplied value from the default.)
#'
#' @param old The name of the deprecated property, as a string. Because the
#'   name is part of the property itself, the `properties` list entry doesn't
#'   need to be named.
#' @param new The name of the replacement property, as a string. If `NULL`,
#'   the property is deprecated without a replacement.
#' @param class,default The property `class` and `default`, as in
#'   [new_property()]. When `new` is supplied, `default` defaults to the
#'   value of the replacement property so that construction only warns when
#'   the deprecated argument is actually used.
#' @inheritParams deprecated_generic
#' @returns An [S7 property][new_property].
#' @seealso [deprecated_generic()] and [deprecated_class()] to deprecate
#'   other parts of your API.
#' @export
#' @examples
#' # A property renamed from count to size:
#' Basket := new_class(properties = list(
#'   size = class_double,
#'   deprecated_property("count", new = "size", when = "1.5.0")
#' ))
#'
#' # Using the new name is silent, using the old name warns:
#' basket <- Basket(size = 3)
#' basket@count
deprecated_property <- function(
  old,
  new = NULL,
  when,
  method = c("base", "lifecycle(warn)", "lifecycle(stop)"),
  class = class_any,
  default = NULL
) {
  check_name(old, arg = "old")
  check_when(when)
  method <- check_deprecate_method(method)
  if (!is.null(new)) {
    check_name(new, arg = "new")
  }
  package <- topNamespaceName(parent.frame())
  env <- parent.frame()

  # Property labels aren't function calls, so wrap them in I() to protect
  # them from lifecycle's spec parser
  signal <- function(self, with) {
    deprecate_signal(
      when = when,
      what = I(prop_label(self, old)),
      with = if (!is.null(with)) I(with),
      package = package,
      method = method,
      env = env,
      # `what` embeds the class of `self`, so it isn't a stable lifecycle id
      id = paste(c(package, old), collapse = "::")
    )
  }

  if (is.null(new)) {
    storage <- prop_storage_rename(old)
    getter <- function(self) {
      signal(self, with = NULL)
      attr(self, storage, exact = TRUE)
    }
    setter <- function(self, value) {
      current <- attr(self, storage, exact = TRUE)
      # An unset property is being initialized by the constructor
      if (!is.null(current) && !identical(value, current)) {
        signal(self, with = NULL)
      }
      attr(self, storage) <- value
      self
    }
  } else {
    getter <- function(self) {
      signal(self, with = prop_label(self, new))
      prop(self, new)
    }
    setter <- function(self, value) {
      # No signal when set to the current value of the replacement, which is
      # what the default constructor does when the old argument isn't used.
      if (!identical(value, prop(self, new))) {
        signal(self, with = prop_label(self, new))
        prop(self, new) <- value
      }
      self
    }
    default <- default %||% as.name(new)
  }

  new_property(
    class = class,
    getter = getter,
    setter = setter,
    default = default,
    name = old
  )
}

# The wrapper's closure environment (the execution environment of
# new_deprecated_fun()) holds everything about the deprecation, so
# introspection reads from it rather than from duplicated attributes.
deprecated_target <- function(x) environment(x)$target

# Build the wrapper exported under the old name: it signals the deprecation,
# then evaluates the user's call with the target in functional position, so
# all arguments are passed on lazily and unmodified.
new_deprecated_fun <- function(
  target,
  what,
  with,
  when,
  package,
  method,
  env,
  class
) {
  out <- function(...) {
    call <- sys.call()
    deprecate_signal(
      when = when,
      what = what,
      with = with,
      package = package,
      method = method,
      env = env,
      call = call,
      user_env = parent.frame()
    )
    call[[1L]] <- target
    eval(call, parent.frame())
  }
  # The body ignores its formals, but the target's formals give informative
  # introspection (args(), autocomplete) and identical argument matching errors
  formals(out) <- formals(target)
  class(out) <- c(class, "function")
  out
}

# The pluggable deprecation signal. `what`/`with` are function specs like
# "gen1()"; specs that aren't function calls (property labels) must be
# wrapped in I() by the caller.
deprecate_signal <- function(
  when,
  what,
  with = NULL,
  package = NULL,
  method = "base",
  env = parent.frame(),
  call = NULL,
  user_env = NULL,
  id = NULL
) {
  if (method == "base") {
    # Equivalent to .Deprecated(msg =, old =), but attributes the warning to
    # the user's call rather than to S7 internals
    warning(warningCondition(
      deprecated_message(what, when, with, package),
      old = as.character(what),
      class = "deprecatedWarning",
      call = call
    ))
  } else {
    # `env` (the deprecation site) attributes the deprecation to the right
    # package; `user_env` (the caller of the deprecated code) blames the
    # right user. Find it now: lazy evaluation would walk the frame stack
    # from inside lifecycle.
    user_env <- user_env %||% user_frame()
    switch(
      method,
      "lifecycle(warn)" = lifecycle::deprecate_warn(
        when,
        what,
        with,
        id = id %||% as.character(what),
        env = env,
        user_env = user_env
      ),
      "lifecycle(stop)" = lifecycle::deprecate_stop(
        when,
        what,
        with,
        env = env
      )
    )
  }
  invisible()
}

# The frame of the nearest caller from outside S7. Used to attribute a
# deprecation to the right user when it's signalled from deep inside S7
# machinery (e.g. a deprecated property accessed via `@`).
user_frame <- function() {
  S7_ns <- topenv(environment())
  for (i in rev(seq_len(sys.nframe() - 1L))) {
    fun_env <- environment(sys.function(i))
    if (is.null(fun_env) || !identical(topenv(fun_env), S7_ns)) {
      return(sys.frame(i))
    }
  }
  globalenv()
}

# How to refer to the replacement: qualified with its package, unless it
# lives in the same package as the deprecated alias.
target_label <- function(target_package, target_name, package) {
  if (!is.null(target_package) && !identical(target_package, package)) {
    sprintf("%s::%s()", target_package, target_name)
  } else {
    sprintf("%s()", target_name)
  }
}

check_when <- function(when, call = sys.call(-1L)) {
  if (!is_string(when)) {
    stop2("`when` must be a single string.", call = call)
  }
  version <- tryCatch(numeric_version(when), error = function(e) NULL)
  if (is.null(version)) {
    msg <- sprintf("`when` must be a version number, not \"%s\".", when)
    stop2(msg, call = call)
  }
}

deprecate_methods <- c("base", "lifecycle(warn)", "lifecycle(stop)")

check_deprecate_method <- function(method, call = sys.call(-1L)) {
  if (identical(method, deprecate_methods)) {
    return("base")
  }
  if (!is_string(method) || !method %in% deprecate_methods) {
    msg <- sprintf(
      "`method` must be one of %s.",
      oxford_or(paste0('"', deprecate_methods, '"'))
    )
    stop2(msg, call = call)
  }
  method
}

#' @export
print.S7_deprecated_generic <- function(x, ...) {
  cat("<S7_deprecated_generic> ", deprecated_desc(x), "\n", sep = "")
  invisible(x)
}

#' @export
print.S7_deprecated_class <- function(x, ...) {
  cat("<S7_deprecated_class> ", deprecated_desc(x), "\n", sep = "")
  invisible(x)
}

deprecated_desc <- function(x) {
  env <- environment(x)
  msg <- deprecated_message(env$what, env$when, env$with, env$package)
  gsub("\n", " ", msg, fixed = TRUE)
}
deprecated_message <- function(what, when, with = NULL, package = NULL) {
  msg <- sprintf(
    "`%s` was deprecated in %s %s.",
    what,
    package %||% "version",
    when
  )
  if (!is.null(with)) {
    msg <- paste0(msg, "\n", sprintf("Please use `%s` instead.", with))
  }
  msg
}
