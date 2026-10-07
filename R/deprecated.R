#' Deprecate a generic
#'
#' @description
#' To deprecate a generic, change [new_generic()] to `deprecated_generic()` in
#' its definition and supply `when`. Keep its existing `dispatch_args`, `fun`,
#' and method registrations, and keep exporting it under the same name. Calls
#' still dispatch to its methods, but now warn.
#'
#' If you rename a generic or move it to another package, supply the
#' replacement as `new` instead of `dispatch_args` and `fun`:
#'
#' * Calling the old name warns, then calls the replacement.
#' * Registering a method on the old name registers it on the replacement,
#'   without a warning. Both names use the same methods.
#'
#' @section Moving a generic to another package:
#' If `pkg::gen()` moves to `pkgcore::gen()`, define the old name in `pkg` as:
#'
#' ```r
#' gen := deprecated_generic(new = pkgcore::gen, when = "2.0.0")
#' ```
#'
#' Methods registered through either name apply to calls through both names,
#' including methods supplied by other packages.
#'
#' @section Changing arguments:
#' When `new` is supplied, the deprecated generic has the same arguments as
#' the replacement and forwards them unchanged. Changing or deprecating
#' arguments needs code in the generic itself; this helper only deprecates
#' the generic's name.
#'
#' @param name The old name of the generic, as a string. As with
#'   [new_generic()], the result should be assigned to a variable with this
#'   name, most easily with [:=].
#' @param dispatch_args,fun As in [new_generic()]. Use these to define a
#'   deprecated generic without a replacement. Cannot be supplied with `new`.
#' @param new The replacement: an S7 generic, usually the renamed generic, or
#'   a generic that now lives in another package.
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
#' @param new_label How to name the replacement in the warning, e.g. `"Bar()"`
#'   or `"pkg::Bar()"`. Requires `new`. Defaults to the name recorded when the
#'   replacement was created. Use this when you export it under a different
#'   name. It changes the message, not the object used as the replacement.
#' @returns A function with class `S7_deprecated_generic`.
#' @seealso [deprecated_class()] and [deprecated_property()] to deprecate
#'   other parts of your API.
#' @export
#' @examples
#' # Deprecate a generic by changing its definition:
#' shout := deprecated_generic("x", when = "2.0.0")
#' method(shout, class_character) <- \(x) toupper(x)
#' shout("hi")
#'
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
deprecated_generic <- function(
  name,
  dispatch_args,
  fun = NULL,
  when,
  new = NULL,
  method = c("base", "lifecycle(warn)", "lifecycle(stop)"),
  new_label = NULL
) {
  check_name(name)
  check_when(when)
  method <- check_deprecate_method(method)
  env <- parent.frame()
  package <- topNamespaceName(env)

  if (!is.null(new_label)) {
    check_name(new_label)
    if (is.null(new)) {
      stop2("`new_label` requires `new`.")
    }
  }

  if (is.null(new)) {
    # Construct in the caller's environment so the generic belongs to their
    # package and can receive methods from downstream packages.
    definition <- as.call(list(
      quote(S7::new_generic),
      name = name,
      dispatch_args = dispatch_args,
      fun = fun
    ))
    target <- eval(definition, env)
  } else {
    if (!missing(dispatch_args) || !missing(fun)) {
      stop2("Can't supply `dispatch_args` or `fun` with `new`.")
    }
    if (is_deprecated_generic(new)) {
      new <- deprecated_target(new)
    }
    if (!is_S7_generic(new)) {
      msg <- sprintf("`new` must be an S7 generic, not %s.", obj_desc(new))
      stop2(msg)
    }
    target <- new
    new_label <- new_label %||%
      target_label(package_name(target), target@name, package)
  }

  new_deprecated_fun(
    target = target,
    what = paste0(name, "()"),
    with = new_label,
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
#' To deprecate a class, change [new_class()] to `deprecated_class()` in its
#' definition and supply `when`. Keep its existing parent, properties,
#' constructor, and validator, and keep exporting it under the same name.
#'
#' Calling the constructor warns, then constructs an instance of the
#' deprecated class. Using it in method signatures, `parent`, property
#' classes, or [new_external_class()] references does not warn.
#' Property defaults generated before deprecation can still signal; see
#' "Installed property defaults" below.
#'
#' Supply `new` to recommend another class in the warning. By default, this
#' changes the message only: `Dog()` still creates a `Dog`, even if the
#' warning recommends `Pet()`. Methods for `Dog` remain methods for `Dog`.
#' To register an individual method for both classes, use `Dog | Pet`.
#'
#' Omit `new` to deprecate the class without recommending another.
#'
#' @section Sharing methods with a replacement:
#' If methods for the deprecated class also work for `new`, set
#' `share_methods = TRUE`. Registering or removing a method for `Dog` then
#' has the same effect as using `Dog | Pet` in its signature. Registering
#' directly for `Pet` still affects only `Pet`.
#'
#' This option applies when methods are registered; it does not copy methods
#' already registered for `Dog`. Reinstall downstream packages whose method
#' registrations captured the old class definition. Registrations using
#' [new_external_class()] resolve the class when the package is loaded.
#'
#' The classes keep separate identities. `Dog()` still creates a `Dog`, and
#' `Pet()` does not satisfy a property whose class is `Dog`. Existing objects
#' and subclasses keep their original class. Use this option only when the
#' replacement supports the behavior expected by methods for the old class.
#'
#' @section Installed subclasses:
#' Adding deprecation to an otherwise unchanged class does not require
#' rebuilding packages that define subclasses. Existing instances and
#' subclasses continue matching methods for the deprecated class.
#'
#' The helper does not convert existing or saved objects to `new`,
#' or update installed subclasses if you also change the class definition.
#' Changes to the parent, properties, constructor, or validator need the same
#' compatibility considerations as changes to a non-deprecated class.
#'
#' @section Installed property defaults:
#' A downstream package installed before deprecation can retain a property
#' default that calls the constructor directly. For example:
#'
#' ```r
#' Box := new_class(properties = list(item = upstream::Foo))
#' ```
#'
#' After `upstream` deprecates `Foo`, `Box()` can warn, or error with
#' `method = "lifecycle(stop)"`. Rebuild and reinstall the downstream package
#' against the updated `upstream` to generate a silent default. Defaults
#' generated from [new_external_class()] references stay silent without
#' rebuilding.
#'
#' Explicitly written constructor calls, including calls in a property's
#' `default` or a custom constructor, still signal after rebuilding.
#'
#' @param name The name of the class, as a string. Assign the result to a
#'   variable with this name, most easily with [:=].
#' @param ... Named arguments passed to [new_class()], such as `parent`,
#'   `properties`, `constructor`, and `validator`.
#' @param new An S7 class to recommend in the warning, or `NULL` to
#'   give no recommendation. It does not affect construction. Methods remain
#'   separate unless `share_methods = TRUE`.
#' @param share_methods If `TRUE`, registering or removing a method for the
#'   deprecated class also registers or removes it for `new`. Requires `new`.
#'   Defaults to `FALSE`.
#' @inheritParams deprecated_generic
#' @returns An S7 class with the additional class `S7_deprecated_class`.
#' @seealso [deprecated_generic()] and [deprecated_property()] to deprecate
#'   other parts of your API.
#' @export
#' @examples
#' # Recommend Pet() while keeping Dog() working:
#' Pet := new_class(properties = list(name = class_character))
#' Dog := deprecated_class(
#'   properties = list(name = class_character),
#'   new = Pet,
#'   when = "2.0.0"
#' )
#'
#' Dog(name = "Fido") # warns and creates a Dog
#' Pet(name = "Fido") # creates a Pet without warning
#'
#' # Existing methods and subclasses still use Dog:
#' speak := new_generic("x")
#' method(speak, Dog) <- function(x) "Woof!"
#' Puppy := new_class(parent = Dog)
#' speak(Puppy(name = "Rex"))
#'
#' # A method can support both classes:
#' method(speak, Dog | Pet) <- function(x) x@name
#' speak(Pet(name = "Rex"))
#'
#' # Share registrations when methods also work for the replacement:
#' Hound := deprecated_class(
#'   properties = list(name = class_character),
#'   new = Pet,
#'   when = "3.0.0",
#'   share_methods = TRUE
#' )
#' greet := new_generic("x")
#' method(greet, Hound) <- function(x) paste("Hello", x@name)
#' greet(Pet(name = "Rex"))
#'
#' # A class deprecated without a replacement:
#' Cat := deprecated_class(
#'   properties = list(lives = class_double),
#'   when = "3.0.0"
#' )
#' Cat(lives = 9)
deprecated_class <- function(
  name,
  ...,
  when,
  new = NULL,
  method = c("base", "lifecycle(warn)", "lifecycle(stop)"),
  share_methods = FALSE
) {
  check_name(name)
  check_when(when)
  method <- check_deprecate_method(method)
  if (!isTRUE(share_methods) && !isFALSE(share_methods)) {
    stop2("`share_methods` must be TRUE or FALSE.")
  }
  if (share_methods && is.null(new)) {
    stop2("`share_methods = TRUE` requires `new`.")
  }
  env <- parent.frame()

  if (!is.null(new) && !is_class(new)) {
    msg <- sprintf(
      "`new` must be an S7 class, not %s.",
      obj_desc(new)
    )
    stop2(msg)
  }

  # Define the class in the caller's environment, preserving package names
  # and the scope of property defaults and custom constructors.
  definition <- as.call(c(
    list(quote(S7::new_class), name = name),
    as.list(substitute(list(...)))[-1L]
  ))
  target <- eval(definition, env)
  if (share_methods) {
    attr(target, "S7_method_replacement") <- as_class(new)
    class_ref <- get_class_ref(environment(target))
    class_ref$class <- target
  }
  with <- if (!is.null(new)) {
    target_label(new@package, new@name, target@package)
  }

  out <- new_deprecated_fun(
    target = target,
    what = paste0(name, "()"),
    with = with,
    when = when,
    package = target@package,
    method = method,
    env = env,
    class = "S7_deprecated_class"
  )
  attributes(out) <- attributes(target)
  class(out) <- c("S7_deprecated_class", class(out))
  out
}

is_deprecated_class <- function(x) inherits(x, "S7_deprecated_class")

#' Deprecate a property
#'
#' @description
#' Add `deprecated_property()` to a class's `properties` to warn users when
#' they use an old property name. Supply `new` to forward reads and writes to
#' a replacement property, or omit it to keep storing the property's value.
#'
#' The default `print()` and `str()` methods omit deprecated properties.
#' Direct access and [props()] still read them and signal deprecation.
#'
#' @section When properties warn:
#' Reading a deprecated property signals a warning. Setting it to a different
#' value also warns, but assignments of the current value can be silent.
#'
#' With a replacement, the old constructor argument defaults to the value of
#' the new argument. For example, `Basket(size = 2)` and
#' `Basket(size = 2, count = 2)` are both silent, while
#' `Basket(size = 2, count = 3)` warns. S7 cannot distinguish explicitly
#' supplying the same value from using that default.
#'
#' Without a replacement, construction is silent: S7 initializes the property
#' for every object, whether its value came from an argument or a default.
#'
#' `method = "lifecycle(stop)"` turns the deprecation warnings described above
#' into errors. The cases described as silent remain silent.
#'
#' @section Preserving validation:
#' When deprecating a stored property without a replacement, keep its existing
#' `class`, `default`, and `validator`. For a renamed property, put validation
#' on the replacement. To deprecate a computed property or one with a custom
#' setter, add a deprecation signal to its existing getter and setter instead
#' of using this helper.
#'
#' @section Downstream packages:
#' Downstream packages can store copies of classes, generics, or properties
#' when they are built, instead of looking them up dynamically when they are
#' used. Those packages may not emit deprecation warnings until they are rebuilt
#' against the updated version of your package and reinstalled. Code that looks
#' them up dynamically can start warning as soon as your package is updated.
#'
#' @param old The name of the deprecated property, as a string. Because the
#'   name is part of the property itself, the `properties` list entry doesn't
#'   need to be named.
#' @param new The name of the replacement property, as a string. If `NULL`,
#'   the property is deprecated without a replacement.
#' @param class,default The property `class` and `default`, as in
#'   [new_property()]. When `new` is supplied, `default` defaults to the value
#'   of the replacement property so that construction only signals deprecation
#'   when the deprecated argument differs from it.
#' @param validator A property validator, as in [new_property()]. Only allowed
#'   when `new` is `NULL`. When renaming a property, put the validator on the
#'   replacement instead.
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
  default = NULL,
  validator = NULL
) {
  check_name(old, arg = "old")
  check_when(when)
  method <- check_deprecate_method(method)
  if (!is.null(new)) {
    check_name(new, arg = "new")
    if (!is.null(validator)) {
      stop2(
        "When `new` is supplied, put `validator` on the replacement property."
      )
    }
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
      prop(self, old)
    }
    setter <- function(self, value) {
      current <- attr(self, storage, exact = TRUE)
      # An unset property is being initialized by the constructor
      if (!is.null(current) && !identical(value, current)) {
        signal(self, with = NULL)
      }
      prop(self, old) <- value
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

  out <- new_property(
    class = class,
    getter = getter,
    setter = setter,
    validator = validator,
    default = default,
    name = old
  )
  class(out) <- c("S7_deprecated_property", class(out))
  out
}

is_deprecated_property <- function(x) inherits(x, "S7_deprecated_property")

# The wrapper's closure environment (the execution environment of
# new_deprecated_fun()) holds everything about the deprecation, so
# introspection reads from it rather than from duplicated attributes.
deprecated_target <- function(x) {
  target <- environment(x)$target
  if (is_external_generic(target)) {
    as_generic(resolve_generic(target))
  } else {
    target
  }
}

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
  # Force metadata so promises don't retain the caller's target object.
  what
  with
  when
  package
  method
  env
  class
  args <- formals(target)
  if (is_S7_generic(target)) {
    target_package <- package_name(target)
    if (!is.null(target_package) && !identical(target_package, package)) {
      # Keep a reference to the owning package's binding, not a serialized
      # copy of its method table. The binding must keep the generic's S7 name.
      target <- as_external_generic(target)
    }
  }
  delegate <- function() {
    call <- sys.call(-1L)
    user_env <- parent.frame(2L)
    deprecate_signal(
      when = when,
      what = what,
      with = with,
      package = package,
      method = method,
      env = env,
      call = call,
      user_env = user_env
    )
    call[[1L]] <- deprecated_target(out)
    eval(call, user_env)
  }
  # Keep the target's argument names out of the environment where deprecation
  # state is read. Embed the delegate so even an argument named `delegate`
  # cannot shadow it.
  out <- new_function(args, as.call(list(delegate)), environment())
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

# Find the caller outside S7, its generated constructors, and base evaluation
# helpers (e.g. lapply() in props()). Keep downstream package frames so lifecycle
# can attribute and throttle indirect use normally.
user_frame <- function() {
  S7_ns <- topenv(environment())
  parents <- sys.parents()
  i <- sys.parent()
  while (i > 0L) {
    fun <- sys.function(i)
    if (is_class(fun)) {
      # The underlying constructor retains the marker for generated code.
      fun <- attr(fun, "constructor", exact = TRUE)
    }
    fun_env <- environment(fun)
    ns <- topenv(fun_env)
    if (
      !is_default_constructor(fun) &&
        (is.null(fun_env) ||
          (!identical(ns, S7_ns) &&
            !identical(ns, baseenv()) &&
            !isBaseNamespace(ns)))
    ) {
      return(sys.frame(i))
    }
    # Follow callers, skipping frames whose arguments are being evaluated
    # (e.g. setNames() in props()). C accessors can be their own parent.
    i <- min(i - 1L, parents[[i]])
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
  if (missing(when)) {
    stop2('argument "when" is missing, with no default', call = call)
  }
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
