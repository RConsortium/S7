% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/super.R
\name{super}
\alias{super}
\title{Force method dispatch to use a superclass}
\usage{
super(from, to)
}
\arguments{
\item{from}{An S7 object to cast.}

\item{to}{An S7 class specification, passed to \code{\link[=as_class]{as_class()}}. Must be a
superclass of \code{object}.}
}
\value{
An \code{S7_super} object which should always be passed
immediately to a generic. It has no other special behavior.
}
\description{
\code{super(from, to)} causes the dispatch for the next generic to use the method
for the superclass \code{to} instead of the actual class of \code{from}. It's needed
when you want to implement a method in terms of the implementation of its
superclass.
\subsection{S3 & S4}{

\code{super()} performs a similar role to \code{\link[=NextMethod]{NextMethod()}} in S3 or
\code{\link[methods:NextMethod]{methods::callNextMethod()}} in S4, but is much more explicit:
\itemize{
\item The super class that \code{super()} will use is known when write \code{super()}
(i.e. statically) as opposed to when the generic is called
(i.e. dynamically).
\item All arguments to the generic are explicit; they are not automatically
passed along.
}

This makes \code{super()} more verbose, but substantially easier to
understand and reason about.
}

\subsection{\code{super()} in S3 generics}{

Note that you can't use \code{super()} in methods for an S3 generic.
For example, imagine that you have made a subclass of "integer":

\if{html}{\out{<div class="sourceCode r">}}\preformatted{myint <- new_class("myint", parent = class_integer)
}\if{html}{\out{</div>}}

Now you go to write a custom print method:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{method(print, myint) <- function(x, ...) \{
   cat("<myint>")
   print(super(x, to = class_integer))
\}

myint(10L)
#> <myint>super(<myint>, <integer>)
}\if{html}{\out{</div>}}

This doesn't work because \code{print()} isn't an S7 generic so doesn't
understand how to interpret the special object that \code{super()} produces.
While you could resolve this problem with \code{\link[=NextMethod]{NextMethod()}} (because S7 is
implemented on top of S3), we instead recommend using \code{\link[=S7_data]{S7_data()}} to extract
the underlying base object:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{method(print, myint) <- function(x, ...) \{
   cat("<myint>")
   print(S7_data(x))
\}

myint(10L)
#> <myint>[1] 10
}\if{html}{\out{</div>}}
}
}
\examples{
foo1 <- new_class("foo1", properties = list(x = class_numeric, y = class_numeric))
foo2 <- new_class("foo2", foo1, properties = list(z = class_numeric))

total <- new_generic("total", "x")
method(total, foo1) <- function(x) x@x + x@y

# This won't work because it'll be stuck in an infinite loop:
method(total, foo2) <- function(x) total(x) + x@z

# We could write
method(total, foo2) <- function(x) x@x + x@y + x@z
# but then we'd need to remember to update it if the implementation
# for total(<foo1>) ever changed.

# So instead we use `super()` to call the method for the parent class:
method(total, foo2) <- function(x) total(super(x, to = foo1)) + x@z
total(foo2(1, 2, 3))

# To see the difference between convert() and super() we need a
# method that calls another generic

bar1 <- new_generic("bar1", "x")
method(bar1, foo1) <- function(x) 1
method(bar1, foo2) <- function(x) 2

bar2 <- new_generic("bar2", "x")
method(bar2, foo1) <- function(x) c(1, bar1(x))
method(bar2, foo2) <- function(x) c(2, bar1(x))

obj <- foo2(1, 2, 3)
bar2(obj)
# convert() affects every generic:
bar2(convert(obj, to = foo1))
# super() only affects the _next_ call to a generic:
bar2(super(obj, to = foo1))
}
