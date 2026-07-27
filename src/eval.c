#include "compat.h"

// Evaluate `expr` in `env` without pushing an R-level evaluation context.
// Unlike base `eval()` (and `eval.parent()`), this does not establish a
// return context, so a function called in `expr` sees `env` as its
// `parent.frame()`. `:=` relies on this so that helpers which capture
// `parent.frame()` (e.g. for deferred cleanup) attach to the caller of `:=`
// rather than a transient evaluation frame.
SEXP S7_eval_bare_(SEXP expr, SEXP env) {
  return Rf_eval(expr, env);
}
