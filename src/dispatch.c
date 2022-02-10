#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

extern SEXP parent_sym;
extern SEXP name_sym;

Rboolean should_ignore(SEXP value, SEXP ignore) {
  for (R_xlen_t i = 0; i < Rf_xlength(ignore); ++i) {
    if (R_compute_identical(value, VECTOR_ELT(ignore, i), 16) == TRUE) {
      return TRUE;
    }
  }
  return FALSE;
}

// Recursively walk through method table to perform iterated dispatch
SEXP method_rec(SEXP table, SEXP signature, R_xlen_t signature_itr, SEXP ignore) {
  if (signature_itr >= Rf_xlength(signature)) {
    return R_NilValue;
  }

  SEXP classes = VECTOR_ELT(signature, signature_itr);

  if (Rf_inherits(classes, "R7_class")) {
    while(classes != R_NilValue) {
      SEXP klass = Rf_install(CHAR(STRING_ELT(Rf_getAttrib(classes, name_sym), 0)));
      SEXP val = Rf_findVarInFrame(table, klass);
      if (TYPEOF(val) == ENVSXP) {
        val = method_rec(val, signature, signature_itr + 1, ignore);
      }
      if (TYPEOF(val) == CLOSXP && (ignore == R_NilValue || !should_ignore(val, ignore))) {
        return val;
      }
      classes = Rf_getAttrib(classes, parent_sym);
    }
  } else {
    for (R_xlen_t i = 0; i < Rf_xlength(classes); ++i) {
      SEXP klass = Rf_install(CHAR(STRING_ELT(classes, i)));
      SEXP val = Rf_findVarInFrame(table, klass);
      if (TYPEOF(val) == ENVSXP) {
        val = method_rec(val, signature, signature_itr + 1, ignore);
      }
      if (TYPEOF(val) == CLOSXP && (ignore == R_NilValue || !should_ignore(val, ignore))) {
        return val;
      }
    }
  }
  return R_NilValue;
}

__attribute__ ((noreturn))
void R7_method_lookup_error(SEXP generic, SEXP signature) {
  static SEXP R7_method_lookup_error_fun = NULL;
  SEXP ns = Rf_findVarInFrame(R_NamespaceRegistry, Rf_install("R7"));

  if (R7_method_lookup_error_fun == NULL) {
    R7_method_lookup_error_fun = Rf_findVarInFrame(ns, Rf_install("method_lookup_error"));
  }
  SEXP name = Rf_getAttrib(generic, Rf_install("name"));
  SEXP args = Rf_getAttrib(generic, Rf_install("dispatch_args"));
  SEXP R7_method_lookup_error_call = PROTECT(Rf_lang4(R7_method_lookup_error_fun, name, args, signature));
  Rf_eval(R7_method_lookup_error_call, ns);

  while(1);
}

SEXP method_(SEXP generic, SEXP signature, SEXP ignore) {
  if (!Rf_inherits(generic, "R7_generic")) {
    return R_NilValue;
  }

  SEXP table = Rf_getAttrib(generic, Rf_install("methods"));

  SEXP m = method_rec(table, signature, 0, ignore);
  if (m == R_NilValue) {
    R7_method_lookup_error(generic, signature);
  }

  return m;
}

SEXP get_class(SEXP object, SEXP envir) {
    static SEXP fun = NULL;
    if (fun == NULL) {
      fun = Rf_findVarInFrame(R_BaseEnv, Rf_install(".class2"));
    }
    SEXP call = PROTECT(Rf_lang2(fun, object));
    SEXP res = Rf_eval(call, envir);
    UNPROTECT(1);
    return res;
}

SEXP object_class_(SEXP object, SEXP envir) {
  if (Rf_inherits(object, "R7_class")) {
    return object;
  }

  if (Rf_inherits(object, "R7_object")) {
    return Rf_getAttrib(object, Rf_install("object_class"));
  }

  SEXP klass = get_class(object, envir);

    // We need to call `methods::extends(class(object))` for S4 objects, so use the S3 klass value we just obtained.
  if (IS_S4_OBJECT(object)) {
    static SEXP methods_extends_fun = NULL;
    if (methods_extends_fun == NULL) {
      SEXP ns = Rf_findVarInFrame(R_NamespaceRegistry, Rf_install("methods"));
      methods_extends_fun = Rf_findVarInFrame(ns, Rf_install("extends"));
    }
    SEXP methods_extends_call = PROTECT(Rf_lang2(methods_extends_fun, klass));
    SEXP res = Rf_eval(methods_extends_call, envir);
    UNPROTECT(1);
    return res;
  }

  return klass;
}

SEXP R7_object_() {
  return Rf_allocSExp(S4SXP);
}

SEXP method_call_(SEXP call, SEXP generic, SEXP envir) {
  int n_protect = 0;

  // Get the number of arguments to the generic
  SEXP formals = FORMALS(generic);
  R_xlen_t n_args = Rf_xlength(formals);
  // And how many are used for dispatch
  SEXP dispatch_args = Rf_getAttrib(generic, Rf_install("dispatch_args"));
  R_xlen_t n_dispatch = Rf_xlength(dispatch_args);

  // Allocate a list to store the classes for the arguments
  SEXP dispatch_classes = PROTECT(Rf_allocVector(VECSXP, n_dispatch));
  ++n_protect;

  // Allocate a pairlist to hold the arguments for when we call the method
  SEXP mcall = PROTECT(Rf_lcons(R_NilValue, R_NilValue));
  ++n_protect;
  SEXP mcall_tail = mcall;

  // For each of the arguments to the generic
  for (R_xlen_t i = 0; i < n_args; ++i) {

    // Find its name and look up its value (a promise)
    SEXP name = TAG(formals);
    SEXP arg = Rf_findVar(name, envir);

    if (i < n_dispatch) {
      if (PRCODE(arg) != R_MissingArg) {
        // Evaluate the original promise so we can look up its class
        SEXP val = Rf_eval(arg, R_EmptyEnv);
        // And update the value of the promise to avoid evaluating it
        // again in the method body
        SET_PRVALUE(arg, val);

        // Then add to arguments of method call
        SETCDR(mcall_tail, Rf_cons(arg, R_NilValue));

        // Determine class string to use for method look up
        SET_VECTOR_ELT(dispatch_classes, i, object_class_(val, envir));
      } else {
        SETCDR(mcall_tail, Rf_cons(name, R_NilValue));
        SET_VECTOR_ELT(dispatch_classes, i, Rf_mkString("MISSING"));
      }
    } else {
      // other arguments not used for dispatch
      SEXP arg_wrap = Rf_cons(name, R_NilValue);
      SET_TAG(arg_wrap, name);
      SETCDR(mcall_tail, arg_wrap);
    }

    mcall_tail = CDR(mcall_tail);
    formals = CDR(formals);
  }

  // Now that we have all the classes, we can look up what method to call
  SEXP m = method_(generic, dispatch_classes, R_NilValue);
  SETCAR(mcall, m);

  // And then call it
  SEXP res = Rf_eval(mcall, envir);

  UNPROTECT(n_protect);
  return res;
}
