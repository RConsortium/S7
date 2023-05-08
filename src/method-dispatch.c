#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

extern SEXP parent_sym;
extern SEXP sym_ANY;

// Recursively walk through method table to perform iterated dispatch
SEXP method_rec(SEXP table, SEXP signature, R_xlen_t signature_itr) {
  if (signature_itr >= Rf_xlength(signature)) {
    return R_NilValue;
  }

  SEXP classes = VECTOR_ELT(signature, signature_itr);

  for (R_xlen_t i = 0; i < Rf_xlength(classes); ++i) {
    SEXP klass = Rf_install(CHAR(STRING_ELT(classes, i)));
    SEXP val = Rf_findVarInFrame(table, klass);
    if (TYPEOF(val) == ENVSXP) {
      val = method_rec(val, signature, signature_itr + 1);
    }
    if (TYPEOF(val) == CLOSXP) {
      return val;
    }
  }

  // ANY fallback
  SEXP val = Rf_findVarInFrame(table, sym_ANY);
  if (TYPEOF(val) == ENVSXP) {
    val = method_rec(val, signature, signature_itr + 1);
  }
  if (TYPEOF(val) == CLOSXP) {
    return val;
  }

  return R_NilValue;
}

SEXP generic_args(SEXP generic, SEXP envir) {
  // How many arguments are used for dispatch?
  SEXP dispatch_args = Rf_getAttrib(generic, Rf_install("dispatch_args"));
  R_xlen_t n_dispatch = Rf_xlength(dispatch_args);

  // Allocate a list to store the arguments
  SEXP args = PROTECT(Rf_allocVector(VECSXP, n_dispatch));

  // Find the value of each argument.
  SEXP formals = FORMALS(generic);
  for (R_xlen_t i = 0; i < n_dispatch; ++i) {
    SEXP name = TAG(formals);
    SEXP arg = Rf_findVar(name, envir);

    if (PRCODE(arg) == R_MissingArg) {
      SET_VECTOR_ELT(args, i, R_MissingArg);
    } else {
      // method_call_() has already done the necessary computation
      SET_VECTOR_ELT(args, i, Rf_eval(arg, R_EmptyEnv));
    }

    formals = CDR(formals);
  }
  Rf_setAttrib(args, R_NamesSymbol, dispatch_args);

  UNPROTECT(1);

  return args;
}

__attribute__ ((noreturn))
void S7_method_lookup_error(SEXP generic, SEXP signature, SEXP envir) {
  SEXP ns = Rf_findVarInFrame(R_NamespaceRegistry, Rf_install("S7"));
  static SEXP S7_method_lookup_error_fun = NULL;
  if (S7_method_lookup_error_fun == NULL) {
    S7_method_lookup_error_fun = Rf_findVarInFrame(ns, Rf_install("method_lookup_error"));
  }

  SEXP name = Rf_getAttrib(generic, R_NameSymbol);
  SEXP args = generic_args(generic, envir);

  SEXP S7_method_lookup_error_call = PROTECT(Rf_lang4(S7_method_lookup_error_fun, name, args, signature));
  Rf_eval(S7_method_lookup_error_call, ns);

  while(1);
}

SEXP method_(SEXP generic, SEXP signature, SEXP envir, SEXP error_) {
  if (!Rf_inherits(generic, "S7_generic")) {
    return R_NilValue;
  }

  SEXP table = Rf_getAttrib(generic, Rf_install("methods"));
  if (TYPEOF(table) != ENVSXP) {
    Rf_error("Corrupt S7_generic: @methods isn't an environment");
  }

  SEXP m = method_rec(table, signature, 0);

  int error = Rf_asInteger(error_);
  if (error && m == R_NilValue) {
    S7_method_lookup_error(generic, signature, envir);
  }

  return m;
}

SEXP S7_obj_dispatch(SEXP object) {
  SEXP ns = Rf_findVarInFrame(R_NamespaceRegistry, Rf_install("S7"));

  static SEXP obj_dispatch_fun = NULL;
  if (obj_dispatch_fun == NULL) {
    obj_dispatch_fun = Rf_findVarInFrame(ns, Rf_install("obj_dispatch"));
  }

  SEXP obj_dispatch_call = PROTECT(Rf_lang2(obj_dispatch_fun, object));
  SEXP res = Rf_eval(obj_dispatch_call, ns);
  UNPROTECT(1);

  return res;
}

SEXP S7_object_(void) {
  SEXP obj = PROTECT(Rf_allocSExp(S4SXP));
  Rf_classgets(obj, Rf_mkString("S7_object"));
  UNPROTECT(1);

  return obj;
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
        SEXP val = PROTECT(Rf_eval(arg, R_EmptyEnv));

        if (!Rf_inherits(val, "S7_super")) {

          // If it's a promise, update the value of the promise to avoid
          // evaluating it again in the method body
          if (TYPEOF(val) == PROMSXP) {
            SET_PRVALUE(arg, val);
          }

          // Then add to arguments of method call
          SETCDR(mcall_tail, Rf_cons(arg, R_NilValue));

          // Determine class string to use for method look up
          SET_VECTOR_ELT(dispatch_classes, i, S7_obj_dispatch(val));
        } else {
          // If it's a superclass, we get the stored value and dispatch class
          SEXP true_val = VECTOR_ELT(val, 0);
          SET_PRVALUE(arg, true_val);
          SETCDR(mcall_tail, Rf_cons(arg, R_NilValue));
          SET_VECTOR_ELT(dispatch_classes, i, VECTOR_ELT(val, 1));
        }
        UNPROTECT(1);
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
  SEXP m = method_(generic, dispatch_classes, envir, Rf_ScalarLogical(1));
  SETCAR(mcall, m);

  UNPROTECT(n_protect);
  return mcall;
}
