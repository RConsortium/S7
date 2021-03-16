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

SEXP method_internal(SEXP table, SEXP signature, R_xlen_t signature_itr, SEXP ignore) {
  if (signature_itr >= Rf_xlength(signature)) {
    return R_NilValue;
  }

  SEXP classes = VECTOR_ELT(signature, signature_itr);

  if (Rf_inherits(classes, "R7_class")) {
    while(classes != R_NilValue) {
      SEXP klass = Rf_install(CHAR(STRING_ELT(Rf_getAttrib(classes, name_sym), 0)));
      SEXP val = Rf_findVarInFrame(table, klass);
      if (TYPEOF(val) == ENVSXP) {
        val = method_internal(val, signature, signature_itr + 1, ignore);
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
        val = method_internal(val, signature, signature_itr + 1, ignore);
      }
      if (TYPEOF(val) == CLOSXP && (ignore == R_NilValue || !should_ignore(val, ignore))) {
        return val;
      }
    }
  }
  return R_NilValue;
}

SEXP get_class(SEXP object, SEXP envir) {
    static SEXP fun = NULL;
    if (fun == NULL) {
      fun = Rf_findVarInFrame(R_BaseEnv, Rf_install("class"));
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

/* TODO: handle errors when method is not found */
SEXP method_(SEXP generic, SEXP signature, SEXP ignore) {
  if (!Rf_inherits(generic, "R7_generic")) {
    return R_NilValue;
  }

  SEXP table = Rf_getAttrib(generic, Rf_install("methods"));

  return method_internal(table, signature, 0, ignore);
}

SEXP R7_object_() {
  return Rf_allocSExp(S4SXP);
}

SEXP method_call_(SEXP call, SEXP generic, SEXP envir) {
  int n_protect = 0;

  // Get the names to dispatch on from the generic signature
  SEXP gen_signature_args = Rf_getAttrib(Rf_getAttrib(generic, Rf_install("signature")), R_NamesSymbol);

  // Every generic signature has `...` as the last arg, which we want to ignore.
  R_xlen_t gen_signature_len = Rf_xlength(gen_signature_args);

  Rboolean has_dots = strcmp(CHAR(STRING_ELT(gen_signature_args, gen_signature_len - 1)), "...") == 0;

  if (has_dots) {
    --gen_signature_len;
  }

  // Allocate a list to store the classes for the arguments
  SEXP signature_classes = PROTECT(Rf_allocVector(VECSXP, gen_signature_len));
  ++n_protect;

  // Allocate a pairlist to hold the argument promises when we do the call to the method
  SEXP args = PROTECT(Rf_cons(R_NilValue, R_NilValue));
  ++n_protect;
  SEXP tail = args;

  // For each of the arguments in the signature
  for (R_xlen_t i = 0; i < gen_signature_len; ++i) {

    // Lookup the promise for that argument in the environment
    SEXP name = Rf_install(CHAR(STRING_ELT(gen_signature_args, i)));
    SEXP arg = Rf_findVar(name, envir);

    // Most of the time this should be a promise
    if (TYPEOF(arg) == PROMSXP) {

      // We first want to duplicate the existing promise
      SEXP new_promise = PROTECT(Rf_duplicate(arg));

      // Then evaluate the original promise so we can lookup its class
      SEXP val = PROTECT(Rf_eval(arg, envir));

      // And set the value of the new promise to that of the evaluated one, so
      // we don't evaluate it twice in the method body.
      SET_PRVALUE(new_promise, val);

      // We can then add our new promise to our argument list
      SETCDR(tail, Rf_cons(new_promise, R_NilValue));

      // We need to call `R7::object_class()`, as not every object has a class
      // attribute, some are created dynamically.
      SEXP klass = PROTECT(object_class_(val, envir));

      // Now that we have the classes for the argument we can add them to the signature classes
      SET_VECTOR_ELT(signature_classes, i, klass);

      UNPROTECT(3);
    }
    // but the bytecode compiler sometimes inlines literals, which we handle
    // here
    else {
      SETCDR(tail, Rf_cons(arg, R_NilValue));
    }

    // Move the pointer forward for the next iteration
    tail = CDR(tail);
  }


  // We only need to add the dots to our arguments if the generic has dots and
  // something was passed in them. Otherwise they are `R_MissingArg` and we
  // don't need to.
  if (has_dots) {
    SEXP dots = Rf_findVar(R_DotsSymbol, envir);
    if (dots != R_MissingArg) {
      SETCDR(tail, dots);
    }
  }

  // The head of args is always R_NilValue, so we just want the tail
  args = CDR(args);

  // Now that we have retrieved all the classes, we can look up what method to call.
  SEXP m = method_(generic, signature_classes, R_NilValue);

  // If no method found, throw an error
  if (m == R_NilValue) {
    Rf_errorcall(R_NilValue, "No method found!");
  }

  // And then actually call it.
  SEXP res = Rf_applyClosure(call, m, args, envir, R_NilValue);

  UNPROTECT(n_protect);

  return res;
}
