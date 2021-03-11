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

/* TODO: handle errors when method is not found */
SEXP method_(SEXP generic, SEXP signature, SEXP ignore) {
  if (!Rf_inherits(generic, "R7_generic")) {
    return R_NilValue;
  }

  SEXP table = Rf_getAttrib(generic, Rf_install("methods"));

  return method_internal(table, signature, 0, ignore);
}

SEXP R7_object_() {
  return Rf_allocSExp(DOTSXP);
}

SEXP method_call_(SEXP generic, SEXP envir) {
  int n_protect = 0;
  static SEXP object_class_fun = NULL;
  if (object_class_fun == NULL) {
    SEXP ns = Rf_findVarInFrame(R_NamespaceRegistry, Rf_install("R7"));
    object_class_fun = Rf_findVarInFrame(ns, Rf_install("object_class"));
  }

  SEXP gen_signature_args = Rf_getAttrib(Rf_getAttrib(generic, Rf_install("signature")), R_NamesSymbol);

  // Every generic signature has `...` as the last arg, which we want to ignore.
  R_xlen_t gen_signature_len = Rf_xlength(gen_signature_args) - 1;

  SEXP signature_classes = PROTECT(Rf_allocVector(VECSXP, gen_signature_len));
  ++n_protect;

  SEXP prom_args = PROTECT(Rf_cons(R_NilValue, R_NilValue));
  ++n_protect;
  SEXP tail = prom_args;

  for (R_xlen_t i = 0; i < gen_signature_len; ++i) {
    SEXP name = Rf_install(CHAR(STRING_ELT(gen_signature_args, i)));
    SEXP arg = Rf_findVar(name, envir);
    if (TYPEOF(arg) == PROMSXP) {
      SEXP new_promise = PROTECT(Rf_duplicate(arg));
      SEXP val = PROTECT(Rf_eval(arg, envir));
      SET_PRVALUE(new_promise, val);
      SEXP object_class_call = PROTECT(Rf_lang2(object_class_fun, val));
      SEXP klass = PROTECT(Rf_eval(object_class_call, envir));
      SET_VECTOR_ELT(signature_classes, i, klass);
      SETCDR(tail, Rf_cons(new_promise, R_NilValue));
      UNPROTECT(4);
    } else {
      SETCDR(tail, Rf_cons(arg, R_NilValue));
    }
    tail = CDR(tail);
  }
  // We only need to add the dots if they exist
  SEXP dots = Rf_findVar(R_DotsSymbol, envir);
  if (dots != R_MissingArg) {
    SETCDR(tail, dots);
  }

  SEXP m = method_(generic, signature_classes, R_NilValue);

  SEXP res = Rf_applyClosure(R_NilValue, m, CDR(prom_args), envir, R_NilValue);
  UNPROTECT(n_protect);

  return res;
}
