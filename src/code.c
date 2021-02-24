#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

extern SEXP parent_sym;
extern SEXP name_sym;

SEXP class_names_(SEXP obj) {
  SEXP parent;
  R_xlen_t n = 0;

  parent = obj;
  while(parent != R_NilValue) {
    parent = Rf_getAttrib(parent, parent_sym);
    ++n;
  }

  SEXP out = PROTECT(Rf_allocVector(STRSXP, n));

  R_xlen_t i = 0;
  parent = obj;
  while(parent != R_NilValue) {
    SET_STRING_ELT(out, i, STRING_ELT(Rf_getAttrib(parent, name_sym), 0));
    parent = Rf_getAttrib(parent, parent_sym);
    ++i;
  }

  UNPROTECT(1);

  return out;
}

extern SEXP object_class_sym;

SEXP object_class_(SEXP obj) {
  if (Rf_inherits(obj, "r7_class")) {
    return(obj);
  }

  SEXP val = Rf_getAttrib(obj, object_class_sym);

  if (val != R_NilValue) {
    return val;
  }

  return Rf_getAttrib(obj, R_ClassSymbol);
}

extern SEXP r7_methods_sym;

/* TODO: handle errors when method is not found */
SEXP method_(SEXP generic, SEXP signature) {
  SEXP env = CLOENV(generic);

  SEXP table = Rf_findVarInFrame(env, r7_methods_sym);

  SEXP generic_sym = Rf_install(CHAR(STRING_ELT(Rf_getAttrib(generic, name_sym), 0)));

  table = Rf_findVarInFrame(table, generic_sym);

  R_xlen_t signature_len = Rf_xlength(signature);

  for (R_xlen_t i = 0; i < signature_len; ++i) {
    SEXP parent = VECTOR_ELT(signature, i);

    if (Rf_inherits(parent, "r7_class")) {
      while(parent != R_NilValue) {
        SEXP class = Rf_install(CHAR(STRING_ELT(Rf_getAttrib(parent, name_sym), 0)));
        SEXP val = Rf_findVarInFrame(table, class);
        if (val != R_UnboundValue) {
          table = val;
          break;
        }
        parent = Rf_getAttrib(parent, parent_sym);
      }
    }
    else {
      for (R_xlen_t j = 0; j < Rf_xlength(parent); ++j) {
        SEXP class = Rf_install(CHAR(STRING_ELT(parent, j)));
        SEXP val = Rf_findVarInFrame(table, class);
        if (val != R_UnboundValue) {
          table = val;
          break;
        }
      }
    }
  }
  return table;
}
// stop(sprintf("No methods found for generic '%s' for classes:\n%s", generic, paste0("- ", signature,  collapse = "\n"), call. = FALSE))
