#include "compat.h"

static SEXP type_NULL;
static SEXP type_missing;
static SEXP type_any;
static SEXP type_S7_base;
static SEXP type_S7;
static SEXP type_S7_union;
static SEXP type_S7_S3;
static SEXP type_S7_external;
static SEXP type_S4;

static SEXP class_missing;
static SEXP class_any;
static SEXP class_S7_base;
static SEXP class_S7;
static SEXP class_S7_union;
static SEXP class_S7_S3;
static SEXP class_S7_external;
static SEXP class_S4;

static SEXP preserve_string(const char* x) {
  SEXP out = Rf_mkString(x);
  R_PreserveObject(out);
  return out;
}

void class_type_init(void) {
  type_NULL = preserve_string("NULL");
  type_missing = preserve_string("missing");
  type_any = preserve_string("any");
  type_S7_base = preserve_string("S7_base");
  type_S7 = preserve_string("S7");
  type_S7_union = preserve_string("S7_union");
  type_S7_S3 = preserve_string("S7_S3");
  type_S7_external = preserve_string("S7_external");
  type_S4 = preserve_string("S4");

  class_missing = PRINTNAME(Rf_install("S7_missing"));
  class_any = PRINTNAME(Rf_install("S7_any"));
  class_S7_base = PRINTNAME(Rf_install("S7_base_class"));
  class_S7 = PRINTNAME(Rf_install("S7_class"));
  class_S7_union = PRINTNAME(Rf_install("S7_union"));
  class_S7_S3 = PRINTNAME(Rf_install("S7_S3_class"));
  class_S7_external = PRINTNAME(Rf_install("S7_external_class"));
  class_S4 = PRINTNAME(Rf_install("classRepresentation"));
}

SEXP class_type_(SEXP x) {
  if (x == R_NilValue)
    return type_NULL;

  SEXP classes = Rf_getAttrib(x, R_ClassSymbol);
  int type = 9;

  if (TYPEOF(classes) == STRSXP) {
    for (R_xlen_t i = 0; i < Rf_xlength(classes); ++i) {
      SEXP cls = STRING_ELT(classes, i);

      if (type > 1 && cls == class_missing)
        type = 1;
      else if (type > 2 && cls == class_any)
        type = 2;
      else if (type > 3 && cls == class_S7_base)
        type = 3;
      else if (type > 4 && cls == class_S7)
        type = 4;
      else if (type > 5 && cls == class_S7_union)
        type = 5;
      else if (type > 6 && cls == class_S7_S3)
        type = 6;
      else if (type > 7 && cls == class_S7_external)
        type = 7;
      else if (type > 8 && cls == class_S4)
        type = 8;
    }
  }

  switch (type) {
  case 1: return type_missing;
  case 2: return type_any;
  case 3: return type_S7_base;
  case 4: return type_S7;
  case 5: return type_S7_union;
  case 6: return type_S7_S3;
  case 7: return type_S7_external;
  case 8: return type_S4;
  }

  Rf_error("`x` is not a standard S7 class.");
}
