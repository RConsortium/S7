#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>


SEXP class_names_(SEXP obj) {
  SEXP parent;
  R_xlen_t n = 0;
  SEXP parent_sym = Rf_install("parent");
  SEXP name_sym = Rf_install("name");

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

#define BUF_SIZE 100000

SEXP construct_signature_(SEXP signature) {
  static char ans[BUF_SIZE];
  R_xlen_t n = 1;

  R_xlen_t signature_len = Rf_xlength(signature);

  for (R_xlen_t i = 0; i < signature_len; ++i) {
    n *= Rf_xlength(VECTOR_ELT(signature, i));
  }

  SEXP out = PROTECT(Rf_allocVector(STRSXP, n));

  R_xlen_t cum_n = 1;
  for (R_xlen_t i = 0; i < signature_len; ++i) {
    SEXP col_data = VECTOR_ELT(signature, i);
    R_xlen_t len = Rf_xlength(col_data);
    cum_n *= len;

    R_xlen_t each = n / cum_n;
    R_xlen_t times = n / each / len;

    R_xlen_t out_itr = 0;
    /*REprintf("i: %i each: %i times: %i\n", i, each, times);*/

    for(R_xlen_t time = 0; time < times; ++time) {
      for (R_xlen_t row = 0; row < len; ++row) {
        SEXP row_data = STRING_ELT(col_data, row);
        for(R_xlen_t rep = 0; rep < each; ++rep) {
          strcpy(ans, CHAR(STRING_ELT(out, out_itr)));
          if (i > 0) {
            strcat(ans, "-");
          }
          strcat(ans, CHAR(row_data));
          SET_STRING_ELT(out, out_itr++, Rf_mkChar(ans));
        }
      }
    }
  }

  UNPROTECT(1);
  return out;
}

SEXP get_r7_method_(SEXP generic, SEXP signatures, SEXP envir) {
  static char ans[BUF_SIZE];

  SEXP s3_table_sym = Rf_install(".__S3MethodsTable__.");
  SEXP table = Rf_findVar(s3_table_sym, envir);
  if (table == R_UnboundValue) {
    return R_NilValue;
  }

  /* We need to evaluate the table if it is a promise */
  if (TYPEOF(table) == PROMSXP) {
    PROTECT(table);
    table = Rf_eval(table, envir);
    UNPROTECT(1);
  }

  R_xlen_t n = Rf_xlength(signatures);

  const char* generic_str = CHAR(STRING_ELT(generic, 0));
  for (R_xlen_t i = 0; i < n; ++i) {
    strcpy(ans, generic_str);
    strcat(ans, ".");
    strcat(ans, CHAR(STRING_ELT(signatures, i)));
    SEXP method_sym = Rf_install(ans);

    SEXP out = Rf_findVar(method_sym, table);
    if (out != R_UnboundValue) {
      return out;
    }
  }

  Rf_errorcall(R_NilValue, "No methods found for generic '%s'", generic_str);

  return R_NilValue;
}

// stop(sprintf("No methods found for generic '%s' for classes:\n%s", generic, paste0("- ", signature,  collapse = "\n"), call. = FALSE))
