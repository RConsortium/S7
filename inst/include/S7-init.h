#ifndef S7_NATIVE_API_INIT_H
#define S7_NATIVE_API_INIT_H

#include "S7.h"

SEXP (*S7_get_class)(SEXP) = NULL;
SEXP (*S7_get_prop)(SEXP, SEXP) = NULL;

void S7_init_api(void) {
  S7_get_class = (SEXP (*)(SEXP)) R_GetCCallable("S7", "S7_get_class");
  S7_get_prop = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("S7", "S7_get_prop");
}

#endif
