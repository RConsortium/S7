#ifndef S7_NATIVE_API_H
#define S7_NATIVE_API_H

#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#ifdef __cplusplus
extern "C" {
#endif

// See help("native-api", package = "S7") for the API contract.
extern SEXP (*S7_get_class)(SEXP object);
extern SEXP (*S7_get_prop)(SEXP object, SEXP name);

void S7_init_api(void);

#ifdef __cplusplus
}
#endif

#endif
