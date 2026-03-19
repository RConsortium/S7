#ifndef S7_COMPAT_H
#define S7_COMPAT_H

#if (R_VERSION >= R_Version(4, 5, 0))
static inline
SEXP s7_get_var_in_frame(SEXP env, SEXP sym, SEXP ifnotfound) {
  return R_getVarEx(sym, env, FALSE, ifnotfound);
}
#else
static inline
SEXP s7_get_var_in_frame(SEXP env, SEXP sym, SEXP ifnotfound) {
  SEXP val = Rf_findVarInFrame(env, sym);
  return val == R_UnboundValue ? ifnotfound : val;
}
#endif

#endif
