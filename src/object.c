#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP collect_dots_skip_missing_(SEXP env, SEXP list_dddExprs_call) {
    // This function is equivalent to `base::list(...)`, except it
    // silently skips missing arguments. Ideally we could iterate
    // over the DOTSXP list of promises directly, but there is currently
    // no non-"non-API" way to do this. So we use `base::missing(..i)` to
    // test for missingness, and use `substitute(list(...))` to get the
    // promise expressions.
    static SEXP missing_call = NULL;
    if (missing_call == NULL) {
        SEXP missing_fun = Rf_eval(Rf_install("missing"), R_BaseEnv);
        missing_call = Rf_lang2(missing_fun, R_NilValue);
        R_PreserveObject(missing_call);
    }
    // 14 = 2 for ".." + up to 10 digit number + '\0' + 1 extra for safety
    static char ddi_buf[14] = "..";
    static char *i_buf = ddi_buf + 2;
    ddi_buf[13] = '\0'; // Technically not necessary, but just to be safe

    PROTECT_INDEX pi;
    PROTECT_WITH_INDEX(R_NilValue, &pi);

    {
    unsigned int i = 1;
    SEXP prev_node = list_dddExprs_call;
    SEXP ddExpr_node = CDR(list_dddExprs_call);
    for (; ddExpr_node != R_NilValue; i++) {
        snprintf(i_buf, sizeof(ddi_buf) - 2, "%u", i);
        SEXP ddSym = Rf_install(ddi_buf);
        SETCADR(missing_call, ddSym);
        SEXP is_missing = Rf_eval(missing_call, env);
        REPROTECT(is_missing, pi);

        if (Rf_asLogical(is_missing)) {
            ddExpr_node = CDR(ddExpr_node);
            SETCDR(prev_node, ddExpr_node);
        } else {
            if (TAG(ddExpr_node) == R_NilValue) {
                SEXP val_expr = CAR(ddExpr_node);
                if (TYPEOF(val_expr) == SYMSXP) {
                    SET_TAG(ddExpr_node, val_expr);
                }
            }
            SETCAR(ddExpr_node, ddSym);
            prev_node = ddExpr_node;
            ddExpr_node = CDR(ddExpr_node);
        }
    }
    }

    UNPROTECT(1); // is_missing
    return Rf_eval(list_dddExprs_call, env);
}