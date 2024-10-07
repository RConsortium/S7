#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP collect_dots_skip_missing_(SEXP env, SEXP list_dddExprs_call) {
    // This function is equivalent to `base::list(...)`, except it
    // silently skips missing arguments, and auto-names elements
    // that are unnamed and supplied in the call as a symbol. I.e.,
    // f(a, , b+1) becomes f(a = a, b+1)
    //
    // Implementation note: ideally we could iterate
    // over the DOTSXP list of promises directly, but there is currently
    // no non-"non-API" way to do this. Approved API promise accessors are
    // pending. So, in the interim, we use `base::missing(..i)` to
    // test for missingness, and use `substitute(list(...))` to get the
    // promise expressions.
    //
    // This same C function can be use to add "skip-missing" and "auto-name" to
    // any function that takes dots. E.g.:
    //
    // list2     <- function(...) .Call(collect_dots_skip_missing_, substitute(list(...)))
    // c2        <- function(...) .Call(collect_dots_skip_missing_, substitute(c(...)))
    // pairlist2 <- function(...) .Call(collect_dots_skip_missing_, substitute(pairlist(...)))
    static SEXP missing_call = NULL;
    if (missing_call == NULL) {
        SEXP missing_fun = Rf_eval(Rf_install("missing"), R_BaseEnv);
        missing_call = Rf_lang2(missing_fun, R_NilValue);
        R_PreserveObject(missing_call);
    }

    static char ddi_buf[14] = "..";
    static char *i_buf = ddi_buf + 2;

    PROTECT_INDEX pi;
    PROTECT_WITH_INDEX(R_NilValue, &pi);

    {
    unsigned int i = 1;
    SEXP prev_node = list_dddExprs_call;
    SEXP ddExpr_node = CDR(list_dddExprs_call);
    for (; ddExpr_node != R_NilValue; i++) {
        {
        int ret = snprintf(i_buf, sizeof(ddi_buf) - 2, "%u", i);
        if (ret < 0)
            Rf_error("unknown snprintf error");
        if (ret >= (int)(sizeof(ddi_buf) - 3))
            Rf_error("snprintf truncated output, too many args in `...`");
        ddi_buf[sizeof(ddi_buf) - 1] = '\0'; // just in case
        }

        SEXP ddSym = Rf_install(ddi_buf);

        SETCADR(missing_call, ddSym);
        SEXP is_missing = Rf_eval(missing_call, env);
        REPROTECT(is_missing, pi);

        if (Rf_asLogical(is_missing)) {
            // splice out the node from the exprs list.
            ddExpr_node = CDR(ddExpr_node);
            SETCDR(prev_node, ddExpr_node);
        } else {
            // maybe auto-name if unnamed and expr is a symbol.
            if (TAG(ddExpr_node) == R_NilValue) {
                SEXP val_expr = CAR(ddExpr_node);
                if (TYPEOF(val_expr) == SYMSXP) {
                    SET_TAG(ddExpr_node, val_expr);
                }
            }
            // replace the node expr with `..i`
            SETCAR(ddExpr_node, ddSym);
            // advance to the next node.
            prev_node = ddExpr_node;
            ddExpr_node = CDR(ddExpr_node);
        }
    }
    }

    UNPROTECT(1); // is_missing
    return Rf_eval(list_dddExprs_call, env);
}
