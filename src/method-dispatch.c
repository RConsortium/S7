#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

extern SEXP parent_sym;
extern SEXP sym_ANY;
extern SEXP ns_S7;
extern SEXP sym_obj_dispatch;
extern SEXP sym_dispatch_args;
extern SEXP sym_methods;
extern SEXP fn_base_quote;
extern SEXP fn_base_missing;

// extern Rboolean is_S7_object(SEXP);
// extern Rboolean is_s7_class(SEXP);
// extern void check_is_S7(SEXP object);


static
SEXP maybe_enquote(SEXP x) {
  switch (TYPEOF(x)) {
    case SYMSXP:
    case LANGSXP:
      return Rf_lang2(fn_base_quote, x);
    default:
      return x;
  }
}

// Recursively walk through method table to perform iterated dispatch
SEXP method_rec(SEXP table, SEXP signature, R_xlen_t signature_itr) {
  if (signature_itr >= Rf_xlength(signature)) {
    return R_NilValue;
  }

  SEXP classes = VECTOR_ELT(signature, signature_itr);

  for (R_xlen_t i = 0; i < Rf_xlength(classes); ++i) {
    SEXP klass = Rf_install(CHAR(STRING_ELT(classes, i)));
    SEXP val = Rf_findVarInFrame(table, klass);
    if (TYPEOF(val) == ENVSXP) {
      PROTECT(val); // no really necessary, but rchk flags spuriously
      val = method_rec(val, signature, signature_itr + 1);
      UNPROTECT(1);
    }
    if (TYPEOF(val) == CLOSXP) {
      return val;
    }
  }

  // ANY fallback
  SEXP val = Rf_findVarInFrame(table, sym_ANY);
  if (TYPEOF(val) == ENVSXP) {
    PROTECT(val);
    val = method_rec(val, signature, signature_itr + 1);
    UNPROTECT(1);
  }
  if (TYPEOF(val) == CLOSXP) {
    return val;
  }

  return R_NilValue;
}

SEXP generic_args(SEXP generic, SEXP envir) {
  // This function is only used to generate an informative message when
  // signalling an S7_method_lookup_error, so it doesn't need to be maximally efficient.

  // How many arguments are used for dispatch?
  SEXP dispatch_args = Rf_getAttrib(generic, sym_dispatch_args);
  R_xlen_t n_dispatch = Rf_xlength(dispatch_args);

  // Allocate a list to store the arguments
  SEXP args = PROTECT(Rf_allocVector(VECSXP, n_dispatch));

  SEXP missing_call = PROTECT(Rf_lang2(fn_base_missing, R_NilValue));
  PROTECT_INDEX pi;
  PROTECT_WITH_INDEX(R_NilValue, &pi);

  // Find the value of each argument.
  SEXP formals = FORMALS(generic);
  for (R_xlen_t i = 0; i < n_dispatch; ++i) {
    SEXP name = TAG(formals);

    SETCADR(missing_call, name);
    SEXP is_missing = Rf_eval(missing_call, envir);
    REPROTECT(is_missing, pi);

    if (Rf_asLogical(is_missing))  {
      SET_VECTOR_ELT(args, i, R_MissingArg);
    } else {
      // method_call_() has already done the necessary computation
      SET_VECTOR_ELT(args, i, Rf_eval(name, envir));
    }

    formals = CDR(formals);
  }
  Rf_setAttrib(args, R_NamesSymbol, dispatch_args);

  UNPROTECT(3);

  return args;
}

__attribute__ ((noreturn))
void S7_method_lookup_error(SEXP generic, SEXP envir) {

  SEXP name = Rf_getAttrib(generic, R_NameSymbol);
  SEXP args = generic_args(generic, envir);

  SEXP S7_method_lookup_error_call = PROTECT(Rf_lang3(Rf_install("method_lookup_error"), name, args));
  Rf_eval(S7_method_lookup_error_call, ns_S7);

  while(1);
}

SEXP method_(SEXP generic, SEXP signature, SEXP envir, SEXP error_) {
  if (!Rf_inherits(generic, "S7_generic")) {
    return R_NilValue;
  }

  SEXP table = Rf_getAttrib(generic, sym_methods);
  if (TYPEOF(table) != ENVSXP) {
    Rf_error("Corrupt S7_generic: @methods isn't an environment");
  }

  Rboolean error = Rf_asLogical(error_);
  SEXP m = method_rec(table, signature, 0);

  if (error && m == R_NilValue) {
    S7_method_lookup_error(generic, envir);
  }

  return m;
}


SEXP S7_obj_dispatch(SEXP object) {

  SEXP obj_dispatch_call = PROTECT(Rf_lang2(sym_obj_dispatch, maybe_enquote(object)));
  SEXP res = Rf_eval(obj_dispatch_call, ns_S7);
  UNPROTECT(1);

  return res;
}

SEXP S7_object_(void) {
  SEXP obj = PROTECT(Rf_allocSExp(S4SXP));
  Rf_classgets(obj, Rf_mkString("S7_object"));
  UNPROTECT(1);

  return obj;
}

SEXP method_call_(SEXP call_, SEXP op_, SEXP args_, SEXP env_) {
  args_ = CDR(args_);
  SEXP generic = CAR(args_); args_ = CDR(args_);
  SEXP envir = CAR(args_); args_ = CDR(args_);
  int n_protect = 0;

  // Get the number of arguments to the generic
  SEXP formals = FORMALS(generic);
  R_xlen_t n_args = Rf_xlength(formals);
  // And how many are used for dispatch
  SEXP dispatch_args = Rf_getAttrib(generic, sym_dispatch_args);
  R_xlen_t n_dispatch = Rf_xlength(dispatch_args);

  // Allocate a list to store the classes for the arguments
  SEXP dispatch_classes = PROTECT(Rf_allocVector(VECSXP, n_dispatch));
  ++n_protect;

  // Allocate a pairlist to hold the arguments for when we call the method
  SEXP mcall = PROTECT(Rf_lcons(R_NilValue, R_NilValue));
  ++n_protect;
  SEXP mcall_tail = mcall;

  PROTECT_INDEX arg_pi;
  PROTECT_WITH_INDEX(R_NilValue, &arg_pi);
  ++n_protect;

  // For each of the arguments to the generic
  for (R_xlen_t i = 0; i < n_args; ++i) {

    // Find its name and look up its value (a promise)
    SEXP name = TAG(formals);
    SEXP arg = Rf_findVar(name, envir);

    if (i < n_dispatch) {
      // return(arg);
      if (arg == R_MissingArg ||
         (TYPEOF(arg) == PROMSXP && PRCODE(arg) == R_MissingArg)) {

        SETCDR(mcall_tail, Rf_cons(name, R_NilValue));
        SET_VECTOR_ELT(dispatch_classes, i, Rf_mkString("MISSING"));

      } else { // arg not missing

        // Force the promise so we can look up its class.
        // However, we preserve the promise itself so that
        // methods can still call substitute()
        // Instead of Rf_eval(arg, R_EmptyEnv), we do Rf_eval(name, envir), so that
        // - if TYPEOF(arg) == LANGSXP or SYMSXP, arg doesn't need to be enquoted and
        // - if TYPEOF(arg) == PROMSXP, arg is updated in place.
        REPROTECT(arg, arg_pi); // no really necessary, but rchk flags spuriously
        SEXP val = PROTECT(Rf_eval(name, envir));

        if (Rf_inherits(val, "S7_super")) {

          // If it's a superclass,
          // - update the promise with the super() stored value
          // - populate dispatch_classes with super() class dispatch vector
          SEXP true_val = VECTOR_ELT(val, 0);

          // could do arg = true_val always, to avoid non-API usage, but
          // then substitute() in methods would not work.
          if (TYPEOF(arg) == PROMSXP)
            SET_PRVALUE(arg, true_val);
          else
            arg = true_val;

          SETCDR(mcall_tail, Rf_cons(arg, R_NilValue));
          SET_VECTOR_ELT(dispatch_classes, i, VECTOR_ELT(val, 1));

        } else { // not a S7_super, a regular value

          // A PROMSXP arg will have been updated in place by Rf_eval() above.
          // Add to arguments of method call
          SETCDR(mcall_tail, Rf_cons(arg, R_NilValue));

          // Determine class string to use for method look up
          SET_VECTOR_ELT(dispatch_classes, i, S7_obj_dispatch(val));
        }
        UNPROTECT(1);
      }
    } else {
      // other arguments not used for dispatch
      SEXP arg_wrap = Rf_cons(name, R_NilValue);
      if (name != R_DotsSymbol)
        SET_TAG(arg_wrap, name);
      SETCDR(mcall_tail, arg_wrap);
    }

    mcall_tail = CDR(mcall_tail);
    formals = CDR(formals);
  }

  // Now that we have all the classes, we can look up what method to call
  SEXP error_if_not_found = PROTECT(Rf_ScalarLogical(1));
  ++n_protect;
  SEXP m = method_(generic, dispatch_classes, envir, error_if_not_found);
  SETCAR(mcall, m);

  SEXP out = Rf_eval(mcall, envir);
  UNPROTECT(n_protect);
  return out;
}
