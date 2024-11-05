#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

extern SEXP parent_sym;
extern SEXP sym_ANY;
extern SEXP ns_S7;
extern SEXP sym_obj_dispatch;
extern SEXP sym_dispatch_args;
extern SEXP sym_methods;
extern SEXP sym_S7_dispatch;
extern SEXP sym_name;

extern SEXP fn_base_quote;
extern SEXP fn_base_missing;

extern SEXP R_TRUE;


static inline
void APPEND_NODE(SEXP node, SEXP tag, SEXP val) {
  SEXP new_node = Rf_cons(val, R_NilValue);
  SETCDR(node, new_node);
  SET_TAG(new_node, tag);
}

// extern Rboolean is_S7_object(SEXP);
// extern Rboolean is_s7_class(SEXP);
// extern void check_is_S7(SEXP object);


static inline
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

  SEXP m = method_rec(table, signature, 0);

  if (m == R_NilValue && Rf_asLogical(error_)) {
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

  // Get the number of arguments to the generic
  SEXP formals = FORMALS(generic);
  R_xlen_t n_args = Rf_xlength(formals);
  // And how many are used for dispatch
  SEXP dispatch_args = Rf_getAttrib(generic, sym_dispatch_args);
  R_xlen_t n_dispatch = Rf_xlength(dispatch_args);

  // Allocate a list to store the classes for the arguments
  SEXP dispatch_classes = PROTECT(Rf_allocVector(VECSXP, n_dispatch));

  // Allocate a pairlist to hold the arguments for when we call the method
  SEXP mcall = PROTECT(Rf_lcons(R_NilValue, R_NilValue));
  SEXP mcall_tail = mcall;

  PROTECT_INDEX arg_pi, val_pi;
  PROTECT_WITH_INDEX(R_NilValue, &arg_pi); // unnecessary, for rchk only
  PROTECT_WITH_INDEX(R_NilValue, &val_pi); // unnecessary, for rchk only

  // For each of the arguments to the generic
  for (R_xlen_t i = 0; i < n_args; ++i) {

    SEXP name = TAG(formals);

    if (i < n_dispatch) {

      SEXP arg = Rf_findVarInFrame(envir, name);
      if (arg == R_MissingArg) {

        APPEND_NODE(mcall_tail, name, arg);
        SET_VECTOR_ELT(dispatch_classes, i, Rf_mkString("MISSING"));

      } else { // arg not missing, is a PROMSXP

        // Force the promise so we can look up its class.
        // However, we preserve and pass along the promise itself so that
        // methods can still call substitute()
        // Instead of Rf_eval(arg, R_EmptyEnv), we do Rf_eval(name, envir), so that
        // - if TYPEOF(arg) == LANGSXP or SYMSXP, arg doesn't need to be enquoted and
        // - if TYPEOF(arg) == PROMSXP, arg is updated in place.
        REPROTECT(arg, arg_pi); // unnecessary, for rchk only
        SEXP val = Rf_eval(name, envir);
        REPROTECT(val, val_pi); // unnecessary, for rchk only

        if (Rf_inherits(val, "S7_super")) {


          // Put the super() stored value into the method call.
          // Note: This means we don't pass along the arg PROMSXP, meaning that
          // substitute() in methods does not retrieve the `super()` call.
          // If we wanted substitute() to work here too, we could do:
          //   if (TYPEOF(arg) == PROMSXP) { SET_PRVALUE(arg, true_val); } else { arg = true_val; }
          SEXP arg = VECTOR_ELT(val, 0); // true_val used for dispatch
          APPEND_NODE(mcall_tail, name, arg);

          // Put the super() stored class dispatch vector into dispatch_classes
          SET_VECTOR_ELT(dispatch_classes, i, VECTOR_ELT(val, 1));

        } else { // val is not a S7_super, a regular value

          // The PROMSXP arg will have been updated in place by Rf_eval() above.
          // Add to arguments of method call
          APPEND_NODE(mcall_tail, name, arg);

          // Determine class string to use for method look up
          SET_VECTOR_ELT(dispatch_classes, i, S7_obj_dispatch(val));
        }
      }
    } else {
      // other arguments not used for dispatch
      if (name == R_DotsSymbol) {
        SETCDR(mcall_tail, Rf_cons(R_DotsSymbol, R_NilValue));
      } else {
        // pass along the promise so substitute() works
        SEXP arg = Rf_findVarInFrame(envir, name);
        APPEND_NODE(mcall_tail, name, arg);
      }
    }

    mcall_tail = CDR(mcall_tail);
    formals = CDR(formals);
  }

  // Now that we have all the classes, we can look up what method to call
  SEXP m = method_(generic, dispatch_classes, envir, R_TRUE);
  REPROTECT(m, val_pi); // unnecessary, for rchk only

  /// Inlining the method closure in the call like `SETCAR(mcall, m);`
  /// leads to extremely verbose (unreadable) traceback()s. So,
  /// for nicer tracebacks, we set a SYMSXP at the head.
  SEXP method_name = Rf_getAttrib(m, sym_name);
  if (TYPEOF(method_name) != SYMSXP) {
    // if name is missing, fallback to masking the `S7_dispatch` symbol.
    // we could alternatively fallback to inlining m: SETCAR(mcall, m)
    method_name = sym_S7_dispatch;
  }

  Rf_defineVar(method_name, m, envir);
  SETCAR(mcall, method_name);

  SEXP out = Rf_eval(mcall, envir);
  UNPROTECT(4);
  return out;
}
