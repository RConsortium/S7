#include <S7.h>

extern "C" SEXP cpp_class(SEXP object) {
  return S7_get_class(object);
}

extern "C" SEXP cpp_prop(SEXP object, SEXP name) {
  return S7_get_prop(object, name);
}
