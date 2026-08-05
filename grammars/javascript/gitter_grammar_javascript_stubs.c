#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* Exported by the vendored grammar (parser_javascript.c). */
const void* tree_sitter_javascript(void);

CAMLprim value caml_gitter_grammar_javascript_language(value unit) {
  CAMLparam1(unit);
  CAMLreturn(caml_copy_nativeint((intnat)tree_sitter_javascript()));
}
