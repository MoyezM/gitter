#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* Exported by the vendored grammar (parser_opam.c). */
const void* tree_sitter_opam(void);

CAMLprim value caml_gitter_grammar_opam_language(value unit) {
  CAMLparam1(unit);
  CAMLreturn(caml_copy_nativeint((intnat)tree_sitter_opam()));
}
