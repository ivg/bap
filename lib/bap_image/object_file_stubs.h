#include "caml/mlvalues.h"

#ifdef __cplusplus 
extern "C" {
#endif

    CAMLprim value object_file_create_stub(value);
    CAMLprim value object_file_get_arch_stub(value);
    CAMLprim value object_file_symbols_stub(value);
    CAMLprim value object_file_dynamic_symbols_stub(value);
    CAMLprim value object_file_sections_stub(value);

#ifdef __cplusplus 
}
#endif
