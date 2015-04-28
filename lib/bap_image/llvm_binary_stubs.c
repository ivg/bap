
#include "llvm_binary.h"

struct segment {
    char* name_;
    uint64_t offset_;
    uint64_t addr_;
    uint64_t size_;
    bool is_readable_;
    bool is_writable_;
    bool is_executable_;
};

struct section {
    char* name_;
    uint64_t addr_;
    uint64_t size_;
};

struct symbol {
    char* name_;
    kind* kind_;
    uint64_t addr_;
    uint64_t size_;
  
};

image* c_create(const char*, size_t);

image* from_value(value v) {return *(image**) Data_custom_val(v);}

void finalize(value v) {CAMLparam1(v); free (from_value(v)); CAMLreturn0;}

value to_value_arc(const char* m) {
    CAMLparam0();
    CAMLlocal1(v);
    CAMLreturn(caml_copy_string(m));
}

value to_value_img(image* m) {
    CAMLparam0();
    CAMLlocal1(v);
    static struct custom_operations binary_ops = {
        "llvm.caml.llvm_image",
        finalize,
        custom_compare_default,
        custom_hash_default,
        custom_serialize_default,
        custom_deserialize_default
    };
    v = alloc_custom(&binary_ops, sizeof(image*), 0, 1);
    *((image**)(Data_custom_val(v))) = m;
    CAMLreturn(v);
}

value to_value_seg(const segment* s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(7, 0);  
    Store_field (result, 0, caml_copy_string(s->name_));
    Store_field (result, 1, caml_copy_int64(s->offset_));
    Store_field (result, 2, caml_copy_int64(s->addr_));
    Store_field (result, 3, caml_copy_int64(s->size_));
    Store_field (result, 4, Val_bool(s->is_readable_));
    Store_field (result, 5, Val_bool(s->is_writable_));
    Store_field (result, 6, Val_bool(s->is_executable_));
    CAMLreturn(result);
}

value to_value_sec(const section* s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(3, 0);  
    Store_field (result, 0, caml_copy_string(s->name_));
    Store_field (result, 1, caml_copy_int64(s->addr_));
    Store_field (result, 2, caml_copy_int64(s->size_));
    CAMLreturn(result);
}

value to_value_sym(const symbol* s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(4, 0);  
    Store_field (result, 0, caml_copy_string(s->name_));
    Store_field (result, 1, Val_int(s->kind_));
    Store_field (result, 2, caml_copy_int64(s->addr_));
    Store_field (result, 3, caml_copy_int64(s->size_));
    CAMLreturn(result);

}

CAMLprim value llvm_binary_create_stub(value arg) {
    CAMLparam1(arg);
    const caml_ba_array* array = Caml_ba_array_val(arg);
    if (array->num_dims != 1)
        caml_invalid_argument("invalid bigarray dimension");
    image* obj =
        c_create((const char*)(array->data), array->dim[0]);
    CAMLreturn(to_value_img(obj));
}

CAMLprim value llvm_binary_arch_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(to_value_arc(c_arch(from_value(arg))));
}

CAMLprim value llvm_binary_entry_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(caml_copy_int64(c_entry(from_value(arg))));
}

CAMLprim value llvm_binary_segments_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(to_value_seg(c_segments(from_value(arg))));
}

CAMLprim value llvm_binary_symbols_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(to_value_sym(c_symbols(from_value(arg))));
}

CAMLprim value llvm_binary_sections_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(to_value_sec(c_sections(from_value(arg))));
}
