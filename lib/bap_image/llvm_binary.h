#ifndef BAP_LLVM_BINARY_H
#define BAP_LLVM_BINARY_H

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>

#include <inttypes.h>
#include <stdbool.h>

typedef struct caml_ba_array caml_ba_array;
typedef struct image image;
typedef struct section section;
typedef struct symbol symbol;
typedef struct segment segment;
typedef struct kind kind;

image* from_value(value);

value to_value_img(image*);
value to_value_sec(const section*);
value to_value_seg(const segment*);
value to_value_sym(const symbol*);
value to_value_arc(const char*);

const section* c_sections(image*);
const segment* c_segments(image*);
const symbol* c_symbols(image*);
const char* c_arch(image*);
uint64_t c_entry(image*);

value llvm_binary_create_stub(value);
value llvm_binary_arch_stub(value);
value llvm_binary_entry_stub(value);
value llvm_binary_segments_stub(value);
value llvm_binary_symbols_stub(value);
value llvm_binary_sections_stub(value);


#endif // BAP_LLVM_BINATY_H
