#ifndef LLVM_ELF_LOADER_HPP
#define LLVM_ELF_LOADER_HPP

#include <algorithm>
#include <iostream>
#include <iomanip>

#include <llvm/Object/ELFObjectFile.h>

#include "llvm_error_or.hpp"
#include "llvm_loader_utils.hpp"
#include "llvm_primitives.hpp"

#if LLVM_VERSION_MAJOR < 6
#error an unsupported LLVM version
#endif


namespace loader {
namespace elf_loader {

using namespace llvm;
using namespace llvm::object;

// computes the base address of an ELF file.
//
// The base address is either derived as a difference between the
// virtual address of any loadable code segment or, if there are no
// segments or no loadable segments, it is the difference between
// the suggested address of the PROGBITS section with a minimal offset
// and that offset. For object and relocatable files, it is usually
// 0 - 0x34.
//
// Finally, if there are no loadable segments or PROGBIT sections,
// i.e., we don't really have a binary program but something else
// packed as an ELF file, we just return 0.
template <typename T>
uint64_t deduce_base_address(const ELFObjectFile<T> &obj) {
    uint64_t base = 0L;
    auto elf = *obj.getELFFile();
    auto segs = prim::elf_program_headers(elf);
    auto code = segs.end();

    for (auto it = segs.begin(); it != segs.end(); ++it)
        if (it->p_type == ELF::PT_LOAD && (it->p_flags & ELF::PF_X))
            code = it;

    if (code != segs.end()) {
        base = code->p_vaddr - code->p_offset;
    } else {
        auto secs = prim::elf_sections(elf);
        auto first = secs.end();
        auto smallest = std::numeric_limits<uint64_t>::max();
        for (auto it = secs.begin(); it != secs.end(); ++it) {
            if (it->sh_type == ELF::SHT_PROGBITS && it->sh_offset < smallest) {
                first = it;
                smallest = it->sh_offset;
            }
        }

        if (first != secs.end())
            base = first->sh_addr - first->sh_offset;
    }
    return base;
}


template <typename T>
void emit_entry_point(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto hdr = obj.getELFFile()->getHeader();
    s.entry("llvm:entry-point") << hdr->e_entry;
}

std::string name_of_index(std::size_t i) {
    std::ostringstream s;
    s << std::setfill('0') << std::setw(2) << i;
    return s.str();
}

template <typename T>
void emit_program_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto hdrs = prim::elf_program_headers(*obj.getELFFile());
    for (auto it = hdrs.begin(); it != hdrs.end(); ++it) {
        bool ld = (it->p_type == ELF::PT_LOAD);
        bool r = static_cast<bool>(it->p_flags & ELF::PF_R);
        bool w = static_cast<bool>(it->p_flags & ELF::PF_W);
        bool x = static_cast<bool>(it->p_flags & ELF::PF_X);
        auto off = it->p_offset;
        auto filesz = it->p_filesz;
        auto name = name_of_index(it - hdrs.begin());
        s.entry("program-header") << name << off << filesz;
        s.entry("virtual-program-header") << name << it->p_vaddr << it->p_memsz;
        s.entry("program-header-flags") << name << ld << r << w << x;
    }
}

template <typename T>
bool is_external_symbol(const T &sym) {
    return ((sym.getBinding() == ELF::STB_GLOBAL ||
             sym.getBinding() == ELF::STB_WEAK) && sym.st_size == 0);
}

template <typename T>
uint64_t section_offset(const ELFObjectFile<T> &obj, section_iterator it);

template <typename T>
error_or<uint64_t> symbol_file_offset(const ELFObjectFile<T> &obj, const SymbolRef &sym) {
    auto addr = prim::symbol_address(sym);
    auto sect = prim::symbol_section(obj, sym);
    if (auto er = addr || sect) return er;
    uint64_t off = *addr + section_offset(obj, *sect);
    return success(off);
}

template <typename T>
void emit_section(const T &hdr, const std::string &name, ogre_doc &s) {
    s.entry("llvm:section-entry") << name << hdr.sh_addr << hdr.sh_size << hdr.sh_offset;
    bool w = static_cast<bool>(hdr.sh_flags & ELF::SHF_WRITE);
    bool x = static_cast<bool>(hdr.sh_flags & ELF::SHF_EXECINSTR);
    s.entry("llvm:section-flags") << name << true << w << x;
    if (x)
        s.entry("llvm:code-entry") << name << hdr.sh_offset << hdr.sh_size;
}

template <typename T>
void emit_section_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto elf = obj.getELFFile();
    for (auto sec : prim::elf_sections(*elf)) {
        if (auto name = prim::elf_section_name(*elf, &sec))
            emit_section(sec, *name, s);
    }
}

template <typename T>
void emit_symbol_entry(const ELFObjectFile<T> &obj, const SymbolRef &sym, ogre_doc &s) {
    auto sym_elf = obj.getSymbol(sym.getRawDataRefImpl());
    auto name = prim::symbol_name(sym);
    auto addr = prim::symbol_address(sym);
    auto off = symbol_file_offset(obj, sym);
    if (name && addr && off) {
        s.entry("llvm:symbol-entry") << *name
                                     << *addr
                                     << sym_elf->st_size
                                     << *off
                                     << sym_elf->st_value;

        if (sym_elf->getType() == ELF::STT_FUNC)
            s.entry("llvm:code-entry") << *name << *off << sym_elf->st_size ;
    }
}

template <typename T>
void emit_symbol_entries(const ELFObjectFile<T> &obj, symbol_iterator begin, symbol_iterator end, ogre_doc &s) {
    for (auto it = begin; it != end; ++it)
        emit_symbol_entry(obj, *it, s);
}

template <typename T>
void emit_symbol_entries(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto elf = obj.getELFFile();
    emit_symbol_entries(obj, obj.symbol_begin(), obj.symbol_end(), s);
    auto secs = prim::elf_sections(*elf);
    emit_symbol_entries(obj, obj.dynamic_symbol_begin(), obj.dynamic_symbol_end(), s);
}


template <typename T>
uint64_t section_offset(const ELFObjectFile<T> &obj, section_iterator it) {
    if (it == obj.section_end()) return 0; // check for special elf sections
    auto sec_elf = obj.getSection(it->getRawDataRefImpl());
    return sec_elf->sh_offset;
}

template <typename T>
uint64_t section_address(const ELFObjectFile<T> &obj, section_iterator it) {
    if (it == obj.section_end()) return 0; // check for special elf sections
    auto sec_elf = obj.getSection(it->getRawDataRefImpl());
    return sec_elf->sh_addr;
}

template <typename T>
void emit_relocation(const ELFObjectFile<T> &obj, const RelocationRef &rel, section_iterator sec, ogre_doc &s) {
    auto it = rel.getSymbol();
    if (it != prim::end_symbols(obj)) {
        auto sym = *it;
        auto sym_elf = obj.getSymbol(sym.getRawDataRefImpl());
        auto reloc_addr = prim::relocation_offset(rel) + section_address(obj, sec);
        if (is_external_symbol(*sym_elf)) {
            if (auto name = prim::symbol_name(sym))
                s.entry("llvm:name-reference") << reloc_addr << *name;
        } else {
            if (auto addr = prim::symbol_address(sym))
                s.entry("llvm:relocation") << reloc_addr << *addr;
        }
    }
}

template <typename T>
void emit_relocations(const ELFObjectFile<T> &obj, ogre_doc &s) {
    for (auto sec : obj.sections()) {
        if (auto rel_sec = prim::relocated_section(sec)) {
            for (auto rel : sec.relocations()) {
                emit_relocation(obj, rel, *rel_sec, s);
            }
        }
    }
}


} // namespace elf_loader

template <typename T>
error_or<std::string> load(ogre_doc &s, const llvm::object::ELFObjectFile<T> &obj) {
    using namespace elf_loader;
    s.raw_entry("(llvm:file-type elf)");
    s.entry("llvm:default-base-address") << deduce_base_address(obj);
    emit_entry_point(obj, s);
    emit_program_headers(obj, s);
    emit_section_headers(obj, s);
    emit_symbol_entries(obj, s);
    emit_relocations(obj, s);
    return s.str();
}

} // namespace loader

#endif // LLVM_ELF_LOADER_HPP
