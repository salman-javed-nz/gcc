/* Definitions of target machine for GNU compiler, for ARM with PE obj format.
   Copyright (C) 1995, 1996, 1999, 2000, 2002, 2003, 2004, 2005, 2007, 2011
   Free Software Foundation, Inc.
   Contributed by Doug Evans (dje@cygnus.com).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Enable PE specific code.  */
#define ARM_PE 1

#define ARM_PE_FLAG_CHAR '@'

/* Ensure that @x. will be stripped from the function name.  */
#undef SUBTARGET_NAME_ENCODING_LENGTHS
#define SUBTARGET_NAME_ENCODING_LENGTHS  \
  case ARM_PE_FLAG_CHAR: return 3;

#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"


/* Run-time Target Specification.  */

/* Get tree.c to declare a target-specific specialization of
   merge_decl_attributes.  */
#define TARGET_DLLIMPORT_DECL_ATTRIBUTES 1

#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "-D__pe__"

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_NOP_FUN_DLLIMPORT)

#undef  MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS \
  { "marm", "mlittle-endian", "mfloat-abi=soft", "mno-thumb-interwork" }

#undef  WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"
#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

/* r11 is fixed.  */
#undef  SUBTARGET_CONDITIONAL_REGISTER_USAGE
#define SUBTARGET_CONDITIONAL_REGISTER_USAGE \
  fixed_regs [11] = 1; \
  call_used_regs [11] = 1;


/* PE/COFF uses explicit import from shared libraries.  */
#define MULTIPLE_SYMBOL_SPACES 1

#define TARGET_ASM_UNIQUE_SECTION arm_pe_unique_section
#define TARGET_ASM_FUNCTION_RODATA_SECTION default_no_function_rodata_section

#define SUPPORTS_ONE_ONLY 1

/* Switch into a generic section.  */
#undef  TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  default_pe_asm_named_section

/* Select attributes for named sections.  */
#define TARGET_SECTION_TYPE_FLAGS  arm_pe_section_type_flags

#define TARGET_ASM_FILE_START_FILE_DIRECTIVE true

/* Output a reference to a label.  */
#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(STREAM, NAME)  \
  asm_fprintf (STREAM, "%U%s", arm_strip_name_encoding (NAME))

/* Write the extra assembler code needed to declare a function
   properly.  If we are generating SDB debugging information, this
   will happen automatically, so we only need to handle other cases.  */
#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL) \
  arm_pe_start_function (FILE, NAME, DECL)

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE,NAME,DECL) \
  arm_pe_end_function (FILE, NAME, DECL)

/* Add an external function to the list of functions to be declared at
   the end of the file.  */
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)                            \
  do                                                                     \
    {                                                                    \
      if (TREE_CODE (DECL) == FUNCTION_DECL)                             \
        arm_pe_record_external_function ((DECL), (NAME));                \
    }                                                                    \
  while (0)

/* Output the label for an initialized variable.  */
#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL)        \
do {                                                       \
  arm_pe_maybe_record_exported_symbol (DECL, NAME, 1);     \
  ASM_OUTPUT_LABEL ((STREAM), (NAME));                     \
} while (0)

/* Support the ctors/dtors and other sections.  */

#define drectve_section() \
  (fprintf (asm_out_file, "\t.section .drectve\n"), \
   in_section = NULL)

/* Flags to mark dllimport/dllexport.  Used by PE ports, but handy to
   have defined always, to avoid ifdefing.  */
#define SYMBOL_FLAG_DLLIMPORT (SYMBOL_FLAG_MACH_DEP << 1)
#define SYMBOL_REF_DLLIMPORT_P(X) \
  ((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_DLLIMPORT) != 0)

#define SYMBOL_FLAG_DLLEXPORT (SYMBOL_FLAG_MACH_DEP << 2)
#define SYMBOL_REF_DLLEXPORT_P(X) \
  ((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_DLLEXPORT) != 0)
