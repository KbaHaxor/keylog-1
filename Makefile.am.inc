
STD_C    = gnu89
STD_CXX  = c++98

CHIP     = corei7-avx


#----------------------------------------------------------------------
# Debugging
#----------------------------------------------------------------------
DBG = -g


#----------------------------------------------------------------------
# CPU model
#----------------------------------------------------------------------
CPU = \
	-m64 \
	-march=$(CHIP) \
	-mfpmath=sse \
	-mavx \
	-mvzeroupper

CPUX = \
	-masm=att \
	-mieee-fp \
	-malign-double \
	-m128bit-long-double


#----------------------------------------------------------------------
# Code generation
#----------------------------------------------------------------------
GEN =

GEN2 = \
	-funroll-loops \
	-fomit-frame-pointer \
	-ftree-vectorize \
	-frounding-math \
	-fsignaling-nans

GENX = 	\
	-funsafe-math-optimizations \
	-freg-struct-return \
	-ftree-loop-linear \
	-ftree-loop-im \
	-ftree-loop-ivcanon \
	-fivopts \
	-ftracer


#----------------------------------------------------------------------
# Optimization
#----------------------------------------------------------------------
OPT = # -O3


#----------------------------------------------------------------------
# Warnings
#----------------------------------------------------------------------

TEMPORARY_OVERRIDES = \
	-Wno-logical-op \
	-Wno-maybe-uninitialized \
	-Wno-vla

# c/c++ warnings
#--------------------------

COMMON_WARNINGS = \
	-Wno-system-headers \
	\
	-Waddress \
	-Warray-bounds \
	-Wattributes \
	-Wbuiltin-macro-redefined \
	-Wcast-align \
	-Wchar-subscripts \
	-Wclobbered \
	-Wcomment \
	-Wcoverage-mismatch \
	-Wcpp \
	-Wdeprecated \
	-Wdeprecated-declarations \
	-Wdisabled-optimization \
	-Wdiv-by-zero \
	-Wempty-body \
	-Wenum-compare \
	-Wformat-contains-nul \
	-Wformat-extra-args \
	-Wformat-security \
	-Wformat-y2k \
	-Wformat-zero-length \
	-Wfree-nonheap-object \
	-Wignored-qualifiers \
	-Winit-self \
	-Wint-to-pointer-cast \
	-Winvalid-pch \
	-Wlogical-op \
	-Wno-long-long \
	-Wmain \
	-Wmaybe-uninitialized \
	-Wmissing-braces \
	-Wmissing-declarations \
	-Wmissing-field-initializers \
	-Wmissing-format-attribute \
	-Wmissing-include-dirs \
	-Wmissing-noreturn \
	-Wmultichar \
	-Wnonnull \
	-Wnormalized=nfc \
	-Woverflow \
	-Wpacked \
	-Wpacked-bitfield-compat \
	-Wparentheses \
	-Wpointer-arith \
	-Wpragmas \
	-Wreturn-type \
	-Wsequence-point \
	-Wstrict-aliasing=3 \
	-Wswitch \
	-Wsync-nand \
	-Wtrampolines \
	-Wtrigraphs \
	-Wtype-limits \
	-Wundef \
	-Wuninitialized \
	-Wunknown-pragmas \
	-Wunused-but-set-parameter \
	-Wunused-but-set-variable \
	-Wunused-function \
	-Wunused-label \
	-Wunused-macros \
	-Wunused-parameter \
	-Wunused-result \
	-Wunused-value \
	-Wunused-variable \
	-Wvariadic-macros \
	-Wvector-operation-performance \
	-Wvla \
	-Wvolatile-register-var \
	-Wwrite-strings

# stuff to check into
COMMON_TODO = \
	-Wfloat-equal \
	-Wsign-compare \
	-Wstrict-overflow=4 \
	-Wunsafe-loop-optimizations \
	\
	-Wshadow

# stuff to audit periodically
COMMON_AUDIT = \
	-Waggregate-return \
	-Wcast-qual \
	-Wconversion \
	-Wdouble-promotion \
	-Wformat-nonliteral \
	-Winline \
	-Woverlength-strings \
	-Wpadded \
	-Wredundant-decls \
	-Wsign-conversion \
	-Wstack-protector \
	-Wswitch-default \
	-Wswitch-enum \
	-Wunused-local-typedefs \
	\
	-Wframe-larger-than=32 \
	-Wlarger-than=8 \
	-Wstack-usage=32

# stuff to ignore
COMMON_IGNORE = \
	-Wendif-labels \
	-Wmudflap


# c++-only warnings
#--------------------------

CXX_WARNINGS = \
	-Wc++11-compat \
	-Wconversion-null \
	-Wctor-dtor-privacy \
	-Wdelete-non-virtual-dtor \
	-Winvalid-offsetof \
	-Wnarrowing \
	-Wnoexcept \
	-Wnon-template-friend \
	-Wnon-virtual-dtor \
	-Woverloaded-virtual \
	-Wpmf-conversions \
	-Wreorder \
	-Wstrict-null-sentinel

# stuff to check into
CXX_TODO = \
	-Wabi \
	-Weffc++ \
	-Wold-style-cast \
	-Wsign-promo

# stuff to audit periodically
CXX_AUDIT =

# stuff to ignore
CXX_IGNORE = \
	-Wzero-as-null-pointer-constant


# c-only warnings
#--------------------------

C_WARNINGS = \
	-Wbad-function-cast \
	-Wc++-compat \
	-Wimplicit \
	-Wimplicit-function-declaration \
	-Wimplicit-int \
	-Wjump-misses-init \
	-Wmissing-parameter-type \
	-Wmissing-prototypes \
	-Wnested-externs \
	-Wold-style-declaration \
	-Wold-style-definition \
	-Woverride-init \
	-Wpointer-sign \
	-Wpointer-to-int-cast \
	-Wstrict-prototypes \
	-Wsuggest-attribute=const \
	-Wsuggest-attribute=noreturn \
	-Wsuggest-attribute=pure

# stuff to check into
C_TODO =

# stuff to audit periodically
C_AUDIT =

# stuff to ignore
C_IGNORE = \
	-Wdeclaration-after-statement \
	-Wunsuffixed-float-constants


#----------------------------------------------------------------------
# Automake variables
#----------------------------------------------------------------------

ACLOCAL_AMFLAGS = -I m4

AM_CFLAGS = \
	-std=$(STD_C) \
	$(CPU) \
	$(GEN) \
	$(OPT) \
	$(DBG) \
	$(COMMON_WARNINGS) \
	$(C_WARNINGS) \
	$(TEMPORARY_OVERRIDES)

AM_CXXFLAGS = \
	-std=$(STD_CXX) \
	$(CPU) \
	$(GEN) \
	$(OPT) \
	$(DBG) \
	$(COMMON_WARNINGS) \
	$(CXX_WARNINGS) \
	$(TEMPORARY_OVERRIDES)

