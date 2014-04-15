#
# Makefile for SAX exercises
#
default: all
all: simple inventory
#
#---------------------------
MK=$(FLIB_ROOT)/fortran.mk
include $(MK)
#---------------------------
#
# Uncomment the following line for debugging support
#
FFLAGS=$(FFLAGS_DEBUG)
#
LIBS=$(LIB_PREFIX)$(LIB_STD) -lflib
#
simple:  simple.o
	$(FC) $(LDFLAGS) -o simple simple.o  $(LIBS)
inventory:  m_handlers.o inventory.o
	$(FC) $(LDFLAGS) -o inventory m_handlers.o inventory.o  $(LIBS)
#
clean: 
	rm -f simple inventory *.o *.$(MOD_EXT)
#










