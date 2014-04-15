#
# Makefile for Xpath examples
#
default: all
all: simple euros twoelements item_context  item_context2 self mixing
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
euros:  euros.o
	$(FC) $(LDFLAGS) -o euros euros.o  $(LIBS)
twoelements:  twoelements.o
	$(FC) $(LDFLAGS) -o twoelements twoelements.o  $(LIBS)
item_context:  item_context.o
	$(FC) $(LDFLAGS) -o item_context item_context.o  $(LIBS)
item_context2:  item_context2.o
	$(FC) $(LDFLAGS) -o item_context2 item_context2.o  $(LIBS)
self:  self.o
	$(FC) $(LDFLAGS) -o self self.o  $(LIBS)
mixing:  mixing.o
	$(FC) $(LDFLAGS) -o mixing mixing.o  $(LIBS)
#
clean: 
	rm -f simple euros twoelements item_context item_context2 
	rm -f self mixing *.o *.$(MOD_EXT)
#










