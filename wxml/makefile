#
OBJFILES= m_wxml_buffer.o m_wxml_dictionary.o m_wxml_elstack.o \
          m_wxml_text.o m_wxml_core.o flib_wxml.o
MODFILES=$(OBJFILES:.o=)

#------------------------------
MK=$(FLIB_ROOT)/lib.mk
include $(MK)
#------------------------------

clean:
	rm -f *.o *.$(MOD_EXT)

