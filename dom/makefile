#
OBJFILES= m_dom_debug.o m_dom_error.o \
          m_dom_types.o m_dom_namednodemap.o m_dom_nodelist.o m_dom_node.o \
          m_dom_attribute.o m_dom_document.o m_dom_element.o  \
          m_dom_parse.o m_dom_utils.o flib_dom.o
MODFILES=$(OBJFILES:.o=)

#------------------------------
MK=$(FLIB_ROOT)/lib.mk
include $(MK)
#------------------------------
#
m_dom_document.o:  m_dom_types.o
m_dom_nodelist.o: m_dom_types.o
m_dom_namednodemap.o:  m_dom_types.o
m_dom_node.o:  m_dom_types.o m_dom_nodelist.o m_dom_namednodemap.o \
              m_dom_debug.o m_dom_error.o
m_dom_attribute.o: m_dom_types.o  m_dom_node.o
m_dom_element.o: m_dom_types.o m_dom_nodelist.o m_dom_namednodemap.o \
                 m_dom_attribute.o m_dom_document.o m_dom_debug.o m_dom_node.o
m_dom_parse.o: m_dom_types.o m_dom_element.o m_dom_document.o m_dom_node.o \
               m_dom_debug.o
m_dom_utils.o: m_dom_types.o m_dom_element.o m_dom_document.o m_dom_node.o \
                m_dom_namednodemap.o m_dom_debug.o
#
flib_dom.o:  m_dom_types.o m_dom_nodelist.o m_dom_namednodemap.o \
             m_dom_attribute.o m_dom_document.o m_dom_node.o \
             m_dom_node.o m_dom_element.o m_dom_parse.o m_dom_utils.o
#
clean:
	rm -f *.o *.$(MOD_EXT)

