program inventory
use flib_sax
use m_handlers

type(xml_t)        :: fxml      ! XML file object (opaque)
integer            :: iostat    

call open_xmlfile("inventory.xml",fxml,iostat)
if (iostat /= 0) stop "cannot open xml file"

call xml_parse(fxml, begin_element_handler=begin_element, &
                     end_element_handler=end_element,     &
                     pcdata_chunk_handler=pcdata_chunk )
                     
end program inventory
