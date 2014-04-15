module m_aux
use flib_sax
private
public :: begin_element_print

contains !---------------- handler subroutine follows

subroutine begin_element_print(name,attributes)
   character(len=*), intent(in)     :: name
   type(dictionary_t), intent(in)   :: attributes
   
   character(len=3)  :: id
   integer           :: status
   
   print *, "Start of element: ", name
   if (has_key(attributes,"id")) then
      call get_value(attributes,"id",id,status)
      print *, "  Id attribute: ", id
   endif
end subroutine begin_element_print

end module m_aux

program simple
use flib_sax
use m_aux

type(xml_t)        :: fxml      ! XML file object (opaque)
integer            :: iostat    ! Return code (0 if OK)

call open_xmlfile("inventory.xml",fxml,iostat)
if (iostat /= 0) stop "cannot open xml file"

call xml_parse(fxml, begin_element_handler=begin_element_print)


end program simple
