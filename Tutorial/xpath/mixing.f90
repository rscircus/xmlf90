program self
use flib_xpath
!
type(xml_t) :: fxml

integer  :: status
character(len=100)  :: pcdata

call open_xmlfile("inventory_text.xml",fxml,status)
!
do
      call get_node(fxml,path="//item",pcdata=pcdata,status=status)
      if (status < 0)   exit
      !
      print *, "PCDATA retrieved from item element: ", trim(pcdata)

enddo
end program self
