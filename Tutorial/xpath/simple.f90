program simple
use flib_xpath

type(xml_t) :: fxml

integer  :: status
character(len=100)  :: what

call open_xmlfile("inventory.xml",fxml,status)
!
do
      call get_node(fxml,path="//description",pcdata=what,status=status)
      if (status < 0)   exit
      print *, "Appliance: ", trim(what)
enddo
end program simple
