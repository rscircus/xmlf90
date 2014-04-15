program euros
use flib_xpath
!
type(xml_t) :: fxml

integer  :: status
character(len=100)  :: price

!call enable_debug(sax=.false.)

call open_xmlfile("inventory.xml",fxml,status)
!
do
  call get_node(fxml,path="//price", &
                att_name="currency",att_value="euro", &
                pcdata=price,status=status)
  if (status < 0)   exit
  print *, "Price (euro): ", trim(price)
enddo
end program euros

