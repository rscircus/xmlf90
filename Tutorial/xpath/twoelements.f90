program twoelements
use flib_xpath

type(xml_t) :: fxml

integer  :: status
character(len=100)  :: what, price, currency
type(dictionary_t)  :: attributes

call open_xmlfile("inventory.xml",fxml,status)
!
do
  call get_node(fxml,path="//description", &
                pcdata=what,status=status)
  if (status < 0)   exit                   ! No more items
  !
  ! Price comes right after description...
  !
  call get_node(fxml,path="//price", &
                attributes=attributes,pcdata=price,status=status)
  if (status /= 0) stop "missing price element!"
  
  call get_value(attributes,"currency",currency,status)
  if (status /= 0) stop "missing currency attribute!"
  
  write(unit=*,fmt="(6a)") "Appliance: ", trim(what), &
                           ". Price: ", trim(price), " ", trim(currency)
enddo
end program twoelements
