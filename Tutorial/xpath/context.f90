program item_context
use flib_xpath
type(xml_t) :: fxml, context
integer :: status
character(len=100) :: what, price, currency
type(dictionary_t) :: attributes

call open_xmlfile("inventory.xml",fxml,status)
!
do
   call mark_node(fxml,path="//item",status=status)
   if (status < 0) exit ! No more items
   context = fxml ! Save item context
!
! Search relative to context
!
   call get_node(fxml,path="price", &
        attributes=attributes,pcdata=price,status=status)
   call get_value(attributes,"currency",currency,status)
   if (status /= 0) stop "missing currency attribute!"
!
! Rewind to beginning of context

   call sync_to_context(fxml,context)
!
! Search relative to context
!
   call get_node(fxml,path="description",pcdata=what,status=status)
   write(unit=*,fmt="(6a))") "Appliance: ", trim(what), &
        ". Price: ", trim(price), " ", trim(currency)
enddo
end program item_context







