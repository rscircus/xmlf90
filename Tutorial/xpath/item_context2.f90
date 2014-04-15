module m_aux

use flib_xpath
private
public :: get_item_info

CONTAINS

subroutine get_item_info(context,what,price,currency)

type(xml_t), intent(in)       :: context
character(len=*), intent(out) :: what, price, currency

!
! Local variables
!
type(xml_t)        :: ff
integer            :: status
type(dictionary_t) :: attributes

  ! 
  ! context is read-only, so make a copy and sync just in case
  !
  ff = context
  call sync_xmlfile(ff,status)  
  !
  call get_node(ff,path="price", &
                attributes=attributes,pcdata=price,status=status)
  call get_value(attributes,"currency",currency,status)
  if (status /= 0) stop "missing currency attribute!"
  !
  ! Rewind to beginning of context
  !
  ff = context
  call sync_xmlfile(ff,status)  
  !
  call get_node(ff,path="description",pcdata=what,status=status)

end subroutine get_item_info

end module m_aux
!-----------------------------------------------------------------
!-----------------------------------------------------------------
program item_context2
use flib_xpath
use m_aux          ! To access the subroutine

type(xml_t) :: fxml

integer  :: status
character(len=100)  :: what, price, currency

call open_xmlfile("inventory.xml",fxml,status)
!
do
  call mark_node(fxml,path="//item",status=status)
  if (status /= 0)   exit      ! No more items
  call get_item_info(fxml,what,price,currency)
  write(unit=*,fmt="(6a)") "Appliance: ", trim(what), &
                            ". Price: ", trim(price), " ", trim(currency)
  call sync_xmlfile(fxml,status)
enddo
end program item_context2
