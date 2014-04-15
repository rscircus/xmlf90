subroutine get_item_info(context,what,price,currency)
type(xml_t), intent(in)       :: contex
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
  call sync_xmlfile(ff)  
  !
  call get_node(ff,path="price", &
                attributes=attributes,pcdata=price,status=status)
  call get_value(attributes,"currency",currency,status)
  if (status /= 0) stop "missing currency attribute!"
  !
  ! Rewind to beginning of context
  !
  ff = context
  call sync_xmlfile(ff)  
  !
  call get_node(ff,path="description",pcdata=what,status=status)

end subroutine get_item_info
