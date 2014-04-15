module m_handlers
use flib_sax
private
public :: begin_element, end_element, pcdata_chunk
!
logical, private            :: in_item, in_description, in_price
character(len=40), private  :: what, price, currency, id
!
contains !-----------------------------------------
!
subroutine begin_element(name,attributes)
   character(len=*), intent(in)     :: name
   type(dictionary_t), intent(in)   :: attributes
   
   integer  :: status
   
   select case(name)
     case("item")
       in_item = .true.
       call get_value(attributes,"id",id,status)
     
     case("description")
       in_description = .true.
       
     case("price")
       in_price = .true.
       call get_value(attributes,"currency",currency,status)

   end select
   
end subroutine begin_element
!---------------------------------------------------------------
subroutine pcdata_chunk(chunk)
   character(len=*), intent(in) :: chunk

   if (in_description) what = chunk
   if (in_price) price = chunk

end subroutine pcdata_chunk
!---------------------------------------------------------------
subroutine end_element(name)
   character(len=*), intent(in)     :: name
   
   select case(name)
     case("item")
       in_item = .false.
       write(unit=*,fmt="(5(a,tr1))") trim(id), trim(what), ":", &
                                     trim(price), trim(currency)
     
     case("description")
       in_description = .false.
       
     case("price")
       in_price = .false.

   end select
   
end subroutine end_element
!---------------------------------------------------------------
end module m_handlers
