program self
use flib_xpath
!
! Example of re-scanning of an element
!
type(xml_t) :: fxml
type(dictionary_t) :: attributes

integer  :: status
character(len=100)  :: id, currency, price

call open_xmlfile("inventory.xml",fxml,status)
!
do
      call mark_node(fxml,path="//item",status=status)
      if (status < 0)   exit
      !
      ! Pretend we forgot to get the id attribute...
      !
      call get_node(fxml,path=".",attributes=attributes,status=status)
      if (status < 0)   exit
      call get_value(attributes,"id",id,status)
      if (status /= 0) stop "missing id attribute!"
      print *, "Id: ", trim(id)
      !
      ! Now perform a relative search in two stages:
      !  First the attributes...
      !
      call get_node(fxml,path="price",attributes=attributes,status=status)
      if (status < 0)   exit
      call get_value(attributes,"currency",currency,status)
      if (status /= 0) stop "missing currency attribute!"
      print *, "Currency: ", trim(id)

      !  And then the pcdata. Note that "." refers now to the /item/price
      !  element, since fxml has been running through the file and we
      !  have not saved any context to get back to.
      !
      call get_node(fxml,path=".",pcdata=price,status=status)
      if (status /= 0) stop "error in retrieving price data"
      print *, "Price: ", trim(price)
enddo
end program self
