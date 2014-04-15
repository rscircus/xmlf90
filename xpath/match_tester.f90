program m

character(len=100) :: p, t
logical            :: result

do
   write(unit=*,fmt="(a)",advance="no")  "Target path: "
   read(unit=*,fmt="(a)") t
   write(unit=*,fmt="(a)",advance="no")  "Path: "
   read(unit=*,fmt="(a)") p

   result = match(p,t)
   print *, "Result: ", result

enddo


CONTAINS

recursive function match(p,ptarget) result(res_match)
character(len=*), intent(in)  :: p
character(len=*), intent(in)  :: ptarget
logical                       :: res_match

!
! Checks whether a given XML path matches the target path ptarget
! Only absolute paths are considered.
!
! Examples of target paths:
!
!           /pseudo/vps/radfunc      [1]
!           //radfunc/data
!           //data
!           //*/vps/data
!           //job//data      
!           //*
!
integer  :: len_target, len_path, pos_target, pos_path
character(len=100)   :: anchor_leaf

res_match = .false.       

 print *, ":testing: "
 print *, "          ", trim(p)
 print *, " against: ", trim(ptarget)
 print *, "-----------------------------------------"

if (trim(p) == trim(ptarget)) then
   res_match = .true.
   print *, "outright equality"
   return

else if (ptarget == "/") then
   ! We  process // in the middle below

   res_match = .true.
   print *, "target begins by //"
   return

else              ! We get the extreme elements

   len_target = len_trim(ptarget)
   len_path = len_trim(p)
   pos_target = index(ptarget,"/",back=.true.)
   pos_path = index(p,"/",back=.true.)

   print *, " Path leaf: ", p(pos_path+1:len_path) 
   print *, " Target leaf: ", ptarget(pos_target+1:len_target)

   if (pos_target == len_target) then   ! // in the middle...
      ! Get leaf further up
      search_anchor : do 
         print *, "looking for anchor in: ", ptarget(1:len_target-1)
         print *, "press enter"
         read *
         pos_target = index(ptarget(1:len_target-1),"/",back=.true.)
         print *, "pos_target in anchor search: ", pos_target
         if (pos_target == 1) then  ! Target begins by /.// 
            res_match = .true.
            print *, "reached initial /.// in target"
            return
         endif
         anchor_leaf = ptarget(pos_target:len_target-1)
         print *, " Anchor leaf: ", trim(anchor_leaf)
         if (anchor_leaf == "/.") then  ! keep searching
            len_target = pos_target 
            cycle search_anchor
         else
            exit search_anchor
         endif
      enddo search_anchor

      ! Note that the anchor includes the leading /
      ! Now we search for that anchor in the candidate path
      !
      print *, " Searching anchor in : ", trim(p(1:len_path))
      pos_path = index(p(1:len_path),trim(anchor_leaf),back=.true.)
      if (pos_path /= 0) then

         ! Found anchor. Continue further up.
         !
         res_match = match(p(1:pos_path-1),ptarget(1:pos_target-1))
      endif

   else if  (ptarget(pos_target+1:len_target) == ".") then

      ! A dot is a dummy. Continue further up.
      !
      res_match = match(p(1:len_path),ptarget(1:pos_target-1))

   else if  (ptarget(pos_target+1:len_target) == "*") then

      if (len_path == pos_path) then
         print *, "empty element. len_path, pos_path: ", len_path, pos_path
         RETURN   ! empty path element
      endif

      ! A star matches any non-empty leaf. Continue further up.
      !
      res_match = match(p(1:pos_path-1),ptarget(1:pos_target-1))

   else  if (p(pos_path+1:len_path) ==  &
             ptarget(pos_target+1:len_target)) then 

      ! Leafs are equal. Continue further up.
      !
      res_match = match(p(1:pos_path-1),ptarget(1:pos_target-1))

   endif

endif

end function match

end program m













