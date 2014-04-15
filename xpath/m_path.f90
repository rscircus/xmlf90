module m_path
!
! XPATH-like API for XML Parsing
! Copyright Alberto Garcia <wdpgaara@lg.ehu.es>, August 2003
!
use flib_sax

private
!
public :: get_node, mark_node, enable_debug, disable_debug

private :: match, process_node, get_path
private :: begin_element, end_element, pcdata_handler, empty_element
private :: pause_parsing

!
integer, private, save                   :: global_status
integer, public, parameter               :: END_OF_FILE  = -1
integer, public, parameter               :: END_OF_ANCESTOR_ELEMENT = -2
integer, public, parameter               :: PCDATA_OVERFLOW = 7

logical, private, save                   :: debug_xpath = .false.
logical, private, save                   :: debug_sax = .false.
character(len=500), private, save        :: path_required
character(len=100), private, save        :: target_path      ! *** Hard limit


logical, private, save                   :: in_target_element = .false.
logical, private, save                   :: in_pcdata_level = .false.

logical, private, save                   :: stop_parsing = .false.
!
! This global variable determines whether we stop after
! getting the initial element tag, or after digesting the full node.
!
logical, private, save                   :: full_node = .true.

logical, private, save                   :: relative_path = .false.
logical, private, save                   :: looking_for_current_element
                             
logical, private, save                        :: attributes_requested
type(dictionary_t), private, save, pointer    :: attributes_recovered

integer, parameter, private                   :: MAX_PCDATA_SIZE = 65536
logical, private, save                        :: pcdata_requested 
character(len=MAX_PCDATA_SIZE), private, &
                                save          :: pcdata_recovered !*** Hard 
integer, private, save                        :: len_pcdata
integer, private, save                        :: max_len_pcdata

type(xml_t), pointer, save, private           :: xp

CONTAINS !===========================================================

!----------------------------------------------------
! Debugging control
!
subroutine enable_debug(sax)
logical, intent(in), optional  :: sax
  debug_xpath = .true.
  debug_sax = .false.
  if (present(sax)) then
     debug_sax = sax
  endif
end subroutine enable_debug

subroutine disable_debug()
  debug_xpath = .false.
end subroutine disable_debug

!----------------------------------------------------
! Main routines
!---------------------------------------------------------------------
subroutine mark_node(fxml,path,att_name,att_value,attributes,status)
!
! Performs a search of a given element (by path, and/or presence of
! a given attribute and/or value of that attribute), returning optionally
! the element's attribute dictionary, and leaving the file handle fxml
! ready to process the rest of the element's contents (child elements
! and/or pcdata).
!
! Side effects: it sets "ancestor_path" to the element's path
!
! If the argument "path" is present and evaluates to a relative path (a
! string not beginning with "/"), the search is interrupted after the end
! of the "ancestor_element" set by a previous call to "mark_node".
! If not earlier, the search ends at the end of the file.
!
! The status argument, if present, will hold a return value, 
! which will be:
!
!   0 on success,
!   negative in case of end-of-file or end-of-ancestor-element, or 
!   positive in case of a malfunction 
!
type(xml_t), intent(inout), target           :: fxml
character(len=*), intent(in), optional       :: path
character(len=*), intent(in), optional       :: att_name
character(len=*), intent(in), optional       :: att_value
type(dictionary_t), intent(out), optional    :: attributes
integer, intent(out), optional               :: status


character(len=200)         :: ancestor_path           ! local variable

     full_node = .false.
     call process_node(fxml,                    &
                       path,att_name,att_value, &
                       attributes,              &
                       status=status)
     if (status == 0) then
        call xml_mark_path(fxml,ancestor_path)
        if (debug_xpath) print *, "Setting ancestor_path to: ", trim(ancestor_path)
     endif

end subroutine mark_node

!--------------------------------------------------------------------
subroutine get_node(fxml,path,att_name,att_value,attributes,pcdata,status)
!
! Performs a search of a given element (by path, and/or presence of
! a given attribute and/or value of that attribute), returning optionally
! the element's attribute dictionary and any PCDATA characters contained
! in the element's scope (but not child elements). It leaves the file handle
! physically and logically positioned:
!
!     after the end of the element's start tag if 'pcdata' is not present
!     after the end of the element's end tag if 'pcdata' is present
!
! If the argument "path" is present and evaluates to a relative path (a
! string not beginning with "/"), the search is interrupted after the end
! of the "ancestor_element" set by a previous call to "mark_node".
! If not earlier, the search ends at the end of the file.
!
! The status argument, if present, will hold a return value, 
! which will be:
!
!   0 on success,
!   negative in case of end-of-file or end-of-ancestor-element, or 
!   positive in case of a malfunction (such as the overflow of the 
!   user's pcdata buffer).
!
type(xml_t), intent(inout), target           :: fxml
character(len=*), intent(in), optional       :: path
character(len=*), intent(in), optional       :: att_name
character(len=*), intent(in), optional       :: att_value
type(dictionary_t), intent(out), optional    :: attributes
character(len=*), intent(out), optional, target      :: pcdata
integer, intent(out), optional               :: status

     full_node = present(pcdata)
     call process_node(fxml,                    &
                       path,att_name,att_value, &
                       attributes,pcdata,       &
                       status=status)

end subroutine get_node
!
!--------------------------------------------------------------------
! Workhorse routines follow
!--------------------------------------------------------------------
subroutine process_node(fxml,path,att_name,att_value, &
                        attributes,pcdata,&
                        status)
type(xml_t), intent(inout), target           :: fxml
character(len=*), intent(in), optional       :: path
character(len=*), intent(in), optional       :: att_name
character(len=*), intent(in), optional       :: att_value
type(dictionary_t), intent(out), optional    :: attributes
character(len=*), intent(out), optional, target      :: pcdata
integer, intent(out), optional               :: status

logical :: path_present, att_name_present, att_value_present
logical :: attributes_present

character(len=3)   :: any_path  = "//*"
character(len=200) :: local_path, ancestor_path      ! *** Hard limit
character(len=500) :: value                          ! *** Hard limit
integer            :: local_status

type(dictionary_t) :: local_attributes

global_status = 0           ! reset

path_present = present(path)
attributes_present = present(attributes)
att_name_present = present(att_name)
att_value_present = present(att_value)

relative_path = .false.

if (path_present) then
   if (debug_xpath) print *, "SEARCHING for: ", trim(path)
   if (path(1:1) /= "/") then
      !
      ! Relative path search
      !
      call xml_path(fxml,local_path)
      call xml_get_path_mark(fxml,ancestor_path)
      if (ancestor_path == "") then
        if (debug_xpath) print *, "Relative search with null ancestor..."
      endif
      relative_path = .true.    
      if (debug_xpath) print *, "Relative search. ANCESTOR ELEMENT: ", &
                             trim(ancestor_path)
      !
      ! Convert to absolute path
      local_path = trim(local_path) // "/" // trim(path)
      if (debug_xpath) print *, "Converting ", trim(path), &
               " to absolute path: ", trim(local_path)
   else
      local_path = path
   endif
else
   local_path = any_path
endif

looking_for_current_element = (path == ".")
!
! Use local_attributes, since it is in principle possible that
! the user does not need to get back the attribute list.
!
do ! Loop until we satisfy the constraints 
   
   if (debug_xpath) print *, "--> Calling get_path ..."
   call get_path(fxml,local_path,local_attributes,pcdata,local_status)
   if (debug_xpath) print *, "-->Status after get_path: ", local_status
   if (local_status /= 0)  EXIT

   if (debug_xpath) print *, "FOUND path matching: ", trim(local_path)

   if (att_name_present) then
      if (debug_xpath) print *, "Checking ", trim(att_name), " among ", &
                          number_of_entries(local_attributes), " entries:"
      if (debug_xpath) call print_dict(local_attributes)

      if (has_key(local_attributes,att_name)) then

         if (att_value_present) then
            call get_value(local_attributes,att_name,value,local_status)
            if (local_status /= 0) then
               if (debug_xpath) print *, "Failed to get value of att: ", &
                                    trim(att_name)
               EXIT
            endif

            if (att_value == value)  then
               local_status = 0
               if (debug_xpath) print *, "Got correct att name and value "
               EXIT
            else
               if (debug_xpath) print *, "att value: ", trim(value), &
                                   " does not match"
               cycle        ! We keep searching
            endif
         else           ! Found att_name, and no value required
            local_status = 0
            if (debug_xpath) print *, "Got correct att name"
            EXIT
         endif
      else              ! Did not find that attribute name
         if (debug_xpath) print *, "Att name not present"
         cycle          ! keep searching
      endif
   else                 ! Found path, and no att info required
      local_status = 0
      if (debug_xpath) print *, "Found correct path. No other reqs."
      EXIT
   endif
   
enddo

if (present(status)) then
   status = local_status
   if (debug_xpath) print *, "--Returning status: ", status
endif

if (attributes_present) then
   attributes = local_attributes
endif

end subroutine process_node

!--------------------------------------------------------------------
subroutine get_path(fxml,path,attributes,pcdata,status)
type(xml_t), intent(inout), target           :: fxml
character(len=*), intent(in)                 :: path
type(dictionary_t), intent(out), optional, target    :: attributes
character(len=*), intent(out), optional, target      :: pcdata
integer, intent(out), optional                       :: status  

logical            :: status_present

xp => fxml

path_required = path
status_present = present(status)
pcdata_requested = (present(pcdata))

attributes_requested = (present(attributes))
if (attributes_requested) then
      call reset_dict(attributes)
      attributes_recovered => attributes
endif

if (pcdata_requested) then
!
!  Make sure we do not overstep the bounds of the supplied argument
!
   max_len_pcdata = min(len(pcdata),MAX_PCDATA_SIZE)
   len_pcdata = 0
   pcdata_recovered(1:max_len_pcdata) = ""
   if (debug_xpath) print *, "Max length of pcdata store: ", max_len_pcdata
endif

if (looking_for_current_element) then
   if (debug_xpath) print *, "Returning info about current element"

   ! We are now in the desired element, and we have the name and
   ! attribute list saved in xp.
   !
   if (attributes_requested) call xml_attributes(xp,attributes_recovered)
   !
   if (pcdata_requested) then
   !
   ! Set things up so that we can get the pcdata
   !
      call xml_path(xp,target_path)
      in_target_element = .true.
      in_pcdata_level = .true.
   else
      if (status_present) status = 0
      RETURN              ! We are done
   endif
else
   target_path = ""
   in_target_element = .false.
   in_pcdata_level = .false.
endif

stop_parsing = .false.

call xml_parse(fxml,  &
               begin_element_handler = begin_element , &
               end_element_handler = end_element,  &
               pcdata_chunk_handler = pcdata_handler, &
               verbose = debug_sax, signal_handler=pause_parsing, &
               empty_element_handler = empty_element)

if (eof_xmlfile(fxml)) then
   global_status = END_OF_FILE
   if (debug_xpath) print *, "Found end of file"
   if (pcdata_requested) pcdata = ""
else if (global_status == END_OF_ANCESTOR_ELEMENT) then
   if (debug_xpath) print *, "Found end of ancestor element"
   if (pcdata_requested) pcdata = ""
else
   if (debug_xpath) print *, "Parser found candidate element"
   if (pcdata_requested) then
      pcdata = pcdata_recovered(1:len_pcdata)
      if (debug_xpath) print *, "PCDATA recovered: ", pcdata_recovered(1:len_pcdata)
   endif
endif
if (global_status > 0) then
   if (debug_xpath) print *, "Something went slightly wrong. Status > 0"
endif
!
if (present(status)) status = global_status  


end subroutine get_path

!==================================================================
subroutine begin_element(name,attributes)
character(len=*), intent(in)   :: name
type(dictionary_t), intent(in) :: attributes

character(len=1000)   :: path              ! *** Hard limit

call xml_path(xp,path)
if (debug_xpath) print *, " begin_element ::: PATH: " ,  trim(path)
if (debug_xpath) print *, "path: ", trim(path), " req: ", trim(path_required)
if (match(path,path_required)) then
      if (debug_xpath) print *, " Match path: " ,  trim(path)
      in_target_element = .true.
      target_path = path
      in_pcdata_level = .true.
      if (debug_xpath) print *, "In element name: " , name
      if (attributes_requested) attributes_recovered = attributes
      ! stop parsing
      if (debug_xpath) print *, "full_node: ", full_node
      if (.not. full_node)  then
         if (debug_xpath) print *, "Stopping parsing after initial tag"
         stop_parsing = .true.
      endif
else 
   !
   ! If we are at the pcdata level and we enter another element, 
   ! we must not read pcdata
   !
   if (in_pcdata_level)   in_pcdata_level = .false.
endif

end subroutine begin_element
!------------------------------------------------------------
subroutine end_element(name)
character(len=*), intent(in)   :: name

character(len=300)   :: path             ! *** Hard limit
character(len=300)   :: left_path        ! *** Hard limit
character(len=300)   :: ancestor_path    ! *** Hard limit
!

call xml_path(xp,path)   ! path *after* leaving element
left_path = trim(path) // "/" // trim(name)

if (in_target_element) then
   if (path == target_path) then
      ! 
      ! We are back to pcdata level after visiting child elements
      !
      in_pcdata_level = .true.

   else  if (left_path == target_path) then

      ! We stop the parsing at the end of the element
      !
      if (debug_xpath) print *, "Exiting target element: ", trim(target_path)
      in_target_element = .false.
      in_pcdata_level = .false.
      if (debug_xpath) print *, "Stopping parsing after end of target element"
      stop_parsing = .true.
   endif
   
else if (relative_path) then
   !
   ! Check in case we go out of ancestor element
   !
   call xml_get_path_mark(xp,ancestor_path)
   if (match(left_path,ancestor_path)) then
      !
      ! We are leaving the ancestor element
      !
      if (debug_xpath) print *, "Relative search. End of element: ", name
      if (debug_xpath) print *, "Leaving Path: ", trim(left_path)
      if (debug_xpath) print *, "Ancestor Path: ", trim(ancestor_path)
      if (debug_xpath) print *, "Stopping parsing after end of ancestor element"
    
      stop_parsing = .true.
      global_status = END_OF_ANCESTOR_ELEMENT
   endif
endif
  
end subroutine end_element

!------------------------------------------------------------
subroutine empty_element(name,attributes)
character(len=*), intent(in)   :: name
type(dictionary_t), intent(in) :: attributes

character(len=300)   :: path              ! *** Hard limit

call xml_path(xp,path)
if (debug_xpath) print *, " empty_element ::: PATH: " ,  trim(path)
if (debug_xpath) print *, "path: ", trim(path), " req: ", trim(path_required)
if (match(path,path_required)) then
      if (debug_xpath) print *, " Match path: " ,  trim(path)
      if (debug_xpath) print *, "In (empty) element name: " , name
      if (attributes_requested) attributes_recovered = attributes
      ! stop parsing
      stop_parsing = .true.
      if (debug_xpath) print *, "Stopping parsing after empty tag"
      if (debug_xpath) print *, "full_node: ", full_node
      if (full_node)  then
         if (debug_xpath) print *, "*Warning: full_node requested, empty tag found"
      endif
endif
!
! There is no logic for ancestor element handling, as by definition
! an emtpy element cannot have children.
!
end subroutine empty_element

!-----------------------------------------------------------
subroutine pcdata_handler(chunk)
character(len=*), intent(in)   :: chunk

integer  :: len_chunk

if (in_pcdata_level) then
   ! 
   ! Build pcdata_recovered chunk by chunk, until it overflows
   !
   if (pcdata_requested) then
      if (debug_xpath) print *, "Found chunk of pcdata: ", chunk
      len_chunk = len(chunk)
      if ((len_pcdata + len_chunk) > max_len_pcdata) then
         !
         if (debug_xpath) print *, "***Pcdata Overflow "
         global_status = PCDATA_OVERFLOW
         stop_parsing = .true.
         return
      endif
      pcdata_recovered(len_pcdata+1:len_pcdata+len_chunk) = chunk
      len_pcdata = len_pcdata + len_chunk
   endif
endif

end subroutine pcdata_handler
!--------------------------------------------------------------------

subroutine pause_parsing(res)
logical, intent(out)  :: res

res =  stop_parsing

end subroutine pause_parsing
!--------------------------------------------------------------------

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
character(len=100)   :: anchor_leaf                     ! *** Hard limit

res_match = .false.       

if (trim(p) == trim(ptarget)) then
   res_match = .true.
   return

else if (ptarget == "/") then
   ! We  process // in the middle below

   res_match = .true.
   return

else              ! We get the extreme elements

   len_target = len_trim(ptarget)
   len_path = len_trim(p)
   pos_target = index(ptarget,"/",back=.true.)
   pos_path = index(p,"/",back=.true.)

   if (pos_target == len_target) then   ! // in the middle...
      ! Get leaf further up
      search_anchor : do 
         pos_target = index(ptarget(1:len_target-1),"/",back=.true.)
         if (pos_target == 1) then  ! Target begins by /.// 
            res_match = .true.
            return
         endif
         anchor_leaf = ptarget(pos_target:len_target-1)
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
      pos_path = index(p(1:len_path),trim(anchor_leaf),back=.true.)
      if (pos_path /= 0) then

         ! Found anchor. Continue further up.
         !
         res_match = match(p(1:pos_path-1),ptarget(1:pos_target-1))
      endif

   else if  (ptarget(pos_target+1:len_target) == ".") then

      ! A dot is a dummy. Continue further up in target path.
      !
      res_match = match(p(1:len_path),ptarget(1:pos_target-1))

   else if  (ptarget(pos_target+1:len_target) == "*") then

      if (len_path == pos_path) then
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

end module m_path












