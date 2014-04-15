module m_stmw

  use flib_wxml

  private

  integer, private, parameter ::  sp = selected_real_kind(6,30)
  integer, private, parameter ::  dp = selected_real_kind(14,100)

!  TYPE(xmlf_t), save,  :: xf

  PUBLIC :: stmAddScalar
  PUBLIC :: stmAddArray
  PUBLIC :: stmAddMatrix
  PUBLIC :: stmAddTriangle
  PUBLIC :: stmAddStartTag

  INTERFACE stmAddScalar
     MODULE PROCEDURE stmAddString, stmAddInteger, stmAddFloatSP, stmAddFloatDP
  END INTERFACE

  INTERFACE stmAddArray
     MODULE PROCEDURE stmAddFloatArraySP, stmAddFloatArrayDP, stmAddStringArray, &
                      stmAddIntegerArray
  END INTERFACE

  INTERFACE stmAddMatrix
     MODULE PROCEDURE stmAddFloatMatrixSP, stmAddFloatMatrixDP, stmAddIntegerMatrix
  END INTERFACE

  INTERFACE stmAddTriangle
     MODULE PROCEDURE stmAddTriangleSP, stmAddTriangleDP
  END INTERFACE

 
CONTAINS
  
  
  ! =================================================
  ! STMML convenience routines
  ! =================================================
  
  ! -------------------------------------------------
  ! create STMML start tag in xml channel
  ! -------------------------------------------------
  
  SUBROUTINE stmAddStartTag(xf, name, id, title, dictref, dataType, &
       convention, errorValue, errorBasis, min, max, units)

    implicit none
    type(xmlf_t) :: xf
    character(len=*), intent(in) :: name                   ! the element name
    character(len=*), intent(in), optional :: id           ! the element id; if whitespace, is omitted
    character(len=*), intent(in), optional :: title        ! the title; if whitespace, is omitted
    character(len=*), intent(in), optional :: dictref      ! the dictionary reference; if whitespace, is omitted
    character(len=*), intent(in), optional :: dataType  
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: errorValue
    character(len=*), intent(in), optional :: errorBasis
    character(len=*), intent(in), optional :: min
    character(len=*), intent(in), optional :: max
    character(len=*), intent(in), optional :: units

!    call XMLCHKN9(name)
    call xml_NewElement(xf, name)
    if (present(id))         call xml_AddAttribute(xf, 'id', id)
    if (present(title))      call xml_AddAttribute(xf, 'title', title)
    if (present(dictref))    call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(dataType))   call xml_AddAttribute(xf, 'dataType', dataType)
    if (present(convention)) call xml_AddAttribute(xf, 'convention', convention)
    if (present(errorValue)) call xml_AddAttribute(xf, 'errorValue', errorValue)
    if (present(errorBasis)) call xml_AddAttribute(xf, 'errorBasis', errorBasis)
    if (present(min))        call xml_AddAttribute(xf, 'min', min)
    if (present(max))        call xml_AddAttribute(xf, 'max', max)
    if (present(units))      call xml_AddAttribute(xf, 'units', units)

  END SUBROUTINE stmAddStartTag


  ! -------------------------------------------------
  ! outputs STMML scalar in xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddString(xf, value, id, title, dictref, dataType, &
       convention, errorValue, errorBasis, min, max, units)

    implicit none
    type(xmlf_t) :: xf
    character(len=*), intent(in)           :: value         ! the value to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: dataType  
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: errorValue
    character(len=*), intent(in), optional :: errorBasis
    character(len=*), intent(in), optional :: min
    character(len=*), intent(in), optional :: max
    character(len=*), intent(in), optional :: units

    ! Internal variables
    character(len=20) :: temp

!    if (XMLCHKS9(value)) then
    call xml_AddPcdata(xf, ' '//value)

    call xml_NewElement(xf, 'scalar')
    if (present(id))         call xml_AddAttribute(xf, 'id', id)
    if (present(title))      call xml_AddAttribute(xf, 'title', title)
    if (present(dictref))    call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(dataType))   call xml_AddAttribute(xf, 'dataType', dataType)
    if (present(convention)) call xml_AddAttribute(xf, 'convention', convention)
    if (present(errorValue)) call xml_AddAttribute(xf, 'errorValue', errorValue)
    if (present(errorBasis)) call xml_AddAttribute(xf, 'errorBasis', errorBasis)
    if (present(min))        call xml_AddAttribute(xf, 'min', min)
    if (present(max))        call xml_AddAttribute(xf, 'max', max)
    if (present(units))      call xml_AddAttribute(xf, 'units', units)
    call xml_EndElement(xf, 'scalar')

  END SUBROUTINE stmAddString


  ! -------------------------------------------------
  ! outputs STMML integer in xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddInteger(xf, value, id, title, dictref, dataType, &
       convention, errorValue, errorBasis, min, max, units)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in) :: value                           ! the value to be output
    character(len=*), intent(in), optional :: id           ! the id
    character(len=*), intent(in), optional :: title        ! the title
    character(len=*), intent(in), optional :: dictref      ! the dictionary reference
    character(len=*), intent(in), optional :: dataType  
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: errorValue
    character(len=*), intent(in), optional :: errorBasis
    character(len=*), intent(in), optional :: min
    character(len=*), intent(in), optional :: max
    character(len=*), intent(in), optional :: units        ! units (default = none)


    ! Flush on entry and exit
    call xml_NewElement(xf, 'scalar')
    if (present(id))         call xml_AddAttribute(xf, 'id', id)
    if (present(title))      call xml_AddAttribute(xf, 'dictRef', title)
    if (present(dictref))    call xml_AddAttribute(xf, 'title', dictref)
    if (present(dataType))   call xml_AddAttribute(xf, 'dataType', dataType)
    if (present(convention)) call xml_AddAttribute(xf, 'convention', convention)
    if (present(errorValue)) call xml_AddAttribute(xf, 'errorValue', errorValue)
    if (present(errorBasis)) call xml_AddAttribute(xf, 'errorBasis', errorBasis)
    if (present(min))        call xml_AddAttribute(xf, 'min', min)
    if (present(max))        call xml_AddAttribute(xf, 'max', max)
    if (present(units))      call xml_AddAttribute(xf, 'units', units)
    call xml_AddPcdata(xf, str(value))
    call xml_EndElement(xf, 'scalar')

  END SUBROUTINE stmAddInteger


  ! -------------------------------------------------
  ! 1. create an STMML <scalar> DP float in xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddFloatDP(xf, value, id, title, dictref, dataType, &
       convention, errorValue, errorBasis, min, max, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=dp), intent(in)               :: value        ! the value to be output
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! the title
    character(len=*), intent(in), optional :: dictref      ! the dictionary reference
    character(len=*), intent(in), optional :: dataType  
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: errorValue
    character(len=*), intent(in), optional :: errorBasis
    character(len=*), intent(in), optional :: min
    character(len=*), intent(in), optional :: max
    character(len=*), intent(in), optional :: units        ! units
    character(len=*), intent(in), optional :: fmt          ! the format (default 'f10.4')

    ! Internal Vaiable
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f10.4)'
    endif


    ! Flushes on entry and exit
    call xml_NewElement(xf, 'scalar')
    if (present(id))         call xml_AddAttribute(xf, 'id', id)
    if (present(title))      call xml_AddAttribute(xf, 'title', title)
    if (present(dictref))    call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(dataType))   call xml_AddAttribute(xf, 'dataType', dataType)
    if (present(convention)) call xml_AddAttribute(xf, 'convention', convention)
    if (present(errorValue)) call xml_AddAttribute(xf, 'errorValue', errorValue)
    if (present(errorBasis)) call xml_AddAttribute(xf, 'errorBasis', errorBasis)
    if (present(min))        call xml_AddAttribute(xf, 'min', min)
    if (present(max))        call xml_AddAttribute(xf, 'max', max)
    if (present(units))      call xml_AddAttribute(xf, 'units', units)

    call xml_AddPcdata(xf, str(value))
    call xml_EndElement(xf, 'scalar')

  END SUBROUTINE stmAddFloatDP

  ! -------------------------------------------------
  ! 2. create an STMML <scalar> SP float in xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddFloatSP(xf, value, id, title, dictref, dataType, &
       convention, errorValue, errorBasis, min, max, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in)               :: value        ! the value to be output
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! the title
    character(len=*), intent(in), optional :: dictref      ! the dictionary reference
    character(len=*), intent(in), optional :: units        ! units (' ' = none)
    character(len=*), intent(in), optional :: dataType  
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: errorValue
    character(len=*), intent(in), optional :: errorBasis
    character(len=*), intent(in), optional :: min
    character(len=*), intent(in), optional :: max
    character(len=*), intent(in), optional :: fmt          ! the format (default 'f10.4')

    ! Internal Variable
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f10.4)'
    endif

    ! Flushes on entry and exit
    call xml_NewElement(xf, 'scalar')
    if (present(id))         call xml_AddAttribute(xf, 'id', id)
    if (present(title))      call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(dictref))    call xml_AddAttribute(xf, 'title', title)
    if (present(dataType))   call xml_AddAttribute(xf, 'dataType', dataType)
    if (present(convention)) call xml_AddAttribute(xf, 'convention', convention)
    if (present(errorValue)) call xml_AddAttribute(xf, 'errorValue', errorValue)
    if (present(errorBasis)) call xml_AddAttribute(xf, 'errorBasis', errorBasis)
    if (present(min))        call xml_AddAttribute(xf, 'min', min)
    if (present(max))        call xml_AddAttribute(xf, 'max', max)
    if (present(units))      call xml_AddAttribute(xf, 'units', units)
    call xml_AddPcdata(xf, str(value))
    call xml_EndElement(xf, 'scalar')

  END SUBROUTINE stmAddFloatSP


  ! -------------------------------------------------
  ! outputs string array to xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddStringArray(xf, nvalue, array, id, title, dictref, type, delim, ref)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: nvalue        ! number of values to be output
    character(len=*), intent(in)           :: array(*)      ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: type          ! the dataType
    character(len=*), intent(in), optional :: delim         ! delimiter
    character(len=*), intent(in), optional :: ref           ! delimiter

    ! splits data into lines whenever it overflows workspace/linelength
    ! Flush on entry and exit
    character(len=1) :: delim1
    integer          :: i


    if (present(delim)) then
       delim1 = delim
    else
       delim1 = ' '
    endif

    call xml_NewElement(xf, 'array')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(type))    call xml_AddAttribute(xf, 'type', type)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call xml_AddAttribute(xf, 'delimiter', delim1)
    call xml_AddAttribute(xf, 'size', str(nvalue))

    call xml_AddPcdata(xf, array(1))
    do i = 2, nvalue
       if (delim1 .eq. ' ') then
          call xml_AddPcdata(xf, ' '//array(i))
       else
          call xml_AddPcdata(xf, delim1//array(i))
       endif
    enddo
    call xml_EndElement(xf, 'array')

  END SUBROUTINE stmAddStringArray


  ! -------------------------------------------------
  ! outputs integer array to xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddIntegerArray(xf, nvalue, array, id, title, dictref, ref, units)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: nvalue        ! the number of values to be output
    integer, intent(in)                    :: array(*)      ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: ref           ! scienitific units (default ' ')

    ! splits data into lines wherever it overflows the workspace
    integer          :: i

    ! Flush on entry and exit

    call xml_NewElement(xf, 'array')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(units))   call xml_AddAttribute(xf, 'units', units)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call xml_AddAttribute(xf, 'size', str(nvalue))


    call xml_AddPcdata(xf, str(array(1)))
    do i = 2, nvalue
       call xml_AddPcdata(xf, str(array(i)))
    enddo
    call xml_EndElement(xf, 'array')
    
  END SUBROUTINE stmAddIntegerArray


  ! -------------------------------------------------
  ! 1. outputs DP float array to xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddFloatArrayDP(xf, nvalue, array, id, title, dictref, units, ref, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: nvalue        ! number of values to be output
    real(kind=dp), intent(in)               :: array(*)       ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: ref           ! 
    character(len=*), intent(in), optional :: fmt           ! the output format

    ! Internal Variable
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! splits data into lines whenever it overflows workspace/linelength
    ! Flush on entry and exit

    call xml_NewElement(xf, 'array')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(units))   call xml_AddAttribute(xf, 'units', units)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call xml_AddAttribute(xf, 'size', str(nvalue))
    call STMARCF9DP(xf, nvalue, array, fmt)
    call xml_EndElement(xf, 'array')

  END SUBROUTINE stmAddFloatArrayDP

  ! -------------------------------------------------
  ! 2. outputs SP float array to xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddFloatArraySP(xf, nvalue, array, id, title, dictref, units, ref, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: nvalue        ! number of values to be output
    real(kind=sp), intent(in)               :: array(*)       ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: fmt           ! the output format
    character(len=*), intent(in), optional :: ref           ! the output format

    ! Internal Variable
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! splits data into lines whenever it overflows workspace/linelength
    ! Flush on entry and exit

    call xml_NewElement(xf, 'array')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(units))   call xml_AddAttribute(xf, 'units', units)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call xml_AddAttribute(xf, 'size', str(nvalue))
    call STMARCF9SP(xf, nvalue, array, fmt)
    call xml_EndElement(xf, 'array')

  END SUBROUTINE stmAddFloatArraySP


  ! -------------------------------------------------
  ! outputs integer matrix to xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddIntegerMatrix(xf, nrows, ncols, dim, matrix, id, title, dictref, units)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: nrows         ! the number of rows to be output
    integer, intent(in)                    :: ncols         ! the number of rows to be output
    integer, intent(in)                    :: dim           ! the range of the fastest index
    integer, intent(in)                    :: matrix(nrows,ncols) ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')

    ! splits data into lines wherever it overflows the workspace
    ! Flush on entry and exit
    integer ::  i, j



    call xml_NewElement(xf, 'matrix')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(dictref))   call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(units))   call xml_AddAttribute(xf, 'units', units)
    call xml_AddAttribute(xf, 'cols', str(ncols))
    call xml_AddAttribute(xf, 'rows', str(nrows))

!
!   Try addArray...
!
    do i = 1, ncols
       do j = 1, nrows
          call xml_AddPcdata(xf, str(matrix(j, i)))
       enddo
    enddo
    call xml_EndElement(xf, 'matrix')

  END SUBROUTINE stmAddIntegerMatrix


  ! -------------------------------------------------
  ! 1. outputs DP float matrix to xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddFloatMatrixDP(xf, ncols, nrows, dim, matrix, id, title, dictref, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: ncols                ! the number of cols to be output
    integer, intent(in)                    :: nrows                ! the number of rows to be output
    integer, intent(in)                    :: dim                  ! the range of the fastest index
    real(kind=dp), intent(in)               :: matrix(ncols,nrows)  ! the values to be output
    character(len=*), intent(in), optional :: id                   ! the id
    character(len=*), intent(in), optional :: title                ! the title
    character(len=*), intent(in), optional :: dictref              ! the dictionary reference
    character(len=*), intent(in), optional :: units                ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: fmt                  ! format

    ! internal variable
    character(len=10) :: formt
    integer ::  i, j

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! splits data into lines wherever it overflows the workspace
    ! Flush on entry and exit      
    !-------------
    call xml_NewElement(xf, 'matrix')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(units))   call xml_AddAttribute(xf, 'units', units)
    call xml_AddAttribute(xf, 'cols', str(ncols))
    call xml_AddAttribute(xf, 'rows', str(nrows))
    !-------------
    do i = 1, nrows
       do j = 1, ncols
          !              write(*,*) ">>> 1", i, j
          call xml_AddPcdata(xf, str(matrix(j, i)))
       enddo
    enddo
    call xml_EndElement(xf, 'matrix')

  END SUBROUTINE stmAddFloatMatrixDP

  ! -------------------------------------------------
  ! 2. outputs SP float matrix to xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddFloatMatrixSP(xf, ncols, nrows, dim, matrix, id, title, dictref, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: ncols               ! the number of cols to be output
    integer, intent(in)                    :: nrows               ! the number of rows to be output
    integer, intent(in)                    :: dim                 ! the range of the fastest index
    real(kind=sp), intent(in)               :: matrix(ncols,nrows) ! the values to be output
    character(len=*), intent(in), optional :: id                  ! the id
    character(len=*), intent(in), optional :: title               ! the title
    character(len=*), intent(in), optional :: dictref             ! the dictionary reference
    character(len=*), intent(in), optional :: units               ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: fmt                 ! format

    ! internal variable
    character(len=10) :: formt
    integer ::  i, j

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! splits data into lines wherever it overflows the workspace
    ! Flush on entry and exit      
    !
    call xml_NewElement(xf, 'matrix')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(units))   call xml_AddAttribute(xf, 'units', units)
    call xml_AddAttribute(xf, 'cols', str(ncols))
    call xml_AddAttribute(xf, 'rows', str(nrows))
    do i = 1, nrows
       do j = 1, ncols
          call xml_AddPcdata(xf, str(matrix(j, i)))
       enddo
    enddo
    call xml_EndElement(xf, 'matrix')

  END SUBROUTINE stmAddFloatMatrixSP


  ! -------------------------------------------------
  ! 1. outputs DP lower triangle array to xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddTriangleDP(xf, nvalue, array, id, title, dictref, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: nvalue         ! number of values to be output
    real(kind=dp), intent(in)               :: array(*)        ! the values to be output
    character(len=*), intent(in), optional :: id             ! the id
    character(len=*), intent(in), optional :: title          ! the title
    character(len=*), intent(in), optional :: dictref        ! the dictionary reference
    character(len=*), intent(in), optional :: units          ! units (' ' = none)
    character(len=*), intent(in), optional :: fmt            ! the output format

    ! splits data into lines whenever it overflows workspace/linelength
    ! Flush on entry and exit
    integer           :: size
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    size = (nvalue*(nvalue+1))/2
    call xml_NewElement(xf, 'array')
    call xml_AddAttribute(xf, 'size', str(size))
    call xml_AddAttribute(xf, 'rows', str(nvalue))
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(units))   call xml_AddAttribute(xf, 'units', units)
    call STMARCF9DP(xf, size, array, formt)
    call xml_EndElement(xf, 'matrix')

  END SUBROUTINE stmAddTriangleDP

  ! -------------------------------------------------
  ! 2. outputs SP lower triangle array to xml channel
  ! -------------------------------------------------

  SUBROUTINE stmAddTriangleSP(xf, nvalue, array, id, title, dictref, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: nvalue         ! number of values to be output
    real(kind=sp), intent(in)               :: array(*)        ! the values to be output
    character(len=*), intent(in), optional :: id             ! the id
    character(len=*), intent(in), optional :: title          ! the title
    character(len=*), intent(in), optional :: dictref        ! the dictionary reference
    character(len=*), intent(in), optional :: units          ! units (' ' = none)
    character(len=*), intent(in), optional :: fmt            ! the output format

    ! splits data into lines whenever it overflows workspace/linelength
    ! Flush on entry and exit
    integer           :: size
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    size = (nvalue*(nvalue+1))/2
    call xml_NewElement(xf, 'array')
    call xml_AddAttribute(xf, 'size', str(size))
    call xml_AddAttribute(xf, 'rows', str(nvalue))
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(units))   call xml_AddAttribute(xf, 'units', units)
    call STMARCF9SP(xf, size, array, formt)
    call xml_EndElement(xf, 'matrix')

  END SUBROUTINE stmAddTriangleSP


  ! -------------------------------------------------
  ! outputs fatal error message
  ! -------------------------------------------------

  SUBROUTINE stmErrorMessage(xf, msg, id, title, dictref)

    implicit none
    type(xmlf_t) :: xf
    character(len=*), intent(in)           :: msg            ! the message
    character(len=*), intent(in), optional :: id             ! the id
    character(len=*), intent(in), optional :: title          ! the title
    character(len=*), intent(in), optional :: dictref        ! the dictionary reference

    call xml_NewElement(xf, 'message')
    call xml_AddAttribute(xf, 'severity', 'fatal')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    call xml_AddPcdata(xf, msg)
    call xml_EndElement(xf, 'message')

  END SUBROUTINE stmErrorMessage


  ! -------------------------------------------------
  ! outputs informational message
  ! -------------------------------------------------

  SUBROUTINE stmInfoMessage(xf, msg, id, title, dictref)

    implicit none
    type(xmlf_t) :: xf
    character(len=*), intent(in)           :: msg            ! the message
    character(len=*), intent(in), optional :: id             ! the id
    character(len=*), intent(in), optional :: title          ! the title
    character(len=*), intent(in), optional :: dictref        ! the dictionary reference

    call xml_NewElement(xf, 'message')
    call xml_AddAttribute(xf, 'severity', 'warning')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    call xml_AddPcdata(xf, msg)
    call xml_EndElement(xf, 'message')

  END SUBROUTINE stmInfoMessage


  ! -------------------------------------------------
  ! outputs warning message
  ! -------------------------------------------------

  SUBROUTINE stmWarningMessage(xf, msg, id, title, dictref)

    implicit none
    type(xmlf_t) :: xf
    character(len=*), intent(in)           :: msg            ! the message
    character(len=*), intent(in), optional :: id             ! the id
    character(len=*), intent(in), optional :: title          ! the title
    character(len=*), intent(in), optional :: dictref        ! the dictionary reference

    call xml_NewElement(xf, 'message')
    call xml_AddAttribute(xf, 'severity', 'info')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    call xml_AddPcdata(xf, msg)
    call xml_EndElement(xf, 'message')

  END SUBROUTINE stmWarningMessage



  ! =================================================
  ! basic STMML routines
  ! =================================================


  ! -------------------------------------------------
  ! creates STMML <scalar> string
  ! -------------------------------------------------

  SUBROUTINE STMSCAS9(xf, value, id, title, dictref, type)

    implicit none
    type(xmlf_t) :: xf
    character(len=*), intent(in) :: value                 ! the value to be output
    character(len=*), intent(in), optional :: id          ! the id
    character(len=*), intent(in), optional :: title       ! the title
    character(len=*), intent(in), optional :: dictref     ! the dictionary reference
    character(len=*), intent(in), optional :: type        ! the data type (default 'xsd:string')

    ! Internal variables
    character(len=20) :: temp

    call xml_NewElement(xf, 'scalar')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(dictref)) call xml_AddAttribute(xf, 'title', title)
    if (present(type))    call xml_AddAttribute(xf, 'dataType', type)

!    if (XMLCHKS9(value)) then
    call xml_AddPcdata(xf, value)
    call xml_EndElement(xf, 'scalar')

  END SUBROUTINE STMSCAS9



  ! -------------------------------------------------
  ! output start tag for an STMML array
  ! -------------------------------------------------

  SUBROUTINE STMARST9(xf, nvalue, id, title, dictref, typunt, tuval, delim)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: nvalue      ! the number of values to be output
    character(len=*), intent(in), optional :: id          ! the id
    character(len=*), intent(in), optional :: title       ! the title
    character(len=*), intent(in), optional :: dictref     ! the dictionary reference
    character(len=*), intent(in), optional :: typunt      ! 'type' (for strings) or 'unit' (for numeric)
    character(len=*), intent(in), optional :: tuval       ! data type (default 'xsd:string') or units (' ' = none)
    character(len=*), intent(in), optional :: delim       ! the delimiter (default ' ')

    ! Internal Variables
    character(len=1) :: delim1

    if (present(delim)) then
       delim1 = delim
    else
       delim1 = ' '
    endif

    call xml_NewElement(xf, 'array')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(dictref)) call xml_AddAttribute(xf, 'title', title)
    if (present(tuval))   call xml_AddAttribute(xf, 'type', tuval)
    call xml_AddAttribute(xf, 'delimiter', delim1)
    call xml_AddAttribute(xf, 'size', str(nvalue))
    call xml_EndElement(xf, 'array')

  END SUBROUTINE STMARST9



  ! -------------------------------------------------
  ! 2. outputs SP float array to channel
  ! -------------------------------------------------

  SUBROUTINE STMARF9SP(xf, nvalue, arrf, id, title, dictref, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: nvalue      ! the number of values to be output
    real(kind=sp), intent(in)               :: arrf(*)     ! the values to be output
    character(len=*), intent(in), optional :: id          ! the id
    character(len=*), intent(in), optional :: title       ! the title
    character(len=*), intent(in), optional :: dictref     ! the dictionary reference
    character(len=*), intent(in), optional :: units       ! units (' ' = none)
    character(len=*)                       :: fmt         ! the output format

    call xml_NewElement(xf, 'scalar')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(dictref)) call xml_AddAttribute(xf, 'title', title)
    if (present(units))   call xml_AddAttribute(xf, 'units', units)
    call xml_AddAttribute(xf, 'size', str(nvalue))
    call STMARCF9SP(xf, nvalue, arrf, fmt)
    call xml_NewElement(xf, 'scalar')

  END SUBROUTINE STMARF9SP


  ! -------------------------------------------------
  ! 1. outputs content of DP float array to channel
  ! -------------------------------------------------

  SUBROUTINE STMARCF9DP(xf, nvalue, arrf, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)          :: nvalue     ! the number of values to be output
    real(kind=dp), intent(in)     :: arrf(*)    ! the values to be output
    character(len=*), intent(in) :: fmt        ! the output format

    ! splits data into lines whenever it overflows workspace/linelength
    integer :: i

    call xml_AddPcdata(xf, str(arrf(1)))
    do i = 2, nvalue
       call xml_AddPcdata(xf, str(arrf(i)))
    enddo
  END SUBROUTINE STMARCF9DP


  ! -------------------------------------------------
  ! 2. outputs content of SP float array to channel
  ! -------------------------------------------------

  SUBROUTINE STMARCF9SP(xf, nvalue, arrf, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer          :: nvalue     ! the number of values to be output
    real(kind=sp)     :: arrf(*)    ! the values to be output
    character(len=*) :: fmt        ! the output format

    ! splits data into lines whenever it overflows workspace/linelength
    integer :: i

    call xml_AddPcdata(xf, str(arrf(1)))
    do i = 2, nvalue
       call xml_AddPcdata(xf, str(arrf(i)))
    enddo
  END SUBROUTINE STMARCF9SP

end module m_stmw
