module m_cmlw

  use flib_wxml
  use m_stmw

  private

  integer, private, parameter ::  sp = selected_real_kind(6,30)
  integer, private, parameter ::  dp = selected_real_kind(14,100)


! CMLUnits
  character(len=40), parameter :: U_ANGSTR = 'units:angstrom'
  character(len=40), parameter :: U_PMETER = 'units:pm'
  character(len=40), parameter :: U_DEGREE = 'units:degree'
  character(len=40), parameter :: U_RADIAN = 'units:radian'
  character(len=40), parameter :: U_INVCM  = 'units:cm-1'
  character(len=40), parameter :: U_KCALMO = 'units:kcal-mole'
  character(len=40), parameter :: U_EVOLT  = 'units:ev'
  character(len=40), parameter :: U_SECOND = 'units:second'
  character(len=40), parameter :: U_VOLT   = 'units:volt'

! CMLCore
  PUBLIC :: cmlAddCoordinates
  PUBLIC :: cmlAddCrystal
  PUBLIC :: cmlAddAngle
  PUBLIC :: cmlAddLength
  PUBLIC :: cmlAddEigenvalue
  PUBLIC :: cmlAddProperty
  PUBLIC :: cmlAddPropertyList
  PUBLIC :: cmlAddMolecule
  PUBLIC :: cmlAddMetadata

! CMLComp
  PUBLIC :: cmlAddLattice
  PUBLIC :: cmlAddLatticeVector
  PUBLIC :: cmlAddParameter

! CMLCore
  INTERFACE cmlAddCoordinates
     MODULE PROCEDURE cmlAddCoordinatesSP, cmlAddCoordinatesDP
  END INTERFACE

  INTERFACE cmlAddCrystal
     MODULE PROCEDURE cmlAddCrystalSP, cmlAddCrystalDP
  END INTERFACE

  INTERFACE cmlAddAngle
     MODULE PROCEDURE cmlAddAngleSP, cmlAddAngleDP
  END INTERFACE

  INTERFACE cmlAddLength
     MODULE PROCEDURE cmlAddLengthSP, cmlAddLengthDP
  END INTERFACE

  INTERFACE cmlAddEigenvalue
     MODULE PROCEDURE cmlAddEigenvalueSP, cmlAddEigenvalueDP
  END INTERFACE

  INTERFACE cmlAddMolecule
     MODULE PROCEDURE cmlAddMoleculeSP, cmlAddMoleculeDP, cmlAddMolecule3SP, &
                      cmlAddMolecule3DP
  END INTERFACE

! CMLComa
  INTERFACE cmlAddLattice
     MODULE PROCEDURE cmlAddLatticeSP, cmlAddLatticeDP
  END INTERFACE

  INTERFACE cmlAddLatticeVector
     MODULE PROCEDURE cmlAddLatticeVectorSP, cmlAddLatticeVectorDP
  END INTERFACE

  INTERFACE cmlAddProperty
     MODULE PROCEDURE &
          cmlAddPropScalarDP, cmlAddPropScalarSP, cmlAddPropScalarI, & 
          cmlAddPropMatrixDP, cmlAddPropMatrixSP, cmlAddPropMatrixI, &
          cmlAddPropArrayDP,  cmlAddPropArraySP,  cmlAddPropArrayI
  END INTERFACE

  INTERFACE cmlAddParameter
     MODULE PROCEDURE &
        cmlAddParameterCH, cmlAddParameterI, &
        cmlAddParameterSP, cmlAddParameterDP, &
        cmlAddParameterLG
  END INTERFACE


CONTAINS

  ! =================================================
  ! convenience CML routines
  ! =================================================
  
  ! -------------------------------------------------
  ! writes a propertyList start Tag to xml channel
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddPropertyList(xf, id, title, conv, dictref, ref, role)

    implicit none
    type(xmlf_t) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: role
    
    call xml_NewElement(xf, 'propertyList')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    
  END SUBROUTINE cmlAddPropertyList
  
  
  ! -------------------------------------------------
  ! 1. writes a DP property to xml channel
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddPropScalarDP(xf, property, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=dp), intent(in)               :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))    call xml_AddAttribute(xf, 'ref', ref)
    call stmAddScalar(xf=xf, value=property, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')

  END SUBROUTINE cmlAddPropScalarDP

  ! -------------------------------------------------
  ! 2. writes a Scalar SP property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropScalarSP(xf, property, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in) :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddScalar(xf=xf, value=property, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropScalarSP
  
  ! -------------------------------------------------
  ! 3. writes a Scalar integer property to xml channel
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddPropScalarI(xf, property, id, title, conv, dictref, ref, units)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in) :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddScalar(xf=xf, value=property, units=units)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropScalarI

  ! -------------------------------------------------
  ! 4. writes an Float matrix property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropMatrixDP(xf, property, nrows, ncols, dim, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)      :: nrows
    integer, intent(in)      :: ncols
    integer, intent(in)      :: dim
    real(kind=dp), intent(in) :: property(nrows,ncols)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddMatrix(xf=xf, matrix=property, dim=dim, ncols=ncols, nrows=nrows, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropMatrixDP

  ! -------------------------------------------------
  ! 5. writes an SP Float matrix property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropMatrixSP(xf, property, nrows, ncols, dim, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)      :: nrows
    integer, intent(in)      :: ncols
    integer, intent(in)      :: dim
    real(kind=sp), intent(in) :: property(nrows,ncols)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddMatrix(xf=xf,matrix=property, dim=dim, ncols=ncols, nrows=nrows, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropMatrixSP


  ! -------------------------------------------------
  ! 6. writes an Integer matrix property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropMatrixI(xf, property, nrows, ncols, dim, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t) :: xf

    integer, intent(in)                    :: nrows
    integer, intent(in)                    :: ncols
    integer, intent(in), optional          :: dim
    integer, intent(in)                    :: property(nrows,ncols)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(conv)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddMatrix(xf=xf, matrix=property, dim=dim, ncols=ncols, nrows=nrows, units=units)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropMatrixI


  ! -------------------------------------------------
  ! 7. writes an Array DP property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropArrayDP(xf, property, nvalue, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=dp), intent(in)               :: property(*)
    integer, intent(in)                    :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddArray(xf=xf, array=property, nvalue=nvalue, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropArrayDP

  ! -------------------------------------------------
  ! 8. writes an Array SP property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropArraySP(xf, property, nvalue, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in)               :: property(*)
    integer, intent(in)                    :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddArray(xf=xf, array=property, nvalue=nvalue, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropArraySP

  ! -------------------------------------------------
  ! 9. writes an Array integer property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropArrayI(xf, property, nvalue, id, title, conv, dictref, ref, units)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: property(*)
    integer, intent(in)                    :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddArray(xf, array=property, nvalue=nvalue, units=units)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropArrayI

  !------------------------------------------------------------
  ! END OF PROPERTIES 
  !------------------------------------------------------------


  ! -------------------------------------------------
  ! 1. writes complete DP molecule to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddMoleculeDP(xf, natoms, elements, coords, style, id, title, dictref, fmt)

    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: natoms             ! number of atoms
    real(kind=dp), intent(in)               :: coords(3, natoms)  ! atomic coordinates
    character(len=*), intent(in)           :: elements(natoms)   ! chemical element types
    character(len=*), intent(in), optional :: id                 ! id
    character(len=*), intent(in), optional :: title              ! the title
    character(len=*), intent(in), optional :: dictref            ! the dictionary reference
    character(len=*), intent(in), optional :: fmt                ! format for coords
    character(len=*), intent(in), optional :: style              ! type of coordinates 

    ! 'x3' for Cartesians, 
    ! 'xFrac' for fractionals
    ! default => cartesians

    ! Internal Variables
    character(len=6) :: id1, id0
    character(len=10):: formt, stylei
    integer          :: i

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif
    if (present(style)) then
       stylei = style
    else
       stylei = 'x3'
    endif

    call stmAddStartTag(xf, 'molecule', id, title, dictref)
    call xml_NewElement(xf, 'atomArray')
    do i = 1, natoms
       write(id0, '(i4)') i
       id0 = adjustl(id0)
       id1 = 'a'
       id1(2:) = id0
       call cmlAddAtom(xf=xf, elem=elements(i), id=id1)
       if (stylei .eq. 'x3') then
          call CMLATX39DP(xf, coords(1, i), coords(2, i), coords(3, i), formt)
       elseif (stylei .eq. 'xFrac') then
          call CMLATXF9DP(xf, coords(1, i), coords(2, i), coords(3, i), formt)
       elseif (stylei .eq. 'xyz3') then
          call CMLATXYZ39DP(xf, coords(1, i), coords (2, i), coords(3, i), formt)
       elseif (stylei .eq. 'xyzFrac') then
          call CMLATXYZFRACT9DP(xf, coords(1, i), coords(2, i), coords(3, i), formt)
       endif
       call xml_EndElement(xf, 'atom')
    enddo

    call xml_EndElement(xf, 'atomArray')
    call xml_EndElement(xf, 'molecule')
    
  END SUBROUTINE cmlAddMoleculeDP

  
  ! -------------------------------------------------
  ! 2. writes complete SP molecule to xml channel
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddMoleculeSP(xf, natoms, elements, coords, style, id, title, dictref, fmt)
    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: natoms          ! number of atoms
    character(len=*), intent(in)           :: elements(*)     ! chemical element types
    real(kind=sp), intent(in)               :: coords(3, *)    ! atomic coordinates
    character(len=*), intent(in), optional :: id              ! id
    character(len=*), intent(in), optional :: title           ! the title
    character(len=*), intent(in), optional :: dictref         ! the dictionary reference
    character(len=*), intent(in), optional :: fmt             ! format for coords
    character(len=*), intent(in), optional :: style           ! type of coordinates ('x3'for Cartesians, 'xFrac' 
    ! for fractionals; ' ' = default => cartesians)
    ! Flush on entry and exit
    character(len=6) :: id1, id0
    integer          :: i
    character(len=10):: formt, stylei

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif
    if (present(style)) then
       stylei = style
    else
       stylei = 'x3'
    endif

    call stmAddStartTag(xf, 'molecule', id, title, dictref)
    call xml_NewElement(xf, 'atomArray')
    do i = 1, natoms
       write(id0, '(i4)') i
       id0 = adjustl(id0)
       id1 = 'a'
       id1(2:) = id0
       call cmlAddAtom(xf=xf, elem=elements(i), id=id1)
       if (stylei .eq. 'x3') then
          call CMLATX39SP(xf, coords(1, i), coords(2, i), coords(3, i), formt)
       elseif (stylei .eq. 'xFrac') then
          call CMLATXF9SP(xf, coords(1, i), coords(2, i), coords(3, i), formt)
       elseif (stylei .eq. 'xyz3') then
          call CMLATXYZ39SP(xf, coords(1, i), coords(2, i), coords(3, i), formt)
       elseif (stylei .eq. 'xyzFrac') then
          call CMLATXYZFRACT9SP(xf, coords(1, i), coords(2, i), coords(3, i), formt)
       endif
       call xml_EndElement(xf, 'atom')
    enddo

    call xml_EndElement(xf, 'atomArray')
    call xml_EndElement(xf, 'molecule')
    
    
  END SUBROUTINE cmlAddMoleculeSP
  
  
  ! -------------------------------------------------
  ! 1. writes complete DP molecule to xml channel (No. 2)
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddMolecule3DP(xf, natoms, elements, x, y, z, style, id, title, dictref, fmt)
    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)                    :: natoms          ! number of atoms
    real(kind=dp), intent(in)               :: x(*)
    real(kind=dp), intent(in)               :: y(*)
    real(kind=dp), intent(in)               :: z(*)
    character(len=*), intent(in)           :: elements(*)     ! chemical element types
    character(len=*), intent(in), optional :: id              ! id
    character(len=*), intent(in), optional :: title           ! the title
    character(len=*), intent(in), optional :: dictref         ! the dictionary reference
    character(len=*), intent(in), optional :: fmt             ! format for coords
    character(len=*), intent(in), optional :: style           ! type of coordinates ('x3'for Cartesians, 'xFrac' 
    ! for fractionals; ' ' = default => cartesians)
    character(len=6)  :: id1, id0
    integer           :: i, l
    character(len=10) :: formt, stylei

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif
    if (present(style)) then
       stylei = trim(style)
    else
       stylei = 'x3'
    endif

    call stmAddStartTag(xf=xf, name='molecule', id=id, title=title, dictref=dictref)
    call xml_NewElement(xf, 'atomArray')

    do i = 1, natoms
       write(id0, '(i4)') i
       id0 = adjustl(id0)
       id1 = 'a'
       id1(2:) = id0
       call cmlAddAtom(xf=xf, elem=elements(i), id=id1)
       if (trim(stylei) .eq. 'x3') then
          call CMLATX39DP(xf, x(i), y(i), z(i), formt)
       elseif (stylei .eq. 'xFrac') then
          call CMLATXF9DP(xf, x(i), y(i), z(i), formt)
       elseif (stylei .eq. 'xyz3') then
          call CMLATXYZ39DP(xf, x(i), y(i), z(i), formt)
       elseif (stylei .eq. 'xyzFrac') then
          call CMLATXYZFRACT9DP(xf, x(i), y(i), z(i), formt)
       endif
       call xml_EndElement(xf, 'atom')
    enddo

    call xml_EndElement(xf, 'atomArray')
    call xml_EndElement(xf, 'molecule')
    
  END SUBROUTINE cmlAddMolecule3DP
  
  
  ! -------------------------------------------------
  ! 2. writes complete SP molecule to xml channel (No. 2)
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddMolecule3SP(xf, natoms, elements, x, y, z, style, id, title, dictref, fmt)


    implicit none
    type(xmlf_t) :: xf
    ! 10 Arguments
    integer, intent(in)                    :: natoms          ! number of atoms
    real(kind=sp), intent(in)               :: x(*)
    real(kind=sp), intent(in)               :: y(*)
    real(kind=sp), intent(in)               :: z(*)
    character(len=*), intent(in)           :: elements(*)      ! chemical element types
    character(len=*), intent(in), optional :: id               ! id
    character(len=*), intent(in), optional :: title            ! the title
    character(len=*), intent(in), optional :: dictref          ! the dictionary reference
    character(len=*), intent(in), optional :: fmt              ! format for coords
    character(len=*), intent(in), optional :: style            ! type of coordinates ('x3' for Cartesians, 'xFrac' 
    ! for fractionals; ' ' = default => cartesians)
    ! Internal variables
    character(len=6)  :: id1, id0
    integer           :: i, l
    character(len=10) :: formt, stylei

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif
    if (present(style)) then
       stylei = style
    else
       stylei = 'x3'
    endif

    call xml_NewElement(xf, 'molecule')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'title', title)
    call xml_AddAttribute(xf, 'dictref', dictref)
    call xml_NewElement(xf, 'atomArray')
    do i = 1, natoms
       write(id0, '(i4)') i
       id0 = adjustl(id0)
       id1 = 'a'
       id1(2:) = id0
       call cmlAddAtom(xf=xf, elem=elements(i), id=id1)
       if (stylei .eq. 'x3') then
          call CMLATX39SP(xf, x(i), y(i), z(i), formt)
       else if (stylei .eq. 'xFrac') then
          call CMLATXF9SP(xf, x(i), y(i), z(i), formt)
       else if (stylei .eq. 'xyz3') then
          call CMLATXYZ39SP(xf, x(i), y(i), z(i), formt)
       else if (stylei .eq. 'xyzFrac') then
          call CMLATXYZFRACT9SP(xf, x(i), y(i), z(i), formt)
       endif
           call xml_EndElement(xf, 'atom')
    enddo

    call xml_EndElement(xf, 'atomArray')
    call xml_EndElement(xf, 'molecule')

  END SUBROUTINE cmlAddMolecule3SP
  
  ! -------------------------------------------------
  ! writes an <atom> start tag
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddAtom(xf, elem, id, charge, hCount, occupancy, fmt)


    implicit none
    type(xmlf_t) :: xf
    integer, intent(in), optional           :: charge     ! formalCharge
    integer, intent(in), optional           :: hCount     ! hydrogenCount
    real(kind=sp), intent(in), optional      :: occupancy  ! hydrogenCount
    character(len=*), intent(in), optional  :: elem       ! chemical element name
    character(len=*), intent(in), optional  :: id         ! atom id
    character(len=*), intent(in), optional  :: fmt        ! format

    ! internal Variable
    character(len=10):: formt
    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    call xml_NewElement(xf, 'atom')
    if (present(elem))      call xml_AddAttribute(xf, 'elementType', elem)
    if (present(id))        call xml_AddAttribute(xf, 'id', id)
    if (present(charge))    call xml_AddAttribute(xf, 'formalCharge', str(charge))
    if (present(hCount))    call xml_AddAttribute(xf, 'hydrogenCount', str(hCount))
    if (present(occupancy)) call xml_AddAttribute(xf, 'occupancy', str(occupancy,formt))

  END SUBROUTINE cmlAddAtom
  
  
  ! -------------------------------------------------
  ! 1. append SP coordinates to atom tag
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddCoordinatesSP(xf, x, y, z, style, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in)               :: x, y
    real(kind=sp), intent(in), optional     :: z
    character(len=*), intent(in), optional :: style
    character(len=*), intent(in), optional :: fmt

    ! Internal variable
    character(len=10):: formt
    character(len=10):: stylei
    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif
    if (present(style)) then
       stylei = style
    else
       stylei = 'x3'
    endif

    if (present(z) .and. stylei .eq. 'x3') then
       call CMLATX39SP(xf, x, y, z, formt)
    else if (present(z) .and. stylei .eq. 'xFrac') then
       call CMLATXF9SP(xf, x, y, z, formt)
    else if (present(z) .and. stylei .eq. 'xyz3') then
       call CMLATXYZ39SP(xf, x, y, z, formt)
    else if (present(z) .and. stylei .eq. 'xyzFrac') then
       call CMLATXYZFRACT9SP(xf, x, y, z, formt)
    elseif (.not. present(z) .and. stylei .eq. 'xy2') then
       call CMLATXY9SP(xf, x, y, formt)           
    endif

  END SUBROUTINE cmlAddCoordinatesSP

  ! -------------------------------------------------
  ! 2. append DP coordinates to atom tag
  ! -------------------------------------------------

  SUBROUTINE cmlAddCoordinatesDP(xf, x, y, z, style, fmt)
    implicit none
    type(xmlf_t) :: xf 
    real(kind=dp), intent(in)               :: x, y
    real(kind=dp), intent(in), optional     :: z
    character(len=*), intent(in), optional :: style
    character(len=*), intent(in), optional :: fmt
    
    ! Internal variable
    character(len=10):: formt
    character(len=10):: stylei
    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif
    if (present(style)) then
       stylei = style
    else
       stylei = 'x3'
    endif
    
    if (present(z) .and. stylei .eq. 'x3') then
       call CMLATX39DP(xf, x, y, z, formt)
    else if (present(z) .and. stylei .eq. 'xFrac') then
       call CMLATXF9DP(xf, x, y, z, formt)
    else if (present(z) .and. stylei .eq. 'xyz3') then
       call CMLATXYZ39DP(xf, x, y, z, formt)
    else if (present(z) .and. stylei .eq. 'xyzFrac') then
       call CMLATXYZFRACT9DP(xf, x, y, z, formt)
    else if (.not. present(z) .and. stylei .eq. 'xy2') then
       call CMLATXY9DP(xf, x, y, formt)           
    endif
    
  END SUBROUTINE cmlAddCoordinatesDP

  
  ! -------------------------------------------------
  ! 1. writes a DP <length> element to output channel
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddLengthDP(xf, length, id, atomRef1, atomRef2, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=dp), intent(in)     :: length     ! length
    character(len=*), intent(in) :: id         ! length id
    character(len=*), intent(in) :: atomRef1   ! ref to first atom
    character(len=*), intent(in) :: atomRef2   ! ref to second atom
    character(len=*), intent(in) :: fmt        ! format

    optional         :: fmt
    character(len=10):: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! Flush on entry and exit
    call CMLLEN9DP(xf, id, atomRef1, atomRef2, length, formt)
  END SUBROUTINE cmlAddLengthDP

  ! -------------------------------------------------
  ! 2. writes a SP <length> element to output channel
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddLengthSP(xf, length, id, atomRef1, atomRef2, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in)     :: length     ! the length
    character(len=*), intent(in) :: id         ! length id
    character(len=*), intent(in) :: atomRef1   ! ref to first atom
    character(len=*), intent(in) :: atomRef2   ! ref to second atom
    character(len=*), intent(in) :: fmt        ! format

    optional         :: fmt
    character(len=10):: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! Flush on entry and exit
    call CMLLEN9SP(xf, id, atomRef1, atomRef2, length, formt)
  END SUBROUTINE cmlAddLengthSP


  ! -------------------------------------------------
  ! 1. writes an DP <angle> element to output channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddAngleDP(xf, angle, id, atomRef1, atomRef2, atomRef3, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=dp), intent(in)     :: angle        ! the angle
    character(len=*), intent(in) :: id           ! angle id
    character(len=*), intent(in) :: atomRef1     ! ref to first atom
    character(len=*), intent(in) :: atomRef2     ! ref to second atom
    character(len=*), intent(in) :: atomRef3     ! ref to third atom
    character(len=*), intent(in) :: fmt          ! format

    optional         :: fmt
    character(len=10):: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! Flush on entry and exit
    call CMLANG9DP(xf, id, atomRef1, atomRef2, atomRef3, angle, formt)
  END SUBROUTINE cmlAddAngleDP

  ! -------------------------------------------------
  ! 2. writes an SP <angle> element to output channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddAngleSP(xf, angle, id, atomRef1, atomRef2, atomRef3, fmt)


    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in)     :: angle        ! the angle
    character(len=*), intent(in) :: id           ! angle id
    character(len=*), intent(in) :: atomRef1     ! ref to first atom
    character(len=*), intent(in) :: atomRef2     ! ref to second atom
    character(len=*), intent(in) :: atomRef3     ! ref to third atom
    character(len=*), intent(in) :: fmt          ! format

    optional         :: fmt
    character(len=10):: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! Flush on entry and exit
    call CMLANG9SP(xf, id, atomRef1, atomRef2, atomRef3, angle, formt)
  END SUBROUTINE cmlAddAngleSP


  ! -------------------------------------------------
  ! 1. creates and writes a DP <torsion> element
  ! -------------------------------------------------

  SUBROUTINE cmlAddTorsionDP(xf, torsion, id, atomRef1, atomRef2, atomRef3, atomRef4, fmt)


    implicit none
    type(xmlf_t) :: xf
    real(kind=dp), intent(in)     :: torsion         ! the torsion
    character(len=*), intent(in) :: id              ! torsion id
    character(len=*), intent(in) :: atomRef1        ! ref to first atom
    character(len=*), intent(in) :: atomRef2        ! ref to second atom
    character(len=*), intent(in) :: atomRef3        ! ref to third atom
    character(len=*), intent(in) :: atomRef4        ! ref to fourth atom
    character(len=*), intent(in) :: fmt             ! format

    optional         :: fmt
    character(len=10):: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! Flush on entry and exit
    call CMLTOR9DP(xf, id, atomRef1, atomRef2, atomRef3, atomRef4, torsion, formt)
  END SUBROUTINE cmlAddTorsionDP
  
  ! -------------------------------------------------
  ! 2. creates and writes a SP <torsion> element
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddTorsionSP(xf, torsion, id, atomRef1, atomRef2, atomRef3, atomRef4, fmt)


    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in)     :: torsion         ! the torsion
    character(len=*), intent(in) :: id              ! torsion id
    character(len=*), intent(in) :: atomRef1        ! ref to first atom
    character(len=*), intent(in) :: atomRef2        ! ref to second atom
    character(len=*), intent(in) :: atomRef3        ! ref to third atom
    character(len=*), intent(in) :: atomRef4        ! ref to fourth atom
    character(len=*), intent(in) :: fmt             ! format

    optional         :: fmt
    character(len=10):: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! Flush on entry and exit
    call CMLTOR9SP(xf, id, atomRef1, atomRef2, atomRef3, atomRef4, torsion, formt)
  END SUBROUTINE cmlAddTorsionSP


  ! -------------------------------------------------
  ! 1. creates and writes an SP Lattice element
  ! -------------------------------------------------

  SUBROUTINE cmlAddLatticeSP(xf, cell, units, title, id, dictref, conv, lattType, spaceType, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in)               :: cell(3,3)
    character(len=*), intent(in), optional :: units       
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: conv         ! format
    character(len=*), intent(in), optional :: lattType     ! 
    character(len=*), intent(in), optional :: spaceType    !
    character(len=*), intent(in), optional :: fmt         

    ! Internal Variables
    integer           :: i
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'   
    endif

    call xml_NewElement(xf, 'lattice')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(lattType)) call xml_AddAttribute(xf, 'latticeType', lattType)
    if (present(spaceType)) call xml_AddAttribute(xf, 'spaceType', spaceType)

    do i = 1,3
       call xml_NewElement(xf, 'latticeVector')
       if (present(units)) call xml_AddAttribute(xf, 'units', units)
       call xml_AddAttribute(xf, 'dictRef', 'cml:latticeVector')
       call xml_AddPcdata(xf, str(cell(1,i), formt))
       call xml_AddPcdata(xf, str(cell(2,i), formt))
       call xml_AddPcdata(xf, str(cell(3,i), formt))
    call xml_EndElement(xf, 'latticeVector')
    enddo
    call xml_EndElement(xf, 'lattice')

  END SUBROUTINE cmlAddLatticeSP


  ! -------------------------------------------------
  ! 2. creates and writes DP Lattice element
  ! -------------------------------------------------

  SUBROUTINE cmlAddLatticeDP(xf, cell, units, title, id, dictref, conv, lattType, spaceType, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=dp), intent(in)               :: cell(3,3)
    character(len=*), intent(in), optional :: units       
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: conv         ! format
    character(len=*), intent(in), optional :: lattType     ! 
    character(len=*), intent(in), optional :: spaceType    !
    character(len=*), intent(in), optional :: fmt         

    ! Internal Variables
    integer           :: i
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'   
    endif

    call xml_NewElement(xf, 'lattice')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(lattType)) call xml_AddAttribute(xf, 'latticeType', lattType)
    if (present(spaceType)) call xml_AddAttribute(xf, 'spaceType', spaceType)

    do i = 1,3
       call xml_NewElement(xf, 'latticeVector')
       if (present(units)) call xml_AddAttribute(xf, 'units', units)
       call xml_AddAttribute(xf, 'dictRef', 'cml:latticeVector')
       call xml_AddPcdata(xf, str(cell(1,i), formt))
       call xml_AddPcdata(xf, str(cell(2,i), formt))
       call xml_AddPcdata(xf, str(cell(3,i), formt))
       call xml_EndElement(xf, 'latticeVector')
    enddo
    
    call xml_EndElement(xf, 'lattice')

  END SUBROUTINE cmlAddLatticeDP


  ! -------------------------------------------------
  ! 1. creates a DP Lattice Vector element
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddLatticeVectorDP(xf, vector, title, id, dictref, conv, units, periodic, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=dp), intent(in) :: vector(3)
    character(len=*), intent(in), optional :: title        
    character(len=*), intent(in), optional :: id           
    character(len=*), intent(in), optional :: dictref     
    character(len=*), intent(in), optional :: conv        
    character(len=*), intent(in), optional :: units       
    character(len=*), intent(in), optional :: periodic    
    character(len=*), intent(in), optional :: fmt         

    ! Deal with optional things
    ! that have defaults
    character(len=10) :: formt
    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'   
    endif

    call xml_NewElement(xf, 'latticeVector')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(units)) call xml_AddAttribute(xf, 'units', units)
    if (present(periodic)) call xml_AddAttribute(xf, 'periodic', periodic)
    call xml_AddPcdata(xf, str(vector(1), formt))
    call xml_AddPcdata(xf, str(vector(2), formt))
    call xml_AddPcdata(xf, str(vector(3), formt))
    call xml_EndElement(xf, 'latticeVector')

  END SUBROUTINE cmlAddLatticeVectorDP


  ! -------------------------------------------------
  ! 2. creates a SP Lattice Vector element
  ! -------------------------------------------------

  SUBROUTINE cmlAddLatticeVectorSP(xf, vector, title, id, dictref, conv, units, periodic, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in) :: vector(3)
    character(len=*), intent(in), optional :: title        
    character(len=*), intent(in), optional :: id           
    character(len=*), intent(in), optional :: dictref     
    character(len=*), intent(in), optional :: conv        
    character(len=*), intent(in), optional :: units       ! should this be optional
    character(len=*), intent(in), optional :: periodic    
    character(len=*), intent(in), optional :: fmt         

    ! Deal with optional things
    ! that have defaults
    character(len=10) :: formt
    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'   
    endif

    call xml_NewElement(xf, 'latticeVector')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(units)) call xml_AddAttribute(xf, 'units', units)
    if (present(units)) call xml_AddAttribute(xf, 'periodic', periodic)
    call xml_AddPcdata(xf, str(vector(1), formt))
    call xml_AddPcdata(xf, str(vector(2), formt))
    call xml_AddPcdata(xf, str(vector(3), formt))
    call xml_EndElement(xf, 'latticeVector')

  END SUBROUTINE cmlAddLatticeVectorSP

  ! -------------------------------------------------
  ! 1. creates and writes a DP <cell> element
  ! -------------------------------------------------

  SUBROUTINE cmlAddCrystalDP(xf, a, b, c, alpha, beta, gamma, id, title, dictref, lenunits, angunits, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=dp), intent(in)               :: a, b, c      ! cell parameters
    real(kind=dp), intent(in)               :: alpha        ! alpha cell parameter
    real(kind=dp), intent(in)               :: beta         ! beta cell parameter
    real(kind=dp), intent(in)               :: gamma        ! gamma cell parameter
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: lenunits     ! units for length (default = angstrom)
    character(len=*), intent(in), optional :: angunits     ! units for angles (default = degree)
    character(len=*), intent(in), optional :: fmt          ! format

    ! Flush on entry and exit
    character(len=30) ::  lunits, aunits
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif
    if (present(lenunits)) then
       lunits = lenunits
    else
       lunits = 'units:angstrom'
    endif
    if (present(angunits)) then
       aunits = angunits
    else
       aunits = 'units:degree'
    endif

    call stmAddStartTag(xf=xf, name='crystal', id=id, title=title, dictref=dictref)
    call stmAddScalar(xf=xf, value=a, title='a', dictref='cml:a', units=lunits, fmt=formt)
    call stmAddScalar(xf=xf, value=b, title='b', dictref='cml:b', units=lunits, fmt=formt)
    call stmAddScalar(xf=xf, value=c, title='c', dictref='cml:c', units=lunits, fmt=formt)
    call stmAddScalar(xf=xf, value=alpha, title='alpha', dictref='cml:alpha', units=aunits, fmt=formt)
    call stmAddScalar(xf=xf, value=beta,  title='beta',  dictref='cml:beta',  units=aunits, fmt=formt)
    call stmAddScalar(xf=xf, value=gamma, title='gamma', dictref='cml:gamma', units=aunits, fmt=formt)
    call xml_EndElement(xf, 'crystal')

  END SUBROUTINE cmlAddCrystalDP

  ! -------------------------------------------------
  ! 2. creates and writes a SP <cell> element
  ! -------------------------------------------------

  SUBROUTINE cmlAddCrystalSP(xf, a, b, c, alpha, beta, gamma, id, title, dictref, lenunits, angunits, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in)     :: a, b, c      ! cell parameters
    real(kind=sp), intent(in)     :: alpha        ! alpha cell parameter
    real(kind=sp), intent(in)     :: beta         ! beta cell parameter
    real(kind=sp), intent(in)     :: gamma        ! gamma cell parameter
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: lenunits     ! units for length (' ' = angstrom)
    character(len=*), intent(in), optional :: angunits     ! units for angles (' ' = degree)
    character(len=*), intent(in), optional :: fmt          ! format

    ! Flush on entry and exit
    character(len=30) :: lunits, aunits
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif
    if (present(lenunits)) then
       lunits = lenunits
    else
       lunits = U_ANGSTR
    endif
    if (present(angunits)) then
       aunits = angunits
    else
       aunits = U_DEGREE
    endif

    call stmAddStartTag(xf, 'crystal', id, title, dictref)
    call stmAddScalar(xf, a, ' ', 'a', 'cml:a', lunits, formt)
    call stmAddScalar(xf, b, ' ', 'b', 'cml:b', lunits, formt)
    call stmAddScalar(xf, c, ' ', 'c', 'cml:c', lunits, formt)
    call stmAddScalar(xf, alpha, ' ', 'alpha', 'cml:alpha', aunits, formt)
    call stmAddScalar(xf, beta, ' ', 'beta', 'cml:beta', aunits, formt)
    call stmAddScalar(xf, gamma, ' ', 'gamma', 'cml:gamma', aunits, formt)
    call xml_EndElement(xf, 'crystal')

  END SUBROUTINE cmlAddCrystalSP
  
  
  ! -------------------------------------------------
  ! 1. creates and writes an DP <eigen> element
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddEigenvalueDP(xf, n, dim, eigvec, eigval, id, title, dictref, fmt)


    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)          :: n              ! number of elements
    integer, intent(in)          :: dim            ! dimension of matrix
    real(kind=dp), intent(in)     :: eigvec(dim, *) ! eigenvectors
    real(kind=dp), intent(in)     :: eigval(*)      ! eigenvalues
    character(len=*), intent(in), optional :: id             ! id
    character(len=*), intent(in), optional :: title          ! title
    character(len=*), intent(in), optional :: dictref        ! dictionary reference
    character(len=*), intent(in), optional :: fmt            ! format
    character(len=10):: formt
    integer ::  i, j

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! Flush on entry and exit
    call xml_NewElement(xf, 'eigen')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(dictref)) call xml_AddAttribute(xf, 'title', title)
    call stmAddArray(xf=xf, nvalue=n, array=eigval, title='eigenvalues', dictref=dictRef, fmt=fmt)
    call stmAddMatrix(xf=xf, ncols=n, nrows=n, dim=dim, matrix=eigvec, title='eigenvectors', fmt=fmt)
    call xml_EndElement(xf, 'eigen')

  END SUBROUTINE cmlAddEigenvalueDP



  ! -------------------------------------------------
  ! 2. creates and writes an SP <eigen> element
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddEigenvalueSP(xf, n, dim, eigvec, eigval, id, title, dictref, fmt)


    implicit none
    type(xmlf_t) :: xf
    integer, intent(in)          :: n              ! number of elements
    integer, intent(in)          :: dim            ! dimension of matrix
    real(kind=sp), intent(in)     :: eigvec(dim, *) ! eigenvectors
    real(kind=sp), intent(in)     :: eigval(*)      ! eigenvalues
    character(len=*), intent(in), optional :: id             ! id
    character(len=*), intent(in), optional :: title          ! title
    character(len=*), intent(in), optional :: dictref        ! dictionary reference
    character(len=*), intent(in), optional :: fmt            ! format
    character(len=10):: formt
    integer ::  i, j

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    ! Flush on entry and exit
    call xml_NewElement(xf, 'eigen')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(dictref)) call xml_AddAttribute(xf, 'title', title)
    call stmAddArray(xf=xf, nvalue=n, array=eigval, title='eigenvalues', dictref=dictRef, fmt=fmt)
    call stmAddMatrix(xf=xf, ncols=n, nrows=n, dim=dim, matrix=eigvec, title='eigenvectors', fmt=fmt)
    call xml_EndElement(xf, 'eigen')

  END SUBROUTINE cmlAddEigenvalueSP


  SUBROUTINE cmlAddMetadata(xf, name, content, conv)
    
    implicit none
    type(xmlf_t) :: xf
    character(len=*) :: name
    character(len=*) :: content
    character(len=*), optional :: conv
    
    call xml_NewElement(xf, 'metadata')
    call xml_AddAttribute(xf, 'name', name)
    call xml_AddAttribute(xf, 'content', content)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    call xml_EndElement(xf, 'metadata')

  END SUBROUTINE cmlAddMetadata


  ! -------------------------------------------------
  ! 1. creates and writes an Char <parameter> element
  ! -------------------------------------------------


  SUBROUTINE cmlAddParameterCh(xf, value, ref, id, title, conv, &
       cons, units, name, role)

    implicit none
    type(xmlf_t) :: xf
    character(len=*) :: value 
    character(len=*), optional :: ref 
    character(len=*), optional :: title
    character(len=*), optional :: id
    character(len=*), optional :: conv
    character(len=*), optional :: cons
    character(len=*), optional :: units
    character(len=*), optional :: name
    character(len=*), optional :: role

    call xml_NewElement(xf, 'parameter')
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(cons)) call xml_AddAttribute(xf, 'constraint', cons)
    if (present(name)) call xml_AddAttribute(xf, 'name', name)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    if (present(units)) then
       call xml_NewElement(xf, 'scalar')
       call xml_AddAttribute(xf, 'units', units)
       call xml_AddPcdata(xf, value)
       call xml_EndElement(xf, 'scalar')
    else
       call xml_AddAttribute(xf, 'value', value)
    endif
    call xml_EndElement(xf, 'parameter')

  END SUBROUTINE CMLADDPARAMETERCH


  ! -------------------------------------------------
  ! 2. creates and writes an SP <parameter> element
  ! -------------------------------------------------


  SUBROUTINE cmlAddParameterSP(xf, value, ref, title, id, conv, &
       cons, units, name, role, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=sp) :: value 
    character(len=*), optional :: ref 
    character(len=*), optional :: title
    character(len=*), optional :: id
    character(len=*), optional :: conv
    character(len=*), optional :: cons
    character(len=*), optional :: units
    character(len=*), optional :: name
    character(len=*), optional :: role
    character(len=*), optional :: fmt    

    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    call xml_NewElement(xf, 'parameter')
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(cons)) call xml_AddAttribute(xf, 'constraint', cons)
    if (present(name)) call xml_AddAttribute(xf, 'name', name)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    if (present(units)) then
       call xml_NewElement(xf, 'scalar')
       call xml_AddAttribute(xf, 'units', units)
       call xml_AddPcdata(xf, str(value))
       call xml_EndElement(xf, 'scalar')
    else
       call xml_AddAttribute(xf, 'value', str(value,formt))
    endif
    call xml_EndElement(xf, 'parameter')

  END SUBROUTINE CMLADDPARAMETERSP


  ! -------------------------------------------------
  ! 3. creates and writes an DP <parameter> element
  ! -------------------------------------------------


  SUBROUTINE cmlAddParameterDP(xf, value, ref, title, id, conv, &
       cons, units, name, role, fmt)

    implicit none
    type(xmlf_t) :: xf
    real(kind=dp) :: value 
    character(len=*), optional :: ref 
    character(len=*), optional :: title
    character(len=*), optional :: id
    character(len=*), optional :: conv
    character(len=*), optional :: cons
    character(len=*), optional :: units
    character(len=*), optional :: name
    character(len=*), optional :: role
    character(len=*), optional :: fmt    

    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = '(f8.3)'
    endif

    call xml_NewElement(xf, 'parameter')
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(cons)) call xml_AddAttribute(xf, 'constraint', cons)
    if (present(name)) call xml_AddAttribute(xf, 'name', name)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    if (present(units)) then
       call xml_NewElement(xf, 'scalar')
       call xml_AddAttribute(xf, 'units', units)
       call xml_AddPcdata(xf, str(value))
       call xml_EndElement(xf, 'scalar')
    else
       call xml_AddAttribute(xf, 'value', str(value))
    endif
    call xml_EndElement(xf, 'parameter')

  END SUBROUTINE CMLADDPARAMETERDP


  ! -------------------------------------------------
  ! 4. creates and writes an Integer <parameter> element
  ! -------------------------------------------------


  SUBROUTINE cmlAddParameterI(xf, value, ref, id, title, conv, &
       cons, units, name, role)

    implicit none
    type(xmlf_t) :: xf
    integer :: value 
    character(len=*), optional :: ref 
    character(len=*), optional :: title
    character(len=*), optional :: id
    character(len=*), optional :: conv
    character(len=*), optional :: cons
    character(len=*), optional :: units
    character(len=*), optional :: name
    character(len=*), optional :: role

    call xml_NewElement(xf, 'parameter')
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(cons)) call xml_AddAttribute(xf, 'constraint', cons)
    if (present(name)) call xml_AddAttribute(xf, 'name', name)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    if (present(units)) then
       call xml_NewElement(xf, 'scalar')
       call xml_AddAttribute(xf, 'units', units)
       call xml_AddPcdata(xf, str(value))
       call xml_EndElement(xf, 'scalar')
    else
       call xml_AddAttribute(xf, 'value', str(value))
    endif
    call xml_EndElement(xf, 'parameter')

  END SUBROUTINE CMLADDPARAMETERI

  SUBROUTINE cmlAddParameterLG(xf, value, ref, id, title, conv, &
       cons, units, name, role)

    implicit none
    type(xmlf_t) :: xf
    logical      :: value 
    character(len=*), optional :: ref 
    character(len=*), optional :: title
    character(len=*), optional :: id
    character(len=*), optional :: conv
    character(len=*), optional :: cons
    character(len=*), optional :: units
    character(len=*), optional :: name
    character(len=*), optional :: role

    call xml_NewElement(xf, 'parameter')
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(cons)) call xml_AddAttribute(xf, 'constraint', cons)
    if (present(name)) call xml_AddAttribute(xf, 'name', name)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    if (present(units)) then
       call xml_NewElement(xf, 'scalar')
       call xml_AddAttribute(xf, 'units', units)
       call xml_AddPcdata(xf, str(value))
       call xml_EndElement(xf, 'scalar')
    else
       call xml_AddAttribute(xf, 'value', str(value))
    endif
    call xml_EndElement(xf, 'parameter')

  END SUBROUTINE CMLADDPARAMETERLG



! =================================================
! basic CML routines
! =================================================

  
  ! -------------------------------------------------
  ! 1. adds DP xyz3 to start tag
  ! -------------------------------------------------
  
  SUBROUTINE CMLATXYZ39DP(xf, x3, y3, z3, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=dp)      :: x3, y3, z3 ! coordinates
    character(len=*)  :: fmt        ! format (default '(f8.3)')
    character(len=45) :: x, y, z

    write(x,fmt) x3
    write(y,fmt) y3
    write(z,fmt) z3

    call xml_AddAttribute(xf, 'xyz3', trim(x)//' '//trim(adjustl(y))//' '//trim(adjustl(z)) )

  END SUBROUTINE CMLATXYZ39DP


  ! -------------------------------------------------
  ! 2. adds SP xyz3 to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATXYZ39SP(xf, x3, y3, z3, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=sp)      :: x3, y3, z3 ! coordinates
    character(len=*)  :: fmt        ! format (default '(f8.3)')

    character(len=45) :: x, y, z

    write(x,fmt) x3
    write(y,fmt) y3
    write(z,fmt) z3

    call xml_AddAttribute(xf, 'xyz3', trim(x)//' '//trim(adjustl(y))//' '//trim(adjustl(z)) )

  END SUBROUTINE CMLATXYZ39SP
  
  ! -------------------------------------------------
  ! 1. adds DP xyzFrac to start tag
  ! -------------------------------------------------
  
  SUBROUTINE CMLATXYZFRACT9DP(xf, x3, y3, z3, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=dp)      :: x3, y3, z3 ! coordinates
    character(len=*)  :: fmt        ! format (default '(f8.3)')

    character(len=45) :: x, y, z

    write(x,fmt) x3
    write(y,fmt) y3
    write(z,fmt) z3

    call xml_AddAttribute(xf, 'xyzFrac', trim(x)//' '//trim(adjustl(y))//' '//trim(adjustl(z)) )

  END SUBROUTINE CMLATXYZFRACT9DP

  ! -------------------------------------------------
  ! 2. adds SP xyzFrac to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATXYZFRACT9SP(xf, x3, y3, z3, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in)     :: x3, y3, z3 ! coordinates
    character(len=*), intent(in) :: fmt        ! format (default '(f8.3)')

    character(len=45) :: x, y, z

    write(x,fmt) x3
    write(y,fmt) y3
    write(z,fmt) z3

    call xml_AddAttribute(xf, 'xyzFrac', trim(x)//' '//trim(y)//' '//trim(z))

  END SUBROUTINE CMLATXYZFRACT9SP


  ! -------------------------------------------------
  ! 1. adds DP x3, y3, z3 to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATX39DP(xf, x3, y3, z3, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=dp), intent(in)     :: x3, y3, z3 ! coordinates
    character(len=*), intent(in) :: fmt        ! format (default '(f8.3)')

    call xml_AddAttribute(xf, 'x3', str(x3, fmt))
    call xml_AddAttribute(xf, 'y3', str(y3, fmt))
    call xml_AddAttribute(xf, 'z3', str(z3, fmt))

  END SUBROUTINE CMLATX39DP

  ! -------------------------------------------------
  ! 2. adds SP x3, y3, z3 to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATX39SP(xf, x3, y3, z3, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=sp), intent(in)     :: x3, y3, z3 ! coordinates
    character(len=*), intent(in) :: fmt        ! format (default '(f8.3)')

    call xml_AddAttribute(xf, 'x3', str(x3, fmt))
    call xml_AddAttribute(xf, 'y3', str(y3, fmt))
    call xml_AddAttribute(xf, 'z3', str(z3, fmt))

  END SUBROUTINE CMLATX39SP


  ! -------------------------------------------------
  ! 1. adds DP xFract, yFract, zFract to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATXF9DP(xf, xFract, yFract, zFract, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=dp), intent(in)     :: xFract, yFract, zFract ! coordinates
    character(len=*), intent(in) :: fmt                    ! format (default '(f8.3)')

    call xml_AddAttribute(xf, 'xFract', str(xFract, fmt))
    call xml_AddAttribute(xf, 'yFract', str(yFract, fmt))
    call xml_AddAttribute(xf, 'zFract', str(zFract, fmt))

  END SUBROUTINE CMLATXF9DP
  
  ! -------------------------------------------------
  ! 2. adds SP xfrac, yFractractrac, zFractrac to start tag
  ! -------------------------------------------------
  
  SUBROUTINE CMLATXF9SP(xf, xFract, yFract, zFract, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=sp)     :: xFract, yFract, zFract   ! fractional coordinates
    character(len=*) :: fmt                      ! format (default '(f8.3)')

    call xml_AddAttribute(xf, 'xFract', str(xFract, fmt))
    call xml_AddAttribute(xf, 'yFract', str(yFract, fmt))
    call xml_AddAttribute(xf, 'zFract', str(zFract, fmt))

  END SUBROUTINE CMLATXF9SP


  ! -------------------------------------------------
  ! 1. adds DP x2, y2 to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATXY9DP(xf, x2, y2, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=dp)     :: x2, y2   ! coordinates
    character(len=*) :: fmt      ! format (default f8.3)

    call xml_AddAttribute(xf, 'x2', str(x2, fmt))
    call xml_AddAttribute(xf, 'y2', str(y2, fmt))
    call xml_AddPcdata(xf, '>')                   !!! AG****

  END SUBROUTINE CMLATXY9DP

  ! -------------------------------------------------
  ! 2. adds SP x2, y2 to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATXY9SP(xf, x2, y2, fmt)
    implicit none
    type(xmlf_t) :: xf
    real(kind=sp)     :: x2, y2   ! coordinates
    character(len=*) :: fmt      ! format (default f8.3)

    call xml_AddAttribute(xf, 'x2', str(x2, fmt))
    call xml_AddAttribute(xf, 'y2', str(y2, fmt))
    call xml_AddPcdata(xf, '>')                      !!AG***

  END SUBROUTINE CMLATXY9SP


  ! -------------------------------------------------
  ! 1. creates a DP <length> element
  ! -------------------------------------------------

  SUBROUTINE CMLLEN9DP(xf, id, atomRef1, atomRef2, length, fmt)
    implicit none
    type(xmlf_t) :: xf
    character(len=*) :: id           ! length id
    character(len=*) :: atomRef1     ! ref to first atom
    character(len=*) :: atomRef2     ! ref to second atom
    real(kind=dp)     :: length       ! the length
    character(len=*) :: fmt          ! format
    character(len=20) :: temp

    temp = atomRef1//' '//adjustl(atomRef2)

    call xml_NewElement(xf, 'length')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs2', temp)
    call xml_AddPcdata(xf, str(length, fmt))
    call xml_EndElement(xf, 'length')

  END SUBROUTINE CMLLEN9DP
  
  ! -------------------------------------------------
  ! 2. creates a SP <length> element
  ! -------------------------------------------------
  
  SUBROUTINE CMLLEN9SP(xf, id, atomRef1, atomRef2, length, fmt)
    implicit none
    type(xmlf_t) :: xf
    character(len=*) :: id           ! length id
    character(len=*) :: atomRef1     ! ref to first atom
    character(len=*) :: atomRef2     ! ref to second atom
    real(kind=sp)     :: length       ! the length
    character(len=*) :: fmt          ! format
    character(len=20) :: temp

    temp = atomRef1//' '//adjustl(atomRef2)

    call xml_NewElement(xf, 'length')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs2', temp)
    call xml_AddPcdata(xf, str(length, fmt))
    call xml_EndElement(xf, 'length')

  END SUBROUTINE CMLLEN9SP


  ! -------------------------------------------------
  ! 1. creates a DP <angle> element
  ! -------------------------------------------------

  SUBROUTINE CMLANG9DP(xf, id, atomRef1, atomRef2, atomRef3, angle, fmt)
    implicit none
    type(xmlf_t) :: xf
    character(len=*) :: id              ! angle id
    character(len=*) :: atomRef1        ! ref to first atom
    character(len=*) :: atomRef2        ! ref to second atom
    character(len=*) :: atomRef3        ! ref to third atom
    real(kind=dp)     :: angle           ! the angle
    character(len=*) :: fmt             ! format
    character(len=20) :: temp

    temp = atomRef1//' '//adjustl(atomRef2)//' '//adjustl(atomRef3)

    call xml_NewElement(xf, 'angle')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs3', temp)
    call xml_AddPcdata(xf, str(angle, fmt))
    call xml_EndElement(xf, 'angle')

  END SUBROUTINE CMLANG9DP

  ! -------------------------------------------------
  ! 2. creates a SP <angle> element
  ! -------------------------------------------------

  SUBROUTINE CMLANG9SP(xf, id, atomRef1, atomRef2, atomRef3, angle, fmt)
    implicit none
    type(xmlf_t) :: xf
    character(len=*) :: id              ! angle id
    character(len=*) :: atomRef1        ! ref to first atom
    character(len=*) :: atomRef2        ! ref to second atom
    character(len=*) :: atomRef3        ! ref to third atom
    real(kind=sp)     :: angle           ! the angle
    character(len=*) :: fmt             ! format
    character(len=20) :: temp

    temp = atomRef1//' '//adjustl(atomRef2)//' '//adjustl(atomRef3)

    call xml_NewElement(xf, 'angle')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs3', temp)
    call xml_AddPcdata(xf, str(angle, fmt))
    call xml_EndElement(xf, 'angle')

  END SUBROUTINE CMLANG9SP


  ! -------------------------------------------------
  ! 1. creates a DP <torsion> element
  ! -------------------------------------------------
  
  SUBROUTINE CMLTOR9DP(xf, id, atomRef1, atomRef2, atomRef3, atomRef4, torsion, fmt)
    implicit none
    type(xmlf_t) :: xf
    character(len=*) :: id              ! torsion id
    character(len=*) :: atomRef1        ! ref to first atom
    character(len=*) :: atomRef2        ! ref to second atom
    character(len=*) :: atomRef3        ! ref to third atom
    character(len=*) :: atomRef4        ! ref to fourth atom
    real(kind=dp)     :: torsion         ! the torsion
    character(len=*) :: fmt             ! format
    character(len=20) :: temp

    temp = atomRef1//' '//adjustl(atomRef2)//' '//adjustl(atomRef3)//' '//adjustl(atomRef4)

    call xml_NewElement(xf, 'torsion')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs4', temp)
    call xml_AddPcdata(xf, str(torsion, fmt))
    call xml_EndElement(xf, 'torsion')

  END SUBROUTINE CMLTOR9DP

  ! -------------------------------------------------
  ! 2. creates a SP <torsion> element
  ! -------------------------------------------------

  SUBROUTINE CMLTOR9SP(xf, id, atomRef1, atomRef2, atomRef3, atomRef4, torsion, fmt)
    implicit none
    type(xmlf_t) :: xf
    character(len=*) :: id              ! torsion id
    character(len=*) :: atomRef1        ! ref to first atom
    character(len=*) :: atomRef2        ! ref to second atom
    character(len=*) :: atomRef3        ! ref to third atom
    character(len=*) :: atomRef4        ! ref to fourth atom
    real(kind=sp)     :: torsion         ! the torsion
    character(len=*) :: fmt             ! format
    character(len=20) :: temp

    temp = atomRef1//' '//adjustl(atomRef2)//' '//adjustl(atomRef3)//' '//adjustl(atomRef4)

    call xml_NewElement(xf, 'torsion')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs4', temp)
    call xml_AddPcdata(xf, str(torsion, fmt))
    call xml_EndElement(xf, 'torsion')

  END SUBROUTINE CMLTOR9SP

end module m_cmlw
