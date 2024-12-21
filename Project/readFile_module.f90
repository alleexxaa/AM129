! File: readFile_module.f90
! Purpose: module with subroutines to read variables in from a textfile at runtime
! Author: Alexandra Mendoza
! Notes: Adapted from Ian May's bvp.f90 code

module readFile_module
  use utility, only :fp, maxStrLen, maxFileLen
  implicit none
  
contains

 ! function: read_initFileInt
  ! purpose: Pull one integer value from an input file
  ! inputs: inFile -- String holding the name of the input file
  !         varName -- String that names the variable, this must be first entry on a line
  ! outputs: varValue -- Integer value that will hold the result from the input file

  function read_initFileInt(inFile,varName) result(varValue)

    implicit none
    character(len=*),intent(IN) :: inFile,varName
    integer :: varValue

    integer :: i,openStatus,inputStatus
    integer :: simInitVars
    character(len=maxStrLen) :: simCharVars
    integer :: pos1,pos2

    open(unit = 11, file=inFile, status='old',IOSTAT=openStatus,FORM='FORMATTED',ACTION='READ')

    do i=1,maxFileLen
      read(11, FMT = 101, IOSTAT=inputStatus) simCharVars
      pos1 = index(simCharVars,varName)
      pos2 = pos1+len_trim(varName)
      if (pos2 > len_trim(varName)) then
        read(simCharVars(pos2+1:),*)simInitVars
        varValue = simInitVars
      endif
    end do

    close(11)

  101 FORMAT(A, 1X, I5)

  end function read_initFileInt

  ! function: read_initFileReal
  ! purpose: Pull one real value from an input file
  ! inputs: inFile -- String holding the name of the input file
  !         varName -- String that names the variable, this must be first entry on a line
  ! outputs: varValue -- Real value that will hold the result from the input file
  function read_initFileReal(inFile,varName) result(varValue)

    implicit none
    character(len=*),intent(IN) :: inFile,varName
    real (fp) :: varValue

    integer :: i,openStatus,inputStatus
    real (fp) :: simInitVars
    character(len=maxStrLen) :: simCharVars
    integer :: pos1,pos2

    open(unit = 11, file=inFile, status='old',IOSTAT=openStatus,FORM='FORMATTED',ACTION='READ')

    do i=1,maxFileLen
      read(11, FMT = 101, IOSTAT=inputStatus) simCharVars
      pos1 = index(simCharVars,varName)
      pos2 = pos1+len_trim(varName)
      if (pos2 > len_trim(varName)) then
        read(simCharVars(pos2+1:),*)simInitVars
        varValue = simInitVars
      endif
    end do

    close(11)

  101 FORMAT(A, 1X, I5)

  end function read_initFileReal


  ! subroutine: read_initFileChar
  ! purpose: Pull one string with no spaces from an input file
  ! inputs: varName -- String that names the variable, this must be first entry on a line
  ! outputs: varValue -- String that will hold the result from the input file
  function read_initFileChar(inFile, varName) result(varValue)

    implicit none
    character(len=*),intent(IN) :: inFile
    character(len=*),intent(IN)  :: varName
    character(len=maxStrLen) :: varValue

    integer :: i,openStatus,inputStatus
    character(len=maxStrLen) :: simInitVars
    character(len=maxStrLen) :: simCharVars
    integer :: pos1,pos2

    open(unit = 13, file=inFile, status='old',IOSTAT=openStatus,FORM='FORMATTED',ACTION='READ',encoding='utf-8')

    do i=1,maxFileLen
      read(13, FMT = 103, IOSTAT=inputStatus) simCharVars
      pos1 = index(simCharVars,varName)
      pos2 = pos1+len_trim(varName)

      if (pos2 > len_trim(varName)) then
        read(simCharVars(pos2+1:),*)simInitVars
        varValue = simInitVars
      endif
    end do

    close(13)

103 FORMAT(A, 1X, A)

  end function read_initFileChar

end module readFile_module
! -----------------------------------------------------------------------------