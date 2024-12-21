! File: output_module.f90
! Author: Alexandra Mendoza
! Purpose: writes the solutions

module output_module
    use utility, only : fp, maxStrLen
    implicit none

contains
  ! writes outputs
  subroutine write_data(t_indx, delta_t, u, x, N, solsFile)
    implicit none
    integer, intent(in) :: t_indx, N
    real (fp), intent(in) :: delta_t
    real (fp), intent(in) :: u(0:N+1), x(0:N+1) 

    character (len=*) :: solsFile
    real(fp) :: t
    integer :: i

    
      print *, 'Writing file: ', solsFile
      open(20,file=solsFile,status="replace")
      t = t_indx*delta_t
      write(20,*) t_indx, t, N
      
      do i=1,N 
        write(20,*) x(i), u(i)
        !print *, x(i), u(i)
      end do
      close(20)


  end subroutine write_data

  subroutine write_Diff_data(time, u, x, N, solsFile)
    
    implicit none
    integer, intent(in) :: N
    real (fp), intent(in) :: time
    real (fp), intent(in) :: u(0:N+1), x(0:N+1) 
  
    character (len=*) :: solsFile
    integer :: i
  
    
      print *, 'Writing file: ', solsFile
      open(20,file=solsFile,status="replace")
      write(20,*) time, N
      
      do i=1,N 
        write(20,*) x(i), u(i)
        !print *, x(i), u(i)
      end do
      close(20)
  
  
  end subroutine write_Diff_data


end module output_module
