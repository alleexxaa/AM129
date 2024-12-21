! File: error_module.f90
! Author: Alexandra Mendoza
! Purpose: Checks if the diffusion equation has reached steady state

module error_module
    use utility, only : fp
    implicit none

  contains 
  
  subroutine Calc_L1Norm(N,l1_norm,delta_t, u, u_new)
    implicit none
    integer :: i
    real(fp) :: l1_normy
    
    integer, intent(in) :: N 
    real(fp), intent(in) :: delta_t, u(0:N+1), u_new(0:N+1)
    real(fp), intent(out) :: l1_norm
    

    
    !calculate l1 norm
    l1_norm = 0.0
    l1_normy = 0.0

    do i = 1, N
      l1_normy = l1_normy + ABS(u_new(i) - u(i))
      l1_norm = ((1.0) / (N * delta_t)) * l1_normy


    end do

  end subroutine Calc_L1Norm

  end module error_module
