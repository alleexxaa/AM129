 ! File: fd_module.f90
! Author: Alexandra Mendoza
! Purpose: Holds and updates the solutions respecting the grid and boundary condition modules

module fd_module
    use utility, only : fp
    implicit none

contains

    subroutine upwind(N,a,delta_t,delta_x,u,u_new)
        implicit none
        integer :: i

        integer, intent(in) :: N
        real(fp), intent(in) :: a
        real(fp), intent(in) :: delta_t
        real(fp), intent(in) :: delta_x
        real(fp), intent(in out):: u(0:N+1),u_new(0:N+1)

        do i=1,N
            u_new(i)= u(i) - ((a*delta_t)/(delta_x))*(u(i)-u(i-1))
        end do

    end subroutine upwind


    subroutine centered(N,a,delta_t,delta_x,u,u_new)
        implicit none
        integer :: i

        integer, intent(in) :: N
        real(fp), intent(in) :: a
        real(fp), intent(in) :: delta_t
        real(fp), intent(in) :: delta_x
        real(fp), intent(in out):: u(0:N+1),u_new(0:N+1)

        do i=1,N
            u_new(i) = u(i) - ( ((a*delta_t)/(2.0*delta_x)) * (u(i+1) - u(i-1)) )
        end do

    end subroutine centered

    subroutine Lax_Wendroff(N,a,delta_t,delta_x,u,u_new)
        implicit none
        integer :: i
    
        integer, intent(in) :: N
        real(fp), intent(in) :: a
        real(fp), intent(in) :: delta_t
        real(fp), intent(in) :: delta_x
        real(fp), intent(in out):: u(0:N+1),u_new(0:N+1)
    
        do i=1,N
            u_new(i)= u(i) - (((a*delta_t)/(2.0*delta_x)) * (u(i+1) - u(i-1))) + (0.5*(((a*delta_t)/(delta_x))**2.0)) * (u(i+1) - & 
                        & (2.0*u(i)) + u(i-1))
        end do
    
    end subroutine Lax_Wendroff

    
    subroutine diffusionEQN21(N,kappa,delta_t,delta_x,u,u_new)
        implicit none
        integer :: i
    
        integer, intent(in) :: N
        real(fp), intent(in) :: kappa
        real(fp), intent(in) :: delta_t
        real(fp), intent(in) :: delta_x
        real(fp), intent(in out):: u(0:N+1),u_new(0:N+1)
    
        do i=1,N
            u_new(i) = u(i) + ((kappa) * ((delta_t)/(delta_x**2)) * (u(i+1) - (2.0*u(i)) + u(i-1)))
        end do
    
    end subroutine diffusionEQN21


end module fd_module
