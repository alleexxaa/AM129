! File: setup_module.f90
! Author: Alexandra Mendoza
! Purpose: read the initialization files and sets all run wide parameters

module setup_module
    use utility, only : fp, pi
    implicit none

contains

    subroutine advect_cont(N,x,u,u_new)
        implicit none

        integer, intent(in) :: N
        real(fp), intent(in) :: x(0:N+1)
        real(fp), intent(in out):: u(0:N+1),u_new(0:N+1)

            u = sin(2.0*pi*x)
            u_new = sin(2.0*pi*x)

    end subroutine advect_cont

    subroutine advect_discont(N,x,u_new)
        implicit none
        integer :: i

        integer, intent(in) :: N
        real(fp), intent(in) :: x(0:N+1)
        real(fp), intent(in out) :: u_new(0:N+1)

        do i=1, N
           
            if (0.0 <= x(i) .and. x(i)<=1.0/3.0) then
                u_new(i) = -1
            end if
            
            if (1.0/3.0 <= x(i) .and. x(i)<=2.0/3.0) then
                u_new(i) = 1
            end if

            if (2.0/3.0 <= x(i) .and. x(i) <= 1.0) then
                u_new(i) = -1
           
            end if
       
        end do

    end subroutine advect_discont

   
    subroutine diffusionIC(N,u,u_new)
        implicit none
        integer :: i
    
        integer, intent(in) :: N
        real(fp), intent(in out) :: u(0:N+1),u_new(0:N+1)
    
      ! sets diffusion initial conditions
        
        u(0) = 0.0
        u(N+1) = 100.0
        
        do i=0, N
            u(i) = 0.0
        
        end do
        ! set u_new equal to u
        u_new = u
    end subroutine diffusionIC


end module setup_module