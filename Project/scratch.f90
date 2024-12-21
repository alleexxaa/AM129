! File: name_of_module.f90
! Purpose: 
! Author: Sean Riedel

module name_of_module
    use utility, only : fp

    implicit none

contains

    function name_of_func(name_of_input) result(name_of_result)
        ! function:     name_of_func
        ! Author:       Sean Riedel
        ! Purpose:      
        implicit none
        real(fp), intent(in) :: name_of_input
        real(fp) :: name_of_result

    end function name_of_func

    subroutine name_of_routine()
        ! subroutine:   name_of_routine
        ! Author:       Sean Riedel
        ! Purpose:      

        implicit none
        real(kind=fp), intent(in) :: 
        ! local variables


        return
    end subroutine name_of_routine


end module name_of_module



! --------------------------------------------------

    subroutine diffusionEQN21(M,N,a,delta_t,delta_x,u,u_new)
        implicit none
        integer :: i, ntime
    
        integer, intent(in) :: M,N
        real(fp), intent(in) :: a
        real(fp), intent(in) :: delta_t
        real(fp), intent(in) :: delta_x
        real(fp), intent(in out):: u(0:N+1),u_new(0:N+1)
    
        do i=1,N
            u_new(i)=u(i) + ((k*(delta_t)/delta_x**2)*(u(i+1)-2*u(i)+u(i-1)))
        end do
    
    end subroutine diffusionEQN21



      ! sets discontinuous conditions
  if (((0<=x(1)) .and. (x(1)<1/3)) .or. ((2/3<x(1)) .and. (x(1)<=1))) then
    u = -1
  else if (((1/3<=x(1)) .and. (x(1)<=2/3))) then
    u = 1
  end if
  
  ! sets diffusion initial conditions
  if ((0<=x(1)) .and. (x(1)<=1)) then
    u = 0
  else if (x(1) == 1) then
    u = 100
  end if

  ! sets diffusion boundary conditions
  u(0) = 0.0_fp
  u(N+1) = 100.0_fp


  subroutine diffusionGhost(N,x,u,u_new)
    implicit none
    integer :: i

    integer, intent(in) :: N
    real(fp), intent(in) :: x(0:N+1)
    real(fp), intent(in out) :: u(0:N+1),u_new(0:N+1)

  ! sets diffusion initial conditions
    if ((0<=x(1)) .and. (x(1)<=1)) then
        u = 0
        u_new = 0
      else if (x(1) == 1) then
        u = 100
        u_new = 100
      end if

end subroutine diffusionGhost


subroutine write_data(t_indx, N, delta_t, u, x, solsFile)
  implicit none
  integer, intent(in) :: t_indx, N
  real(fp), intent(in) :: delta_t
  real(fp), intent(in) :: u(0:N+1), x(0:N+1) 
  character(len=maxStrLen) :: solsFile
  real(fp) :: t
  integer :: i

  print *, 'Writing file: ', trim(solsFile)

  open(20, file=trim(solsFile), status="replace")
  t = t_indx * delta_t
  write(20, *) t_indx, t

  do i = 1, N
    write(20, *) x(i), u(i)
  end do

  close(20)
  
end subroutine write_data



subroutine write_data(t_indx, delta_t, u, x, N, solsFile)
  implicit none
  integer, intent(in) :: t_indx, N
  real (fp), intent(in) :: delta_t
  real (fp), intent(in) :: u(0:N+1), x(0:N+1) 

  character (len=maxStrLen) :: solsFile
  real(fp) :: t
  integer :: i

  
  print *, 'Writing file: ', solsFile
  open(20,file=solsFile,status="replace")
  t = t_indx*delta_t
  write(20,*) t_indx, t
    
  do i=1,N 
    write(20,*) x(i), u(i)
  end do
  close(20)

end subroutine write_data



axs[0,1].plot(xDont, yDont1, label=labels[4], linestyle='--', color='black')
axs[0,1].plot(xDont, yDont2, label=labels[5], color='deeppink')
axs[0,1].set_title('Initial and Upwind Discontinuous at t=0.95')
axs[0,1].legend()

axs[1,1].plot(xDont, yDont1, label=labels[4], linestyle='--', color='black')
axs[1,1].plot(xDont, yDont3, label=labels[6], color='deeppink')
axs[1,1].set_title('Initial and Centered Discontinuous at t=0.95')
axs[1,1].legend()

axs[2,1].plot(xDont, yDont1, label=labels[4], linestyle='--', color='black')
axs[2,1].plot(xDont, yDont4, label=labels[7], color='deeppink')
axs[2,1].set_title('Initial and Lax Wendroff Discontinuous at t=0.95')
axs[2,1].legend()


else if (a_or_d == 1) then                           ! new loop 2 opens, 
    
  ! set delta_t specific to Diffusion case
  delta_t = (delta_x**2.0)/(kappa*2.0)
  ! sets Diffusion Initial conditions
  call diffusionIC(N,u,u_new)
  
  l1_norm = 0.0

  do (i=1,N)
    if (l1_norm >= epsilon) then

      call diffusionEQN21(N,kappa,delta_t,delta_x,u,u_new)
      call Calc_L1Norm(N, u, u_new)
    
    else:
      

      
  end if






subroutine write_Diff_data(time, delta_t, u, x, N, solsFile)
    
  implicit none
  integer, intent(in) :: N
  real (fp), intent(in) :: delta_t,time
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


end subroutine write_Diff_data