! File: advect_diff.f90
! Author: Alexandra Mendoza
! Purpose: Solve advection-Diffusion equation
! will need **12** init files to read through for each case
program advect_diff
  
  use utility, only: fp, maxStrLen
  use fd_module, only: upwind, centered, Lax_Wendroff, diffusionEQN21
  use output_module, only: write_data, write_Diff_data
  use setup_module, only: advect_cont, advect_discont, diffusionIC
  use readFile_module, only: read_initFileInt, read_initFileReal, read_initFileChar
  use error_module, only: Calc_L1Norm
  
  implicit none

  ! Declare variables
  integer :: N,M,i,ntime,a_or_d, new_ntime
  real (fp) :: a,kappa,epsilon
  real (fp) :: cfl
  real (fp) :: x_a
  real (fp) :: x_b
  real (fp) :: delta_x
  real (fp) :: delta_t, t_max, l1_norm
  real (fp) :: write_interval, current_time, last_write_time
  real (fp) :: timestamps(5)
  character(len=maxStrLen) :: ics, outFile, method
  
  
  
  !logical :: isWorking !!!delete me later!!!
  
  

  real (fp), allocatable :: x(:)  ! grid
  real (fp), allocatable :: u(:)  ! solutions
  real (fp), allocatable :: u_new(:)
  
  
  ! Set variables
  N = read_initFileInt('runtime_parameters.init', 'sim_N')
  a_or_d = read_initFileInt('runtime_parameters.init', 'a_or_d')  
  
  t_max = read_initFileReal('runtime_parameters.init', 't_max')
  a = read_initFileReal('runtime_parameters.init', 'sim_a')
  cfl = read_initFileReal('runtime_parameters.init', 'sim_C')
  kappa = read_initFileReal('runtime_parameters.init', 'sim_k')
  epsilon = read_initFileReal('runtime_parameters.init', 'sim_e')
   

  !print *, a_or_d

  ics = read_initFileChar('runtime_parameters.init', 'ic_type')
  method = read_initFileChar('runtime_parameters.init', 'method')
  write_interval = read_initFileReal('runtime_parameters.init', 'write_interval')
  
  
  x_a = 0.0
  x_b = 1.0
  delta_x = (x_b - x_a)/N
  !M = ceiling(t_max/delta_t)
  !print *, M


  ! allocate arrays
  allocate(x(0:N+1))
  allocate(u(0:N+1))
  allocate(u_new(0:N+1))
  

  ! Fill in x array
  do i=1,N
    x(i) = x_a + (i - 0.5_fp)*delta_x
    !print *, u(i),x(i)
  end do
  

  ! define ghost cells
  x(0) = x_a - (delta_x/2)
  x(N+1) = x_b + (delta_x/2)


! where all the magic happens

  if (a_or_d == 0) then                                      ! Loop 1 (tests advection or Diffusion)
    ! set delta_t specific to advection case                              ! In advection loop
    delta_t = (cfl*delta_x)/(ABS(a))
    M = ceiling(t_max/delta_t)
    ! upwind method
    if (method == 'upwind') then                                        ! Loop 2 (first nested) (tests if upwind or centered or LW)
      ! continuous ICs                                                  ! In advection -> upwind loop
      if (ics == 'continuous') then                                     ! Loop 3 (second neseted)
                                                                        ! Inside advection -> upwind -> continuous loop
        ! make continuous ICs
        call advect_cont(N,x,u,u_new)
        !print *, 'Inside upwind continuous', u_new
        
        ! write the continuous ICs
        call write_data(0, delta_t, u, x, N, 'CICs.dat') 
        
        !write a do loop for time steps
        current_time = 0.0_fp
        last_write_time = 0.0_fp
        
        do ntime=1,M                                                     ! Loop 4 (1st do)
          
          ! sets advection boundary conditions
          u = u_new
          u(0) = u(N)
          u(N+1) = u(1)
          ! update u_new based on upwind method
          call upwind(N,a,delta_t,delta_x,u,u_new)
          current_time = delta_t*ntime
          
          if ((current_time-last_write_time) >= write_interval) then    ! Loop 5 (if under do)
            write(outFile, "(I9)") 100000000+ntime
            outFile = 'data/upwindsCont_' // trim(outFile) //'.dat'
            call write_data(ntime, delta_t, u, x, N, outFile)
            last_write_time = current_time
          
          end if                                                        ! Loop 5 closes (end of if)
        
        end do                                                          ! Loop 4 closes (end of do)

        ! discontinuous ICs
      else if (ics == 'discontinuous') then                             ! Loop 3
                                                                        ! Inside advection -> upwind -> discontinuous
        call advect_discont(N,x,u_new)
        u = u_new
        !print *, 'inside discontinuous', u_new
        
        call write_data(0, delta_t, u_new, x, N, 'discont_ICs.dat')
        !print *, 'after writing discontinuous ICs'
        !write a do loop for time steps
        current_time = 0.0_fp
        last_write_time = 0.0_fp
        do ntime=1,M                                                     ! Loop 4 (1st do)
          ! sets advection boundary conditions
          u = u_new
          u(0) = u(N)
          u(N+1) = u(1)
          call upwind(N,a,delta_t,delta_x,u,u_new)
          current_time = delta_t*ntime
         
          if ((current_time-last_write_time) >= write_interval) then    ! Loop 5 (if under do)
            write(outFile, "(I9)") 100000000+ntime
            outFile = 'data/upwindsDiscont_' // trim(outFile) //'.dat'
            call write_data(ntime, delta_t, u, x, N, outFile)
            last_write_time = current_time
          
          end if                                                        !close loop 5
        end do                                                          ! close loop 4
      end if                                                            ! close loop 3 (discontinuous) now advection -> upwind
    end if                                                              ! close loop 2 (upwind) now advection



    if (method == 'centered') then                                      ! loop 2 advection -> centered
      
      if (ics == 'continuous') then                                     ! loop 3 advection -> centered -> continuous
        
        call advect_cont(N,x,u,u_new)
        call write_data(0, delta_t, u, x, N, 'CICs.dat')
        !write a do loop for time steps
        current_time = 0.0_fp
        last_write_time = 0.0_fp
        
        do ntime=1,M                                                    ! loop 4 advection -> centered -> continuous -> do
          u = u_new
          ! sets advection boundary conditions
          u(0) = u(N)
          u(N+1) = u(1)
          call centered(N,a,delta_t,delta_x,u,u_new)
          current_time = delta_t*ntime
          
          if ((current_time-last_write_time) >= write_interval) then    ! loop 5 advection -> centered -> continuous -> do -> if 
            write(outFile, "(I9)") 100000000+ntime
            outFile = 'data/centeredCont_' // trim(outFile) //'.dat'
            call write_data(ntime, delta_t, u, x, N, outFile)
            last_write_time = current_time
          
          end if                                                        ! loop 5 ends now advection -> centered -> continuous -> do
        
        end do                                                          ! loop 4 ends now advection -> centered -> continuous

      else if (ics == 'discontinuous') then                             ! loop 3 advection -> centered -> discontinuous
        call advect_discont(N,x,u_new)
        u = u_new
        call write_data(0, delta_t, u, x, N, 'discont_ICs.dat')
        !write a do loop for time steps
        current_time = 0.0_fp
        last_write_time = 0.0_fp
        
        do ntime=1,M                                                    ! loop 4 advection -> centered -> discontinuous -> do
          ! sets advection boundary conditions
          u = u_new
          u(0) = u(N)
          u(N+1) = u(1)
          call upwind(N,a,delta_t,delta_x,u,u_new)
          current_time = delta_t*ntime
          
          if ((current_time-last_write_time) >= write_interval) then   ! loop 5 advection -> centered -> continuous -> do -> if 
            write(outFile, "(I9)") 100000000+ntime
            outFile = 'data/centeredDiscont_' // trim(outFile) //'.dat'
            call write_data(ntime, delta_t, u, x, N, outFile)
            last_write_time = current_time
          
          end if                                                        ! loop 5 ends now advection -> centered -> continuous -> do
        end do                                                          ! loop 4 ends now advection -> centered -> continuous
      end if                                                            ! loop 3 ends now advection -> centered
    end if                                                              ! loop 2 ends now advection



    if (method == 'Lax_Wendroff') then                                  ! loop 2 advection -> LW
      
      if (ics == 'continuous') then                                     ! loop 3 advection -> LW -> continuous
        call advect_cont(N,x,u,u_new)
        call write_data(0, delta_t, u, x, N, 'CICs.dat')
        !write a do loop for time steps
        current_time = 0.0_fp
        last_write_time = 0.0_fp

        do ntime=1,M                                                    ! loop 4 advection -> LW -> continuous -> do
          u = u_new
          ! sets advection boundary conditions
          u(0) = u(N)
          u(N+1) = u(1)
          call Lax_Wendroff(N,a,delta_t,delta_x,u,u_new)
          current_time = delta_t*ntime
          
          if ((current_time-last_write_time) >= write_interval) then ! loop 5 advection -> LW -> continuous -> do -> if
            write(outFile, "(I9)") 100000000+ntime
            outFile = 'data/LaxWenCont_' // trim(outFile) //'.dat'
            call write_data(ntime, delta_t, u, x, N, outFile)
            last_write_time = current_time
          
          end if                                                    ! loop 5 closes, now advection -> LW -> continuous -> do
        end do                                                      ! loop 4 closes, now advection -> LW -> continuous

      else if (ics == 'discontinuous') then                         ! new loop 3, advection -> LW -> discontinuous
        
        ! sets discontinuous initial conditions
        call advect_discont(N,x,u_new)
        u = u_new
        ! writes initial conditions
        call write_data(0, delta_t, u, x, N, 'discont_ICs.dat')

        !write a do loop for time steps
        current_time = 0.0_fp
        last_write_time = 0.0_fp
        
        do ntime=1,M                                              ! loop 4, advection -> LW -> discontinuous -> do
          ! sets advection boundary conditions
          u = u_new
          u(0) = u(N)
          u(N+1) = u(1)
          call upwind(N,a,delta_t,delta_x,u,u_new)
          current_time = delta_t*ntime
         
          if ((current_time-last_write_time) >= write_interval) then ! loop 5, advection -> LW -> discontinuous -> do -> if
            write(outFile, "(I9)") 100000000+ntime
            outFile = 'data/LaxWenDiscont_' // trim(outFile) //'.dat'
            call write_data(ntime, delta_t, u, x, N, outFile)
            last_write_time = current_time
          
          end if                                                    ! loop 5 closes, now advection -> LW -> discontinuous -> do
        end do                                                      ! loop 4 closes, now advection -> LW -> discontinuous
      end if                                                        ! loop 3 closes, now advection -> LW 
    end if                                                          ! loop 2 closes, now advection
  
  
  
  else if (a_or_d == 1) then     !1 represents diffusion, in diffusion loop now                      ! new loop 2 opens, 
    
    ! set delta_t specific to Diffusion case
    delta_t = (delta_x**2.0)/(kappa*2.0)
    M = ceiling(t_max/delta_t)
    ! sets Diffusion Initial conditions
    call diffusionIC(N,u,u_new)
    
    l1_norm = 1.0
    
    do while (l1_norm >= epsilon)
      
      !outFile = 'Dif_Sols.dat'
      !open(unit=20, file=outFile)
      !close(20)
      current_time = 0.0_fp
      
      do ntime=1,M

        u = u_new
        
        !updates values of u_new
        call diffusionEQN21(N,kappa,delta_t,delta_x,u,u_new)
        current_time = delta_t*ntime
        l1_norm = 0.0
        !calculates L1Norm at i


        call Calc_L1Norm(N, l1_norm, delta_t, u, u_new)
        !if (ntime == 1 .or. ntime == N) then
        !if (ntime == 1) then
            
        !end if
        ! print *, 'after calculating L1 norm'
        ! finds l1_norm less than epsilon and gives t_max
        if (l1_norm < epsilon) then

          print *, 'First L1 norm less than epsilon:', l1_norm
          t_max = current_time
          print *, 'L1 norm is less than epsilon at t =', t_max
          exit
        end if

      end do
    end do

  timestamps = [0*t_max, 0.2*t_max, 0.5*t_max, 0.8*t_max, 1*t_max]
    
  print *, timestamps(2)
  
  do i = 1, size(timestamps)
    do ntime = 1, M
      ! update u
      call diffusionEQN21(N,kappa,delta_t,delta_x,u,u_new)

      ! Check if the current time matches the desired time
      new_ntime = ntime-1
      
      if (new_ntime * delta_t > timestamps(i)) then
        ! Record the values at time t
          
        write(outFile, "(I9)") 100000000+(i-1)
        outFile = 'data/DifSols_' // trim(outFile) //'.dat'
        call diffusionEQN21(N,kappa,delta_t,delta_x,u,u_new)
        call write_Diff_data(timestamps(i), u, x, N, outFile)
      
      end if
    end do
  end do
  
  else
    print *, '*screaming*'


  
  end if
  
  ! Deallocate x
  deallocate(x)
  deallocate(u)
  deallocate(u_new)


end program advect_diff
