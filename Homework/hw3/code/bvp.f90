! File: dft.f90
! Purpose: Solve periodic shifted Poisson problem with spectral method

program bvp
  use utility, only : fp, pi
  use dft, only: matvecprod,dft_InvTransMat,dft_TransMat
  use problemsetup, only: N,forcing,exact_sol,problemsetup_Init,outFile

  implicit none
  
  ! Problem data
  real (fp) :: dx                             ! Grid point spacing
  real (fp), allocatable :: x(:), k(:)        ! Physical domain, wave domain
  real (fp), allocatable :: u(:), f(:), ft(:) ! Solution, forcing, transformed forcing
  real (fp), allocatable :: T(:,:), Tinv(:,:)   ! Transformation matrices
  ! Loop variables
  integer :: i

  ! Report out the selected kind for reals
  print "(' Selected real kind: fp = ',i2)", fp

  ! Read initialization file
  call problemsetup_Init('bvp.init')

  ! Allocate space for all problem data
  call allocate_data()
  
  !!! === 1. Set up physical domain
  dx = (2*pi)/N
  do i=1,size(x)
    x(i)=(i-1)*dx
  end do
  !!! === 2. Fill in the forcing vector
  f = forcing(x)
  !!! === 3. Fill in k and T
  call dft_TransMat(x,k,T)
  !!! === 4. Fill in Tinv
  call dft_InvTransMat(x,k,Tinv)
  !!! === 5. Set ft = T*f using matvecprod
  ft = matvecprod(T, f)
  !!! === 6. Apply wavenumber scaling
  ft = ft/(1+k**2)
  !!! === 7. Set u = Tinv*ft using matvecprod
  u = matvecprod(Tinv, ft)
  !!! === Reports the maximum error and writes out the data file
  print *, "Err: ", calculate_error()
  
  ! Open the output file and write the columns
  call write_data()

  ! Deallocate everything to clean up
  call deallocate_data()

contains

  function calculate_error() result(err)
    implicit none
    real (fp) :: err
    err = maxval( abs( u - exact_sol(x) ) )
  end function calculate_error

  subroutine allocate_data()
    implicit none
    allocate(x(N))
    allocate(k(N))
    allocate(u(N))
    allocate(f(N))
    allocate(ft(N))
    allocate(T(N,N))
    allocate(Tinv(N,N))
  end subroutine allocate_data

  subroutine deallocate_data()
    implicit none
    deallocate(x)
    deallocate(k)
    deallocate(u)
    deallocate(f)
    deallocate(ft)
    deallocate(T)
    deallocate(Tinv)
  end subroutine deallocate_data
  
  subroutine write_data()
    implicit none
    print *, 'Writing file: ', outFile
    open(20,file=outFile,status="replace")
    do i=1,N
      write(20,*) x(i), u(i), f(i), ft(i)
    end do
    close(20)
  end subroutine write_data
      
end program bvp
