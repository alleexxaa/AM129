! File: dft.f90
! Purpose: Provides discrete Fourier transformation matrices

module dft

  use utility, only : fp, pi
  
  implicit none
  
contains

  !!! === 1. function: matvecprod
  function matvecprod(A,x) result(y)
    real (fp), dimension(:,:), intent(in) :: A
    real (fp), dimension(:), intent(in) :: x ! vector matrix, same # of rows as A
    real (fp), dimension(size(A,1)) :: y ! vector matrix, same # of rows as A & x
    integer :: j

    do j=1,rank(x)
      y = SUM(A(:,j)*x(j))
    end do
  end function matvecprod
    

  ! function: dft_TransMat
  ! purpose: Fill transformation matrix for a discrete Fourier transform
  !          on a given domain, and return compatible set of wavenumbers

subroutine dft_TransMat(x,k,T)
    implicit none
    real (fp), intent(in)     :: x(:) !specifies the grid in the physical space
    real (fp), intent(out)    :: k(size(x)) ! wavenumber space
    real (fp), intent(in out) :: T(size(x),size(x)) ! output, holds the transformation matrix
   !!! Does the variable declarations of size(x) keep the dimensions the same?
   !!! why do I need the local variables below? why can't they be above with the others?
    ! Local variables
    integer :: N, i
    real (fp) :: om, dx

    ! Set sizes and base wavenumber
    N = size(x)
    dx = x(2) - x(1)
    om = 2*pi/(N*dx)
    
    ! Set wavenumbers
    k(1) = 0.0_fp
    do i=2,N,2
      k(i) = i*om/2
      if (i+1 <= N) then
        k(i+1) = k(i)
      end if
    end do
    
    !!! === 2. Add your code to fill T here
    T(1,:) = 1/N
    do i=1,size(x)
      if (i/2 == 0) then
        T(i,:) = (2/N) * cos(k(i)*x)
      else if (i>3) then
        T(i,:) = (2/N) * sin(k(i)*x)
      end if
    end do
  end subroutine dft_TransMat

  !!! === 3. subroutine: dft_InvTransMat
  subroutine dft_InvTransMat(x,k,Tinv)
    implicit none
    real (fp), intent(in) :: x(:)
    real (fp), intent(in) :: k(size(x))
    real (fp), intent(in out) :: Tinv(size(x),size(x))
    integer :: j
    Tinv(:,1) = 1.0
    do j=1,size(x)
      if (j/2 == 0) then
        Tinv(:,j) = cos(k(j)*x)
      else if (j>3) then
        Tinv(:,j) = sin(k(j)*x)
      end if
    end do
  end subroutine
end module dft
