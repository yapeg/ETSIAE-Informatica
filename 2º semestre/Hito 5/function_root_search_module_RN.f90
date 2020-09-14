module function_root_search_module_RN
implicit none

contains

	!Método numérico: cálculo de la matriz jacobiana con la definición de derivada
  subroutine NumericalNewtonRaphsonMethod_RN(x, f, dx, cota_error, iter_max)
	
	implicit none
	
	real(8), dimension(:), intent(inout) :: x
	real(8), intent(in) :: dx
	real(8), intent(in) :: cota_error
	integer, intent(inout) :: iter_max
	interface
	  function f(x)
	  implicit none
	  real(8), dimension(:), intent(in) :: x
	  real(8), dimension(size(x)) :: f
	  end function f
	end interface
	real(8), dimension(size(x), size(x)) :: J_0, Ji
	real(8), dimension(size(x)) :: x_0
	integer :: iter	
	real(8) :: error
		
	x_0 = x + 1d0
	iter = 0
	error = 5d0
	
	do while (iter < iter_max .and. abs(error) > cota_error)
	x_0 = x
	J_0 = J_num(x, f, dx)
	
	call inversa (J_0, Ji)
	
	x = x_0 - matmul(Ji, f(x_0))
	
	iter = iter + 1
	error = (norm2(x) - norm2(x_0))/(norm2(x))
	enddo
	
	iter_max = iter
		
  end subroutine NumericalNewtonRaphsonMethod_RN
  
  
	!Método analítico: forma jacobiana analítica introducida
  subroutine AnalitycalNewtonRaphsonMethod_RN(x, f, J_ana, dx, cota_error, iter_max) 

	implicit none
	
	real(8), dimension(:), intent(inout) :: x
	real(8), intent(in) :: dx
	real(8), intent(in) :: cota_error
	integer, intent(inout) :: iter_max
	interface
	  function f(x)
	  implicit none
	  real(8), dimension(:), intent(in) :: x
	  real(8), dimension(size(x)) :: f
	  end function f
	  
	  function J_ana(x)
	  implicit none
	  real(8), dimension(:), intent(in) :: x
	  real(8), dimension(size(x), size(x)) :: J_ana
	  end function J_ana
	end interface
	real(8), dimension(size(x), size(x)) :: Ji, J_0
	real(8), dimension(size(x)) :: x_0, f_0
	integer :: iter	
	real(8) :: error
	
	x_0 = x + 1d0
	iter = 0
	error = 5d0
	
	do while (iter < iter_max .and. abs(error) > cota_error)
	x_0 = x
	f_0 = f(x_0)
	J_0 = J_ana(x_0)
	
	call Inversa(J_0, Ji)
	
	x = x_0 - matmul(Ji, f_0)
	
	iter = iter + 1
	error = (norm2(x) - norm2(x_0))/(norm2(x))
	enddo
	
	iter_max = iter
	
  end subroutine AnalitycalNewtonRaphsonMethod_RN


  function J_num(x, f, dx)

	implicit none
	
	real(8), dimension(:), intent(inout) :: x
	real(8), dimension(size(x), size(x)) :: J_num
	real(8), intent(in) :: dx
	interface
	  function f(x)
	  implicit none
	  real(8), dimension(:), intent(in) :: x
	  real(8), dimension(size(x)) :: f
	  end function f
	end interface
	integer :: i, n
	real(8), dimension(size(x)) :: x_0, f_0, h
	
		n = size(x)
		
		x_0 = x
		f_0 = f(x_0)	
		
		h = dx * abs(x_0)
		
		where (h <= dx) h = dx
		
		do i = 1, n
		  x(i) = x_0(i) + h(i)
		  J_num(:,i) = (f(x) - f_0) / h(i)
		  x(i) = x_0(i)
		enddo
		
  end function J_Num
  
  
  subroutine Inversa(A, Ai)

	implicit none
	
	real(8), intent(inout) :: A(:,:), Ai(:,:)
	real(8), allocatable :: Ide(:,:), A_0(:,:)
	integer	::	n, i
	
	n = size(A, dim=1)
	allocate(Ide(n,n), A_0(n,n))
	
	A_0 = A
	Ide = 0 

	do i = 1, n 
	Ide(i,i) = 1
	A = A_0
	call Gauss(A, Ide(:,i), Ai(:,i))
	enddo
	
  end subroutine Inversa
  
  
  subroutine Gauss(A, b, x)
	real(8), intent(inout) :: A(:, :), b(:)
	real(8), intent(out) :: x(:)
	real(8) :: l
	real(8) :: suma
	integer :: i, j, k, n

	n = size(A, dim=1)

	do k = 1, n-1
	do i = k+1, n

	l = A(i, k)/A(k, k)
	do j = k, n										
	A(i, j) = A(i,j) - l * A(k,j)
	enddo
	b(i) = b(i) - l * b(k)
	enddo
	enddo

	do i = n, 1, -1
	if (i == n) then
	x(i) = (b(i)/A(i,i))
	else
	suma = 0.0
	do j = n, i+1, -1
	suma = suma + A(i,j) * x(j)
	enddo
	x(i) = (b(i) - suma)/A(i,i)
	endif
	enddo

  end subroutine Gauss


! Las funciones escogidas y sus jacobianas numéricas
function g(x)
real(8), dimension(:), intent(in) :: x
real(8), dimension(size(x)) :: g

g(1) = x(1)**2 - x(2)**2 + 1d0
g(2) = 2 * x(1) * x(2)
g(3) = x(3)**2 - 2d0
end function g

function J_g(x)
real(8), dimension(:), intent(in) :: x
real(8), dimension(size(x), size(x)) :: J_g

J_g = 0d0
J_g(1,1) = 2. * x(1)
J_g(1,2) = -2. * x(2)
J_g(2,1) = 2. * x(2)
J_g(2,2) = 2. * x(1)
J_g(3,3) = 2. * x(3)
end function J_g


function h(x)
real(8), dimension(:), intent(in) :: x
real(8), dimension(size(x)) :: h

h(1) = log(x(1)) - 1./x(2)
h(2) = x(2)**3 - 8d0
end function h

function J_h(x)
real(8), dimension(:), intent(in) :: x
real(8), dimension(size(x), size(x)) :: J_h

J_h = 0d0
J_h(1,1) = 1. / x(1)
J_h(1,2) = (1. / x(2))**2
J_h(2,2) = 3. * x(2)**2
end function J_h


function i(x)
real(8), dimension(:), intent(in) :: x
real(8), dimension(size(x)) :: i

i(1) = exp(x(1) + x(2)) * x(1) *x(2) - exp(x(1) + x(2)) * x(1)**2 
i(2) = 1 - exp(x(1) + x(2)) * x(1) * x(2) - exp(x(1) + x(2)) * x(1) 
end function i

function J_i(x)
real(8), dimension(:), intent(in) :: x
real(8), dimension(size(x), size(x)) :: J_i

J_i(1,1) = exp(x(1)+ x(2)) * (x(1)*x(2) - x(1)**2 - 2.*x(1) + x(2))
J_i(1,2) = exp(x(1)+ x(2)) * x(1) * (-x(1) + x(2) + 1d0)
J_i(2,1) = - exp(x(1)+ x(2)) * (x(1)*x(2) + x(1) + x(2) + 1d0)
J_i(2,2) = - exp(x(1)+ x(2)) * (2.*x(1) + x(1)*x(2))
end function J_i


end module
