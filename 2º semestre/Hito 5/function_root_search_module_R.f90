module function_root_search_module_R
implicit none

contains

subroutine newton_raphson_R(x, f, dx, cota_error, iter_max)
real(8), intent(inout) :: x
real(8), intent(in) :: cota_error
real(8), intent(in) :: dx
integer, intent(inout) :: iter_max
integer:: iter
real(8) :: x_0

interface
function f(x)
	real(8), intent(in) :: x
	real(8) :: f
end function f
end interface

x_0 = x + 1d0

iter = 0
do while (iter < iter_max .and. abs(x - x_0) > cota_error)
x_0 = x

x = x - (f(x))/(derivada(x, dx, f))

iter = iter + 1
enddo

iter_max = iter
end subroutine newton_raphson_R


subroutine bisection(a, b, x, f, cota_error, iter_max)
real(8), intent(inout) :: a, b
real(8) :: a_0, b_0
real(8), intent(out) :: x
real(8), intent(in) :: cota_error
integer, intent(inout) :: iter_max
integer :: iter

interface
function f(x)
real(8), intent(in) :: x
real(8) :: f
end function
end interface

if (f(a)*f(b) > 0d0) then
write(*,*) "No parece que haya ninguna raiz en ese intervalo."
end if

if (f(a)*f(b) < 0d0) then
iter = 0
do while (b - a > cota_error .and. iter < iter_max)
x = (b + a)/2
	if (f(a)*f(x) < 0d0) then
	b_0 = x
	a_0 = a
	endif
	if (f(x)*f(b) < 0d0) then
	a_0 = x
	b_0 = b
	endif
a = a_0
b = b_0

iter = iter + 1
enddo 

x = (b + a)/2
iter_max = iter

endif

end subroutine bisection


function derivada(x, dx, f)
real(8), intent(in) :: x, dx
real(8) :: derivada
interface
function f(x)
	real(8), intent(in) :: x
	real(8) :: f
end function f
end interface

derivada = (1/dx)*(f(x+dx) - f(x))

end function derivada


! Las funciones escogidas
function g(x)
real(8), intent(in) :: x
real(8) :: g

g = exp(x) - x**2

end function g


function h(x)
real(8), intent(in) :: x
real(8) :: h

h = x**3 - 4*x + 6

end function h


function i(x) 						!Esta función requiere precisión al escoger punto
real(8), intent(in) :: x
real(8) :: i

i = sqrt(x) + log(x) 

end function i


end module 
