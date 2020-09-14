module integral_module
use function_module
implicit none

contains

subroutine rectangulo (a, b, f, deltax, integral)

real(8), intent(in) :: a, b, deltax
real(8), intent(out) :: integral
interface
function f(x)
real(8), intent(in) :: x
real(8) :: f
end function f
end interface
real(8) :: x, suma, deltax_0
integer :: j, N

N = (b - a)/deltax
deltax_0 = (b - a)/N

suma = 0d0

do j = 0, N-1
x = a + deltax_0*j
suma = suma + f(x)
enddo

integral = deltax_0 * suma

end subroutine

subroutine punto_medio(a, b, f, deltax, integral)

real(8), intent(in) :: a, b, deltax
real(8), intent(out) :: integral
interface
function f(x)
real(8), intent(in) :: x
real(8) :: f
end function f
end interface
real(8) :: x, suma, deltax_0
integer :: j, N														

N = (b - a)/deltax
if (mod(N,2) /= 0d0) then												! N par
N = N+1
endif
deltax_0 = (b - a)/N

suma = 0d0

do j = 0, (N/2)-1
x = a + deltax_0*(2*j + 1)
suma = suma + f(x)
enddo

integral = 2 * deltax_0 * suma

end subroutine

subroutine trapecio(a, b, f, deltax, integral)

real(8), intent(in) :: a, b, deltax
real(8), intent(out) :: integral
interface
function f(x)
real(8), intent(in) :: x
real(8) :: f
end function f
end interface
real(8) :: x, suma, deltax_0
integer :: j, N

N = (b - a)/deltax
deltax_0 = (b - a)/N

suma = 0d0

do j = 1, N-1
x = a + deltax_0*j
suma = suma + f(x)
enddo

integral = (deltax_0/2) * (f(a) + 2*suma + f(b))

end subroutine

subroutine simpson(a, b, f, deltax, integral)

real(8), intent(in) :: a, b, deltax
real(8), intent(out) :: integral
interface
function f(x)
real(8), intent(in) :: x
real(8) :: f
end function f
end interface
real(8) :: x, deltax_0, suma_1, suma_2
integer :: i, j, N

N = (b - a)/deltax
if (mod(N,2) /= 0d0) then												! N par
N = N+1
endif
deltax_0 = (b - a)/N												

suma_1 = 0d0
suma_2 = 0d0

do i = 1, (N/2)-1
x = a + deltax_0*2*i
suma_1 = suma_1 + f(x)
enddo

do j = 1, N/2
x = a + deltax_0*(2*j - 1)
suma_2 = suma_2 + f(x)
enddo

integral = (deltax_0/3)*(f(a) + 2*suma_1 + 4*suma_2 + f(b))

end subroutine

end module
