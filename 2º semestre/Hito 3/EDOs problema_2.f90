program problema2_edos
implicit none

real(8)  :: deltax
real(8) :: alpha, beta, gamma
integer, parameter :: n = 10
real(8) :: M(n-1,n-1)
real(8) :: x(n-1), y(n-1), u(n-1)
real(8) :: a, b, c, d, pi
integer :: i

pi = acos(-1.0)
M = 0			
!Condiciones de contorno											
a = -1.0																
b = 1.0															
c = 0.0
!d es igual a u(N), donde N es el final del intervalo
!No es una condición que da el enunciado
!Ha sido obtenida con las condiciones dadas (ver informe)
d = 2*pi

deltax = (b-a)/n											
alpha = 2 - deltax
beta = -4 - 2*(deltax**2)
gamma = 2 + deltax

do i = 1, n-1
x(i) = a + deltax*i
enddo

do i = 1, n-2
M(i,i) = beta
M(i+1,i) = alpha
M(i,i+1) = gamma
enddo
M(n-1,n-1) = beta

y = 0.0
y(1) = -alpha*c
y(n-1) = -gamma*d
do i = 1, n-1
y(i) = y(i) + 2*(deltax**2)*sin(2*pi*x(i))
enddo

call eliminacion_gaussiana_y_sustitucion(M, y, u, n-1)

write(*,*) " Punto:        Imagen:"
write(*,"(2(f8.4, 6x))") a, c
do i = 1, n-1
write(*,"(2(f8.4, 6x))") x(i), u(i)
enddo
write(*,"(2(f8.4, 6x))") b, d

write(*,*)
read(*,*)
end program

subroutine eliminacion_gaussiana_y_sustitucion(A, b, x, n)
integer, intent(in) :: n
real(8), intent(inout) :: A(n,n), b(n)
real(8), intent(out) :: x(n)
real(8) :: l
real(8) :: suma
integer :: i, j, k


do k = 1, n-1

do i = k+1, n

l = A(i, k)/A(k, k)												
do j = k, n											
A(i, j) = A(i, j) - l*A(k, j)		
enddo
b(i) = b(i) - l*b(k)
enddo
enddo

do i = n, 1, -1
if (i == n) then
x(i) = (b(i)/A(i,i))
else
suma = 0.0
do j = n, i+1, -1
suma = suma + A(i,j)*x(j)
enddo
x(i) = (b(i) - suma)/A(i,i)
endif
enddo

end subroutine
