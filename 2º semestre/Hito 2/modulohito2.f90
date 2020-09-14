module modulohito2
implicit none

contains

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

! ESTA SUBRUTINA DETERMINA EL AUTOVALOR REAL MAYOR (el radio espectral, por tanto) DE LA MATRIZ A
subroutine autovalor_pdirecta (A, x, lambda, iter)
real(8), intent(inout) :: A(:,:)										!Matriz del problema
real(8), intent(inout) :: x(:)											!Vector inicial (entrada) / Estimación del autovector (salida)
real(8), intent(out) :: lambda 											!Estimación del máximo autovalor (salida)
integer, intent(out) :: iter											!Iteraciones a realizar
real(8) :: tol															!Tolerancia para el criterio de paro
real(8), allocatable :: x_new(:)
integer :: i, n

n = size(x)

tol = 1d-11
iter = 10000

allocate(x_new(n))
x = x/(norm2(x))														!Es preferible que la aproximación inicial sea un vector unitario

do i = 1, iter
x_new = matmul(A,x)
x_new = (x_new)/(norm2(x_new))

if ((norm2(x_new - x))/(norm2(x_new)) <= tol) then						!Cuando se salga del bucle, obtendremos el autovector
iter = i
exit
endif

x = x_new

enddo

! Cálculo de lambda por el método del coeficiente de Rayleigh

lambda = dot_product(x, matmul(A, x)) 
lambda = lambda/(dot_product(x,x))

end subroutine

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

! ESTA SUBRUTINA DETERMINA EL AUTOVALOR REAL MENOR DE LA MATRIZ A
subroutine autovalor_pinversa (A, x, lambda, iter)
real(8), intent(inout) :: A(:,:)
real(8), intent(inout) :: x(:)
real(8), intent(out) :: lambda
integer, intent(out):: iter
real(8) :: tol															!Tolerancia para el criterio de paro
real(8), allocatable :: x_new(:), Ai(:,:), A_0(:,:)
integer :: i, n
 n = size(x)

tol = 1d-11
iter = 100000
allocate(x_new(n), Ai(n,n), A_0(n,n))
x = x/(norm2(x))														!Es preferible que la aproximación inicial sea un vector unitario

A_0 = A

call inversa(A, Ai)

do i = 1, iter
x_new = matmul(Ai,x)
x_new = (x_new)/(norm2(x_new))

if ((norm2(x_new - x))/(norm2(x_new)) <= tol) then						!Cuando se salga del bucle, obtendremos el autovector
iter = i
exit
endif

x = x_new

enddo

! Cálculo de lambda por el método del cociente de Rayleigh

lambda = dot_product(x, matmul(A_0, x)) 
lambda = lambda/(dot_product(x,x))

end subroutine

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

! --- LA SUBRUTINA CALCULA LA MATRIZ INVERSA DE UNA MATRIZ CUADRADA ---
subroutine inversa (A, Ai)
real(8), intent(inout) :: A(:,:) 
real(8), intent(out) :: Ai(:,:)
real(8), allocatable :: Ide(:,:), A_0(:,:)
integer	::	n, i

n = size(A, dim=1)

allocate(Ide(n, n), A_0(n,n))											!Asigna dimensión a matriz identidad

A_0 = A

Ide = 0        															!Genera la matriz I            
do i = 1, n 
Ide(i,i) = 1
enddo

do i = 1, n
A = A_0																	!ESTA ES FÁCILMENTE LA LÍNEA MÁS IMPORTANTE DE TODA LA SUBRUTINA
																		!ES TOTALMENTE NECESARIO 'RESETEAR' LA MATRIZ A (a menos que haya otra manera)
call eliminacion_gaussiana_y_sustitucion(A, Ide(:,i), Ai(:,i)) 			!Resuelve el sistema " A * Ai(:, i) = Ide(:, i) "
enddo																	!donde se busca obtener cada una de las columnas de
																		!la matriz inversa
end subroutine

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

! --- LA SUBRUTINA RESUELVE UN SISTEMA DE ECUACIONES LINEALES POR EL MÉTODO DE GAUSS ---
subroutine eliminacion_gaussiana_y_sustitucion(A, b, x)
real(8), intent(inout) :: A(:, :), b(:)
real(8), intent(out) :: x(:)
real(8), allocatable :: swapA(:)
real(8) :: l, swapB
real(8) :: suma
integer :: i, j, k, h, n

n = size(A, dim=1)

allocate (swapA(n))

! --- PARTE DE ELIMINACIÓN (creación de matriz triangular) ---
do k = 1, n-1

if (A(k, k) == 0.0) then												!Reposiciona las filas si la diagonal contiene ceros
do h = k+1, n																
if (A(h,k) /= 0.0) then
swapA = A(k, :)
A(k, :) = A(h, :)
A(h, :) = swapA

swapB = b(k)
b(k) = b(h)
b(h) = swapB
exit
endif
enddo

if (h > n) then
write(*,*) "Esta operacion no es ralizable, pues la matriz sobre la que se esta operando tiene determinante igual a cero."
endif
endif

do i = k+1, n

l = A(i, k)/A(k, k)														!Se obtiene el multiplicador que se usará para toda la fila
do j = k, n											
A(i, j) = A(i, j) - l*A(k, j)											!Opera con las filas
enddo
b(i) = b(i) - l*b(k)
enddo
enddo


! --- PARTE DE SUSTITUCIÓN (obtención de la solución) ---
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

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _


end module
