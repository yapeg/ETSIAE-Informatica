module modulohito1			       									!Este módulo contendrá una serie de subrutinas o funciones básicas de algoritmos numéricos
implicit none

contains

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

! --- LA SUBRUTINA CALCULA EL DETERMINANTE DE UNA MATRIZ CUADRADA ---
subroutine determinante(A, det)
real(8), intent(in) :: A(:, :)
real(8), intent(inout) :: det
real(8), allocatable :: L(:, :), U(:, :)
integer :: n, i

n = size(A, dim=1)

allocate (L(n, n), U(n, n))

call factorizacion_lu(A, L, U)

det = 1.0

do i = 1, n
det = det * U(i, i)
enddo

end subroutine

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

! --- LA SUBRUTINA CALCULA LA MATRIZ INVERSA DE UNA MATRIZ CUADRADA ---
subroutine inversa (A, Ai)
real(8), intent(inout) :: A(:,:), Ai(:,:)
real(8), allocatable	::	Ide(:,:), A_0(:,:)
integer	::	n, i

n = size(A, dim=1)

allocate(Ide(n, n), A_0(n,n))											!Asigna dimensión a matriz identidad
A_0 = A

Ide = 0        															!Genera la matriz I            
do i = 1, n 
Ide(i,i) = 1
enddo

do i = 1, n
A = A_0
call eliminacion_gaussiana_y_sustitucion(A, Ide(:,i), Ai(:,i)) 			!Resuelve el sistema " A * Ai(:, i) = Ide(:, i) "
enddo																	!donde se busca obtener cada una de las columnas de
																		!la matriz inversa
end subroutine

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

! --- LA SUBRUTINA CREA UNA MATRIZ TRIANGULAR SUPERIOR A PARTIR DE OTRA MATRIZ DADA ---
subroutine eliminacion_gaussiana(A, b)									!Crea una matriz triangular superior a partir del sistema de ecuaciones
real(8), intent(inout) :: A(:, :), b(:)
real(8), allocatable :: swapA(:)
real(8) :: l, swapB
integer :: i, j, k, n, h

n = size(A, dim=1)

allocate (swapA(n))

do k = 1, n																

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

end subroutine

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

! --- LA SUBRUTINA HALLA LA SOLUCIÓN A UN SISTEMA CON MATRIZ TRIANGULAR SUPERIOR ---
subroutine sustitucion_regresiva(A, b, x)								!Subrutina resuelve el sistema de ecuaciones triangular
real(8), intent(in) :: A(:, :), b(:)
real(8), intent(out) :: x(:)
real(8) :: suma
integer :: i, j, n

n = size(A, dim=1)

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

! --- LA SUBRUTINA RESUELVE UN SISTEMA DE ECUACIONES LINEALES POR EL MÉTODO DE GAUSS ---
subroutine eliminacion_gaussiana_y_sustitucion(A, b, x)
real(8), intent(inout) :: A(:, :), b(:)
real(8), intent(out) :: x(:)
real(8), allocatable :: swapA(:)
real(8) :: l, swapB
real(8) :: suma
integer :: i, j, k, n, h

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
write(*,*) "Esta operacion no es realizable, pues la matriz sobre la que se esta operando tiene determinante igual a cero."
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

! --- LA SUBRUTINA DESCOMPONE UNA MATRIZ CUADRADA "A" COMO EL PRODUCTO DE UNA TRIANGULAR INFERIOR Y SUPERIOR ---
subroutine factorizacion_lu(A, L, U)									
																		
real(8), intent(in) :: A(:,:)
real(8), intent(out) :: L(:,:), U(:,:)
real(8) :: suma_u, suma_l 
integer :: i, j, k, n

n = size(A, dim=1)

do i = 1, n
do j = 1, n

if (i == j) then														!Valores de las matrices cuando los numeros de columna y fila coinciden
L(i, j) = 1.0

k = 1
suma_u = 0.0
do while (k /= i)
suma_u = suma_u + (L(i, k))*(U(k, j))
k = k + 1
enddo
		
U(i, j) = A(i, j) - suma_u
			
	
elseif (i < j) then 													!Valores de las matrices cuando la fila es menor que la columna
L(i, j) = 0.0
	
k = 1
suma_u = 0.0
do while (k /= i)
suma_u = suma_u + (L(i, k))*(U(k, j))
k = k + 1
enddo
		
U(i, j) = A(i, j) - suma_u
		
		
else 																	!El resto de las posibilidades. Valores de las matrices cuando la fila es mayor que la columna
U(i, j) = 0.0

k = 1
suma_l = 0.0
do while (k /= j)
suma_l = suma_l + L(i, k)*U(k, j)
k = k + 1
enddo

L(i, j) = (A(i, j) - suma_l)/U(j, j)		


endif

enddo
enddo

end subroutine

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

! --- LA SUBRUTINA RESUELVE UN SISTEMA DE ECUACIONES PLANTEADO POR EL MÉTODO LU ---
subroutine sustitucion_directa (L, U, b, x)
real(8), intent(in) :: L(:,:), U(:,:), b(:)
real(8), intent(out) :: x(:)
real(8), allocatable :: C(:)
real(8) :: suma_1, suma_2
integer :: i, j, n

n = size(b)
allocate (C(n))

C(1) = b(1)																!Paso de sustitución 1
    
do i = 2, n
suma_1 = 0.0
do j = 1, i-1
suma_1 = suma_1 + L(i,j) * C(j)
enddo
        
C(i) = (b(i) - suma_1)
    
enddo  


x(n) = C(n)/U(n,n)														!Paso de sustitución 2
    
do i = n-1, 1, -1
    
suma_2 = 0.0
do j = i+1, n
suma_2 = suma_2 + (U(i,j) * x(j))
enddo
        
x(i) = (C(i) - suma_2)/U(i,i)
    
enddo

    
end subroutine

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

! --- LA SUBRUTINA RESUELVE UN SISTEMA DE ECUACIONES POR EL MÉTODO DE JACOBI ---
subroutine jacobi (A, b, x, clave)

real(8), intent(inout) :: A(:,:), b(:), x(:)
integer, intent(out):: clave											!Nos dirá si el método Jacobi es aplicable o diverge
real(8), allocatable ::	L(:,:), U(:,:), D(:,:), Di(:,:), T(:,:), c(:), x_new(:), swapA(:)
real(8) :: cota_error, error, swapB
integer :: n, i, j, h, k

n = size(A, dim=1)
allocate(L(n,n), U(n,n), D(n,n), T(n,n), c(n), Di(n,n), x_new(n))

clave = 0

do k = 1, n																	

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

enddo

L = 0																	!Hasta que se les dé valores, son matrices nulas
U = 0
D = 0

do i = 1, n-1	
	do j = i+1, n
	L(j,i) = A(j,i)														!La matriz L tomará los valores de la parte inferior de A (bajo la diagonal)
	U(i,j) = A(i,j)														!La matriz U tomará los valores de la parte superior de A (sobre la diagonal)
	end do
end do	

do i = 1, n
	D(i,i) = A(i,i)														!La matriz D tomará los valores de la diagonal de A
end do


call inversa (D, Di)													!Se desea conocer la matriz inversa de D, Di

write(*,*) "A partir del sistema inicial Ax = b, se replantea la ecuacion de la forma siguiente:' x = Di*b - Di*R*x '"
write(*,*) "donde Di es la inversa de la diagonal de A,"
write(*,*) "y R es la suma de las matrices de los elementos superiores e inferiores de A." 

! -> Ax = b -> (D+(L+U))x = b -> Dx = b - (L+U)x -> x = Di*b - Di*(L+U)*x
write(*,*)

c = matmul(Di, b)														!Producto matricial
T = matmul(-Di, (U+L))													!Producto matricial

! x = c + Tx

write(*,*) "Matriz C (Di * b) :"
do i = 1, n
write(*,"(1(f12.5, 2x))") c(i)
enddo
write(*,*)

write(*,*) "Matriz T (-Di * R) :"
do i = 1, n
write(*,"(30(f12.5, 2x))") T(i, :)
enddo
write(*,*)

x = 0																	!Se realiza una primera aproximación aleatoria al resultado
cota_error = 1d-9
error = 1.0
k = 1																	!Error -cualquiera- mayor que la cota fijada, que permite iniciar el bucle

do while (cota_error <= error)											!Se repetirá la iteración hasta que el sistema converja (y si no converge, tenemos criterio de parada)

x_new = matmul(T, x) + c
	
error = (norm2(x - x_new))/(norm2(x_new))
			
if ((k > 5000) .and. (norm2(x_new) > norm2(x)*10)) then					!Nuestro criterio de parada particular en caso de divergencia
clave = 1																!A partir de la 50ª iteración, si la norma de la nueva estimación es 
write(*,*) "Este sistema es irresoluble por el metodo de Jacobi." 		!mucho mayor que la anterior, nos salimos del bucle
write(*,*) "El sistema divergera porque su radio espectral es mayor a la unidad."																	
exit
endif																	

x = x_new																
k = k + 1
end do

end subroutine	

!_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

end module
