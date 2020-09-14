program ecuacion_lineal 												!Vamos aresolver un sistema de ecuaciones lineales
use modulohito1 														!Se usarán subrutinas guardadas en un módulo
implicit none

real(8), allocatable :: A(:, :), b(:), x(:)
integer :: n, i, j, k

write(*,*) "Orden de la matriz?"				
read(*,*) n
write(*,*)


allocate (A(n, n), x(n), b(n))											!Se asignan las dimensiones a las variables

do i = 1, n
do j = 1, n
call random_number(A(i, j))												!Valores aleatorios al sistema A
A(i, j) = A(i, j)*100													!Por 100, para que los valores sean más "entendibles"
enddo
enddo

do i = 1, n
call random_number(b(i))												!Valores aleatorios al vector b
B(i) = B(i)*100
enddo


call eliminacion_gaussiana(A, b)								     	!Se llama a la subrutina que convertirá la matriz A en una triangular superior
																		!y modificará la columna-resultados b
write(*,*) "Obtenemos la siguiente matriz triangular superior A':"
do k = 1, n
write(*,"(25(f12.5, 2x))") A(k, :)
enddo
write(*,*)

write(*,*) "Y la siguiente columna b':"									
do i = 1, n
write(*,"(1(f12.5, 2x))") b(i)
enddo
write(*,*) 

write(*,*) "Ahora deseamos resolver el sistema de ecuaciones."
write(*,*)

call sustitucion_regresiva (A, b, x)

write(*,*) "La solucion al sistema es X:"
do i = 1, n
write(*,"(1(f12.5, 2x))") x(i)
enddo

write(*,*)
read(*,*)
end program 
