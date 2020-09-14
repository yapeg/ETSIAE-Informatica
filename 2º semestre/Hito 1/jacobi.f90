program jacobi_
use modulohito1
implicit none

real(8), allocatable :: A(:, :), b(:), x(:)
integer :: n, i, j, k, clave

write(*,*) "Orden de la matriz?"				
read(*,*) n
write(*,*)


allocate (A(n, n), b(n), x(n))											!Se asignan las dimensiones a las variables

do i = 1, n
do j = 1, n
call random_number(A(i, j))												!Valores aleatorios al sistema A
if (i == j) then
A(i, j) = A(i, j)*100													!Se multiplican por 100 los valores de la diagonal para que converja y sea así posible el método Jacobi
endif
enddo
enddo

do i = 1, n
call random_number(b(i))												!Valores aleatorios al vector b
b(i) = b(i)*100
enddo

write(*,*) "Matriz original A:"
do i = 1, n
write(*,"(20(f12.5, 2x))") A(i,:)
enddo
write(*,*)


write(*,*) "Matriz-columna resultados b:"
do i = 1, n
write(*,"(1(f12.5, 2x))") b(i)
enddo
write(*,*)

call jacobi(A, b, x, clave)

write(*,*)
if (clave == 0) then
write(*,*) "La solucion X al sistema es:"
do i = 1, n
write(*,"(1(f12.5, 2x))") x(i)
enddo
endif

write(*,*)
read(*,*)
end program 
