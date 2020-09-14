program factorizacionlu
use modulohito1
implicit none

real(8), allocatable :: A(:, :), b(:), C(:), x(:), L(:, :), U(:, :)		!Trabajaremos con matrices de orden asignable por teclado
integer :: n, i, j, k, ierr

write(*,*) "Orden de la matriz?"
read(*,*) n
write(*,*)

allocate (A(n, n), b(n), C(n), x(n), L(n, n), U(n, n), stat = ierr)		!Valor asignado!
if (ierr > 0) stop

do i = 1, n
do j = 1, n
call random_number(A(i, j))												!Valores aleatorios al sistema
A(i, j) = A(i, j)*100													
enddo
enddo

do i = 1, n
call random_number(b(i)) 											
B(i) = b(i)*100
enddo


call factorizacion_lu (A, L, U)

write(*,*) "Matriz triangular inferior L:"
do k = 1, n
write(*,"(25(f12.5, 2x))") L(k, :)
enddo
write(*,*)

write(*,*) "Matriz triangular superior U:"
do k = 1, n
write(*,"(25(f12.5, 2x))") U(k, :)
enddo
write(*,*)

call sustitucion_directa (L, U, b, x)

write(*,*) "La solucion al sistema es X:"
do i = 1, n
write(*,"(1(f12.5, 2x))") x(i)
enddo
write(*,*)

read(*,*) 
end program 
 
