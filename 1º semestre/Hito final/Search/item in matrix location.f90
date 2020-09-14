program matrix
implicit none

integer :: i
integer :: m, n
real(8), allocatable :: A(:,:)

write(*,*) "Introduce el numero de filas de la matriz:"
read(*,*) m
write(*,*)

if (m <= 0) then
write(*,*) "El numero de filas no puede ser nulo o negativo."
endif

if (m > 0) then
write(*,*) "Introduce el numero de columnas de la matriz:"
read(*,*) n
write(*,*)
endif

if (m > 0 .and. n <= 0) then
write(*,*) "El numero de columnas no puede ser nulo o negativo."
endif

if(m > 0 .and. n > 0) then
allocate (A(m, n))

write(*,*) "Escribe la matriz (por filas, de arriba a abajo):"
do i = 1, m
read(*,*) A(i, :)
write(*,*)
enddo

call search (m, n, A)

endif

write(*,*)
read(*,*)
end program 


subroutine search (fila, columna, matriz)

integer, intent(in) :: fila
integer, intent(in) :: columna
real(8), intent(in) :: matriz(fila, columna)
integer :: valor
integer :: i, j

write(*,*) "Indica el valor que deseas buscar entre los elementos de la matriz:"
read(*,*) valor
write(*,*)

i = 1
do while (i <= fila)
do j = 1, columna
if (matriz(i, j) == valor) then
write(*,"(a37, 2(i2, 1x))") "El valor se encuentra en la posicion ", i, j
endif
enddo
i = i + 1
enddo

end subroutine 
