!Este programa nos permite hallar la posición de un determinado valor entre las componentes de un vector, siempre que este se repita hasta un máximo de dos veces
!Problema: el numero maximo de repeticiones de ese valor para que el programa sea valido es dos

program busqueda
implicit none

integer :: n
real(8), allocatable :: v(:)
real(8) :: x

write(*,*) "Introduce la dimension del vector:"
read(*,*) n
write(*,*) 

if (n > 0) then
allocate (v(n))

write(*,*) "Escribe el vector:"
read(*,*) v(:)
write(*,*) 

write(*,*) "Escribe el valor que quieres buscar:"
read(*,*) x
write(*,*) 

call search(n, x, v)
end if

if (n <= 0) then 
write(*,*) "La dimension del vector no puede ser negativa o nula."
endif

write(*,*)
read(*,*)
end program

subroutine search(dimens, comp, vector)
implicit none

integer, intent(in) :: dimens
real(8), intent(inout) :: vector(dimens)
real(8), intent(in) :: comp

integer :: i, j

do i = 1, dimens
if (comp == vector(i)) exit
enddo

if (i <= dimens) then
write(*, "(a39, 1x, i2)") "Ese valor se encuentra en la componente", i

do j = (i+1), dimens
if (comp == vector(j)) exit
enddo
endif

if (j <= dimens) then
write(*, "(a39, 1x, i2)") "Ese valor tambien esta en la componente", j
endif

if (i == dimens+1) then
write(*,*) "Esa componente no existe."
endif

end subroutine
