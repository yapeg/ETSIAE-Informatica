!Este programa nos permite identificar la posición de un valor entre las componentes de un vector de dimensión n
!Problema: en caso de que ese valor esté repetido, el algoritmo solamente encontrará el que se halle en primera posición

program busqueda
implicit none

integer :: n
real(8), allocatable :: v(:)
real(8) :: x

write(*,*) "Introduce la dimension del vector:"
read(*,*) n
write(*,*) 

if (n > 0) then
allocate(v(n))

write(*,*) "Escribe el vector:"
read(*,*) v(:)
write(*,*) 

write(*,*) "Escribe el valor que quieres buscar:"
read(*,*) x
write(*,*) 

call search(n, x, v)

end if

if (n <= 0) then 
write (*,*) "La dimension de un vector no puede ser negativa o nula."
endif
write(*,*) 

read(*,*)
end program

subroutine search(dimens, comp, vector)
implicit none

integer, intent(in) :: dimens
real(8), intent(inout) :: vector(dimens)
real(8), intent(in) :: comp

integer :: i

do i = 1, dimens
if (comp == vector(i)) exit
enddo

if (i == dimens+1) then
write(*,*) "No hay ninguna componente con ese valor."
endif

if (i <= dimens) then
write(*, "(a31, 1x, i2)") "Ese valor esta en la posicion", i
end if

end subroutine
