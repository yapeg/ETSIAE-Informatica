!Este programa nos permite dar la posicion (o posiciones, en caso de que se repitiera) de un valor pedido entre las componentes de un vector de dimension n.

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

endif

if (n <= 0) then
write(*,*) "La dimension del vector no puede ser negativa o nula."
endif
write(*,*) 
read(*,*)
end program

subroutine search(dimens, comp, vector)

integer, intent(in) :: dimens
real(8), intent(inout) :: vector(dimens)
real(8), intent(in) :: comp
 
integer :: i, j
i = 1

do while (i <= dimens)
if (comp == vector(i)) then
write(*, "(a44, 2x, i2)") "El valor buscado se encuentra en la posicion", i
endif
i = i+1
enddo 

do j = 1, dimens
if(comp == vector(j)) exit
enddo

if (j == dimens+1) then
write(*,*) "Esa componente no existe."
endif

end subroutine 
