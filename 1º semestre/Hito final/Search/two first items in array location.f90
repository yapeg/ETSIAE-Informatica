!Este programa permite la búsqueda de dos valores diferentes componentes de un vector de dimensión n.
!Problema: una vez más, el programa sólo nos permite identificar el primero de los valores pedidos. No funciona para valores repetidos.

program busqueda 
implicit none

integer :: n
real(8), allocatable:: v(:)
real(8):: x, y

write(*,*) "Introduce la dimension del vector:"
read(*,*) n
write(*,*) 

if (n > 0) then

allocate(v(n))

write(*,*) "Escribe el vector:"
read(*,*) v(:)
write(*,*) 

write(*,*) "Escribe un valor que quieres buscar:"
read(*,*) x
write(*,*) 

write(*,*) "Escribe un segundo valor que quieres buscar:"
read(*,*) y
write(*,*) 
if (y == x) then
write(*,*) "Ya has elegido buscar ese valor."
endif

if (y /= x) then
call search(n,x,y,v)
endif
endif

if (n <= 0) then 
write(*,*) "La dimension del vector no puede ser negativa o nula."
endif

write(*,*) 
read(*,*)
end program


subroutine search(dimens,comp_1,comp_2,vector)
implicit none

integer, intent(in) :: dimens
real(8), intent(inout) :: vector(dimens)
real(8), intent(in) :: comp_1
real(8), intent(in) :: comp_2

integer :: i, j

do i = 1, dimens
if (comp_1 == vector(i)) exit
enddo

do j = 1, dimens
if (comp_2 == vector(j)) exit
enddo

if (i == dimens+1) then
write(*,*) "El primer valor buscado no existe entre las componentes del vector."
end if

if (j == dimens+1) then
write(*,*) "El segundo valor buscado no existe entre las componentes del vector."
end if

if (i <= dimens) then
write(*,"(a35, 2x, i2)") "El primer valor esta en la posicion", i
endif

if (j <= dimens) then
write(*,"(a36, 2x, i2)") "El segundo valor esta en la posicion", j
endif

end subroutine
