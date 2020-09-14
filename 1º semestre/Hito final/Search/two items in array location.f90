!Este programa permite identificar la posición de dos valores entre las componentes de un vector de dimensión n, tantas veces como aparezcan.

program busqueda
implicit none

integer :: n
real(8), allocatable :: v(:)
real(8) :: x, y

write(*,*) "Introduce la dimension del vector:"
read(*,*) n
write(*,*) 

if (n > 0) then
allocate (v(n))

write(*,*) "Escribe el vector:"
read(*,*) v(:)
write(*,*) 

write(*,*) "Introduce el primer valor a buscar:"
read(*,*) x
write(*,*) 

write(*,*) "Introduce el segundo valor a buscar:"
read(*,*) y
write(*,*) 

if (y == x) then
write(*,*) "Ya has elegido buscar ese valor:"
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

subroutine search(dimens, comp_1, comp_2, vector)

integer, intent(in) :: dimens
real(8), intent(inout) :: vector(dimens)
real(8), intent(in) :: comp_1
real(8), intent(in) :: comp_2

integer :: i, j
i = 1
j = 1

do while (i <= dimens)
if (comp_1 == vector(i)) then
write(*,"(a43, 2x, i2)") "El primer valor buscado esta en la posicion", i
endif
i = i+1
enddo

write(*,*) 

do while (j <= dimens)
if (comp_2 == vector(j)) then
write(*,"(a52, 2x, i2)") "El segundo valor buscado se encuentra en la posicion", j
endif
j = j+1
enddo

do i = 1, dimens
if (comp_1 == vector(i)) exit
enddo

if (i == dimens+1) then
write(*,*) "El primer valor buscado no se encuentra entre las componentes del vector."
endif

do j = 1, dimens
if (comp_2 == vector(j)) exit
enddo

if (j == dimens+1) then
write(*,*) "El segundo valor buscado no se encuentra entre las componentes del vector."
endif

end subroutine
