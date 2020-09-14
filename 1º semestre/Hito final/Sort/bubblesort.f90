! Este programa pretende ordenar los elementos de una serie (de menor 
! a mayor) mediante el "ordenamiento de la burbuja" ('bubblesort').

program bubblesort
implicit none 

integer :: n 
integer, allocatable :: v(:)

write(*,*) "Introduce la dimension del vector:"
read(*,*) n
write(*,*)

if (n > 0) then
allocate (v(n))
write(*,*) "Escribe el vector:"
write(*,*)
read(*,*) v

write(*,*)
write(*, "(a19, 100(i3, 2x))") "Vector original =  ", v

write(*,*)
call compare (n, v)
write(*,*)

endif

if (n <= 0) then 
write(*,*) "La dimension de un vector no puede ser negativa o nula."
endif

write(*, "(a19, 100(i3, 2x))") "Vector ordenado =  ", v
write(*,*)

read(*,*)
end program 

subroutine compare (dimension, vector)

integer :: i, j, k
integer :: swap
integer, intent(in) :: dimension
integer, intent(inout) :: vector(dimension)

do j = 1, dimension-1
do k = 2, dimension
if (vector(j) > vector(k)) then

do i = 1, dimension-1
if (vector(i) > vector(i+1)) then
swap = vector(i)
vector(i) = vector(i+1)
vector(i+1) = swap
write(*,*) vector
endif
enddo

endif
enddo
enddo

end subroutine 
