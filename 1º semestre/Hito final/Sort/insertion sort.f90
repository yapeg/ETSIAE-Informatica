program insertionsort
implicit none

integer :: n
integer, allocatable :: v(:)

write(*,*) "Introduce la dimension del vector:"
read(*,*) n
write(*,*)

if (n > 0) then
allocate (v(n))

write(*,*) "Escribe el vector:"
read(*,*) v
write(*,*)

write(*, "(a18, 100(i3, 2x))") "Vector original = ", v 
write(*,*)

call orden(n, v)

write(*,*)
write(*, "(a18, 100(i3, 2x))") "Vector ordenado = ", v

endif

if (n <= 0) then
write(*,*) "La dimension de un vector no puede ser negativa o nula."
endif

write(*,*)
read(*,*)
end program

subroutine orden (dimension, vector)

integer :: i, j
integer :: swap
integer, intent(in) :: dimension
integer, intent(inout) :: vector(dimension)

do i = 2, dimension
j = i - 1
swap = vector(i)

do while (j >= 1 .and. vector(j) > swap)
vector(j+1) = vector(j)
j = j - 1
enddo

vector(j+1) = swap
write(*,*) vector
enddo


end subroutine 

