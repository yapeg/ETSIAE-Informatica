! Programa para ordenar de mayor a menor las componentes de un vector de dimensión n.
! "Ordenamiento por selección"

program sort
implicit none

integer :: i, j, n, temp
integer, allocatable :: v(:)

write(*,*) "Introduce la dimension del vector:"
read(*,*) n
write(*,*) 

if (n > 0) then
allocate (v(n))

write(*,*) "Introduce las componentes del vector:"
write(*,*) 
read(*,*) v

write(*,*)
write(*, "(a18, 100(i3, 2x))") "Vector original = ", v
write(*,*)

do i = 1, n-1
	do j = i+1, n
		if (v(i) < v(j)) then
			temp = v(i)
			v(i) = v(j)
			v(j) = temp
			write(*,"(100(i3, 2x))") v
		endif
	enddo
enddo

write(*,*) 
write(*, "(a18, 100(i3, 2x))") "Vector ordenado = ", v
endif

if (n <= 0) then
write(*,*) "Un vector no puede tener dimension negativa o nula."
endif
write(*,*) 

read(*,*)
end program
