! Este programa primero ordena los elementos de un vector de dimensión n, y 
! luego escribe por pantalla la posición del valor que pidamos por teclado, 
! dividiendo sucesivamente en dos partes el vector hasta dar con el resultado.
! Es el método conocido como "DIVIDE AND CONQUER" (divide y vencerás).

program div_and_conq
implicit none

integer :: n
integer, allocatable :: v(:)

write(*,*) "Introduce la dimension del vector:"
read(*,*) n
write(*,*) 

if (n > 0) then
allocate (v(n))

write(*,*) "Introduce las componentes del vector:"
read(*,*) v

write(*,*)
write(*, "(a18, 100(i4, 2x))") "Vector original = ", v
write(*,*) 

call sort(n, v) !Se llama a la subrutina de ordenacion

write(*,*) "Ahora que hemos ordenado el vector, escoge el valor que deseas buscar:"

call search(n, v) !Se llama a la subrutina de busqueda

endif

if (n <= 0) then 
write(*,*) "Un vector no puede tener dimension negativa o nula."
endif
write(*,*) 

read(*,*)
end program 

! La ordenación se ha hecho por selección ('selection sort'), esto es, 
! buscando el valor mínimo e intercambiando la posición.

subroutine sort(dimension, vector)
integer, intent(in) :: dimension
integer, intent(inout) :: vector(dimension)
integer :: i, j
integer :: swap !variable auxiliar

do i = 1, dimension-1
	do j = i+1, dimension
		if (vector(i) > vector(j)) then 
			swap = vector(i)
			vector(i) = vector(j)
			vector(j) = swap
		endif
	enddo
enddo

write(*,*) 
write(*,"(a18, 100(i4, 2x))") "Vector ordenado = ", vector
write(*,*)

end subroutine 


subroutine search(dimension, vector)

integer, intent(in) :: dimension
integer, intent(inout) :: vector(dimension)
integer :: i, k, sup, inf
real :: valor, n

n = dimension
n = n/2

i = dimension/2		
if (n > i .and. n < (i+1)) then	 !Cuando la dimensión sea impar, la variable i tomaría únicamente la parte entera.
i = i + 1						 !Conviene corregirlo y redondear 'i' al entero superior.
endif

read(*,*) valor
write(*,*)

do k = 1, dimension	    !¿Acaso existe ese valor?
if (valor == vector(k)) exit 
enddo

if (k == dimension+1) then   !Si ya sabemos que el valor no está en el vector, nos ahorramos buscarlo.
write(*,*) "El valor buscado no se encuentra entre las componentes del vector."
endif

if (k <= dimension) then

sup = dimension		! Definimos los que van a ser los puntos final e inicial del intervalo.
inf = 0				! Su posicion va a variar según el valor buscado se encuentre a un lado o a otro.

do while (valor /= vector(i))

	if (valor > vector(i)) then
	write(*,"(a45,(i2, 1x))") "El vector esta a la derecha de la posicion   ", i
	inf = i
	n = inf + sup
	n = n/2
	i = (inf + sup)/2
		if (n > i .and. n < (i+1)) then  !Mismo caso que en línea 79. Redondeamos hacia arriba.
		i = i + 1
		endif
	sup = sup
	endif

	if (valor < vector(i)) then
	write(*,"(a45, (i2, 1x))") "El vector esta a la izquierda de la posicion ", i
	sup = i
	n = inf + sup
	n = n/2
	i = (inf + sup)/2
		if (n > i .and. n < (i+1)) then !Mismo caso que en línea 79. Redondeamos hacia arriba.
		i = i + 1
		endif
	inf = inf
	endif
enddo

write(*,*)
write(*,"(a45, (i2, 2x))") "El valor buscado se encuentra en la posicion ", i

endif

write(*,*)
end subroutine 
