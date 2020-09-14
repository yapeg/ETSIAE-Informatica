program sumatorio_impares
	integer :: n
	integer :: suma
	integer :: i
	write(*,*) "Introduce un numero natural"
	read(*,*) n
	if (n>=0) then
	suma = 0
	do i = 1, n
	suma = suma + 2*i-1
	end do
	write(*,*) "La suma de los n primeros numeros naturales impares es", suma
	else
	write(*,*) "El numero introducido es incorrecto"
	end if
end program sumatorio_impares
																								
