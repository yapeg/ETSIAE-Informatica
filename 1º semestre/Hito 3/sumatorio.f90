program sumatorio
	integer :: n
	integer :: suma
	integer :: i
	write(*,*) "Introduce un numero natural"
	read(*,*) n
	if (n>=0) then
	suma = 0
	do i= 1, n
	suma = suma + i
	end do
	write(*,*) "La suma de los n primeros numeros naturales es", suma 
	else
	write(*,*) "El numero introducido es incorrecto"
	end if
end program sumatorio  																	
																										
