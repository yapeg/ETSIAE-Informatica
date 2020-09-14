program factorial
	integer :: n
	integer(8):: fact 
	integer :: i
	write(*,*) "Introduce un numero natural"
	read(*,*) n
	if (n>=0) then
	fact = 1
	do i = 1, n
	fact = fact*i
	end do
	write(*,*) "El factorial de n es igual a", fact
	else
	write(*,*) "El numero introducido es incorrecto"
	end if
end program factorial
																								
	
