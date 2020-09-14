program factorial_2
	integer :: n
	integer :: fact
	integer :: i
	write(*,*) "Introduce un numero natural"
	read(*,*) n
	if (n>=0) then
	fact = 1
	do i = 1, (n-1) 
	fact = (fact*i) 
	end do
	fact = fact*n
	write(*,*) "El factorial de n es igual a", fact
	else
	write(*,*) "El numero introducido es incorrecto"
	end if
end program 
	
	
