program desarrollo_e
	implicit none
	real :: x, suma, suma_2
	integer :: N, k, i, fact
		write (*,*) "Introduce un valor para 'N':"
		read (*,*) N
		if (N < 0) then
		write (*,*) "No introduzcas un valor negativo para 'N'."; stop
		end if
		write (*,*) "Introduce tambien un valor para 'x':"
		read (*,*) x
		suma = 10.0
		k = 0
		suma_2 = 0.0
		do while (abs (suma - suma_2) > spacing (x) .and. k <= N)
			suma_2 = suma
			if (k == 0) then
			suma = 0.0
			fact = 1
			else
				suma = suma
				fact = 1
				do i = 1, k
				fact = fact*i
				end do 
			end if
			suma = suma + (x**k)/fact 
			k = k + 1
		end do
	write (*,*) "El desarrollo en serie de 'e^x' es igual a", suma
	write (*,*) "hasta el elemento", N
	write (*,*) "en el punto", x
	write (*,*) ""
	write (*,*) "Pulsa 'enter' para finalizar . . ."
	read (*,*)
	
end program desarrollo_e
