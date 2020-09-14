program desarrollo_sin
	implicit none
	real :: x, suma, suma_2
	integer :: N, k, i, a, fact, signo
	write (*,*) "Introduce un valor para 'N':"
	read (*,*) N
		if (N < 0) then
		write (*,*) "No introduzcas un valor negativo para 'N'."; stop
		end if 
		write (*,*) "Introduce tambien un valor para 'x'."
		read (*,*) x
		suma = 0.0
		suma_2 = 10.0
		k = 1
		a = 0
		do while (abs (suma_2 - suma) > spacing (x) .and. k <= N)
		suma_2 = suma
			do i = 1, k
				fact = 1
				fact = fact*i
				end do
				signo = (-1)**a
			suma = suma + (signo*(x**k))/fact
			k = k + 2
			a = a + 1
		end do
	write (*,*) "El desarrollo en serie de 'sen(x)' es igual a", suma
	write (*,*) "hasta el elemento", N
	write (*,*) "en el punto", x
	write (*,*) ""
	write (*,*) "Pulsa 'enter' para finalizar . . ."
	read (*,*) 
end program  desarrollo_sin
