program desarrollo_fraccion
	implicit none
	real :: x, suma, suma_2
	integer ::  N, k
		write (*,*) "Introduce un valor para 'N':"
		read (*,*) N
			if (N < 0) then
			write (*,*) "No introduzcas valores negativos para 'N'."; stop
			end if
		write (*,*) "Introduce tambien un valor para 'x':"
		read (*,*) x
			if (x <= -1 .or. x >= 1) then
			write (*,*) "El valor introducido para 'x' debe estar comprendido en &
			el intervalo (-1, 1), de modo que el desarrollo sea asi valido."; stop
			end if
		suma = 0.0
		suma_2 = 10.0
		k = 0
		do while (abs (suma_2 - suma) > spacing (x) .and. k <= N)
		suma_2 = suma
		suma = suma + x**k
		k = k + 1
		end do
		write (*,*) "El desarrollo en serie de '1/(1-x)' es igual a", suma
		write (*,*) "hasta el elemento", N
		write (*,*) "en el punto", x
		write (*,*) ""
		write (*,*) "Pulsa 'enter' para finalizar . . ."
		read (*,*)
end program desarrollo_fraccion
		
