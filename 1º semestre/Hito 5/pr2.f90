program h5pr2
	implicit none
		integer :: N, D, i, alloc_error, dealloc_error
		real (8) :: a, b, delta_x, pi, x_i, f_x_i
		real (8), allocatable :: f_x(:), x(:)
		pi = 4*atan(1.0)
			write (*,*) "Introduce un valor para 'a', por favor."
			read (*,*) a
				if (a > (-pi/2)) then
				write (*,*) "Introduce un valor para 'a' menor que -pi/2, gracias."; stop
				end if
			write (*,*) "Introduce un valor para 'b', por favor."
			read (*,*) b
				if (b < pi/2) then
				write (*,*) "Introduce un valor para 'b' mayor que pi/2, gracias."; stop
				end if
			write (*,*) "Introduce el numero de particiones en el intervalo (N), por favor."
			read (*,*) N
				if (N <= 0) then
				write (*,*) "'N' no puede ser nulo ni negativo, gracias."; stop
				end if
		delta_x = (b-a)/N
			write (*,*) "Cada intervalo vale", delta_x
		D = N+1
			write (*,*) "La dimension de los vectores es", D
			allocate (f_x(D), x(D), stat = alloc_error)
				if (alloc_error /= 0.0) then 
				write (*,*) "Error en asignacion dinamica."; stop
				end if
		do i = 1, D
		x_i = a + delta_x*(i-1)
			if ((x_i >= a).and.(x_i <= (-pi/2))) then
				f_x_i = 1.0
			elseif ((x_i > (-pi/2)).and.(x_i < pi/2)) then
				f_x_i = cos (pi*x_i) 
			elseif ((x_i >= pi/2).and.(x_i <= b)) then
				f_x_i = 0.0
			end if
		x(i) = x_i
		f_x(i) = f_x_i
		end do
		write (*,*) "Las abscisas definidas por las particiones son"
			write (*,*), x
		write (*,*) "y sus respectivas ordenadas son"
			write (*,*), f_x
			deallocate (f_x, x, stat = dealloc_error)
				if (dealloc_error /= 0.0) then 
				write (*,*) "Error al liberar memoria"; stop
				end if
end program h5pr2
