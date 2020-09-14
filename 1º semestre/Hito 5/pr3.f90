program h5pr3
	implicit none
		integer :: a, b, delta_x, delta_x_2, i, j, N, D, N_2, D_2, alloc_error, dealloc_error
		real (8) :: pi, x_i, x_j, f_x_i, f_x_j, resto
		real (8), allocatable :: f_x(:), x(:), f_x_2(:), x_2(:)
		pi = 4*atan(1.0)
			write (*,*) "Introduce un valor para 'a', por favor."
				read (*,*) a
					if (a>(-pi/2)) then
					write (*,*) "Introduce un valor para 'a' menor que -pi/2, gracias."; stop
					end if
			write (*,*) "Introduce un valor para 'b', por favor."
				read (*,*) b
					if (b<pi/2) then
					write (*,*) "Introduce un valor para 'b' mayor que pi/2, gracias."; stop
					end if
			write (*,*) "Introduce un valor para 'delta_x', que es la longitud de las particiones, por favor."
				read (*,*) delta_x
					if (delta_x>(b-a)) then
					write (*,*) "La longitud de los particiones no puede ser superior al propio intervalo (a,b), gracias."; stop
					end if
		N = (b-a)/delta_x
			write (*,*) "El numero de particiones iguales es", N
		D = N + 1
			write (*,*) "La dimension de los vectores es", D
			allocate (f_x(D), x(D), stat = alloc_error)
				if (alloc_error /= 0.0) then
				write (*,*) "Error en asignacion dinamica."; stop
				end if
		do i = 1, D
		x_i = a + delta_x*(i-1)
				if ((x_i>=a).and.(x_i<=(-pi/2))) then
				f_x_i = 1.0
				elseif ((x_i>(-pi/2)).and.(x_i<(pi/2))) then
				f_x_i = cos (pi*x_i)
				elseif ((x_i>=pi/2).and.(x_i<=b)) then
				f_x_i = 0.0
				end if
		x(i) = x_i
		f_x(i) = f_x_i
		end do
		write (*,*) "Las abscisas definidas por las particiones son"
			write (*,*), x
		write (*,*) "y sus respectivas ordenadas son"
			write (*,*), f_x
			
		resto = (b - a) - N*delta_x
			if (resto /= 0.0) then
					write (*,*) ""
					write (*,*) "Como el numero de particiones iguales ha de ser entero, existe una, &
					la ultima, diferente y de longitud, en este caso,", resto
					write (*,*) ""
					write (*,*) "Aunque cualquier punto de este 'resto' del vector tendra logicamente por &
					ordenada cero, podemos partir este nuevo segmento en tantas partes como unidades tenga, &
					de modo que el nuevo incremento de 'x' sera 1."
				delta_x_2 = 1.0
					write (*,*) ""
					write (*,*) "Con este nuevo dato ya podremos dividir el intervalo en 'M' espacios de &
					igual dimension."
				N_2 = resto + delta_x*N
					write (*,*) ""
					write (*,*) "El numero de particiones diferira respecto al obtenido en un principio. &
					En este caso,", N_2
				D_2 = N_2 + 1
					write (*,*) "y la nueva dimension de los vectores sera", D_2
					allocate (f_x_2(D_2), x_2(D_2), stat = alloc_error)
				do j = 1, D_2
				x_j = a + delta_x_2*(j-1)
					if ((x_j >= a).and.(x_j <= (-pi/2))) then
					f_x_j = 1.0
					else if ((x_j > (-pi/2)).and.(x_j < pi/2)) then
					f_x_j = cos (pi*x_j)
					else if ((x_j > pi/2).and.(x_j <= b)) then
					f_x_j = 0.0
					end if
				x_2(j) = x_j
				f_x_2(j) = f_x_j
				end do
				write (*,*) "Las abscisas definidas por las nuevas particiones son"
					write (*,*), x_2
				write (*,*) "y sus respectivas ordenadas son"
					write (*,*), f_x_2
			end if
					deallocate (x, f_x, x_2, f_x_2, stat = dealloc_error)
						if (dealloc_error /= 0.0) then 
						write (*,*) "Error al liberar memoria"; stop
						end if
end program h5pr3


