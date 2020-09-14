program h5pr1
	implicit none
	integer :: i, j, k, N, alloc_error, dealloc_error
	real (8) :: suma
	real (8), allocatable :: A(:,:), B(:,:), C(:,:), D(:,:)
		write (*,*) "Introduce la dimension para las matrices."
		read (*,*) N
		allocate (A(N,N), B(N,N), C(N,N), D(N,N), stat = alloc_error)
			if (alloc_error /= 0) then
			write (*,*) "Error en asignacion dinamica"; stop     
			end if
		write (*,*) "Asigna valores a los elementos de la matriz 'A'."
		do i = 1, N
			do j = 1, N
			 write (*,*) "Posicion", i, j
				read (*,*) A(i,j)
			end do
		end do
		write (*,*) "Asigna valores a los elementos de la matriz 'B'."
		do i = 1, N
			do j = 1, N
			 write (*,*) "Posicion", i, j
				read (*,*) B(i,j)
			end do
		end do 
		do i = 1, N
			do j = 1, N 
			suma = 0e0
				do k = 1, N
				suma = suma + A(i,k)*B(k,j)        
				end do
			C(i,j) = suma
			end do
		end do
		write (*,*) "Producto de A y B (ordenado de arriba a abajo y de izquierda a derecha):", C
		write (*,*) ""
		D = matmul(A,B)
		write (*,*) "Comprobacion del producto de A y B con funcion intrinseca:", D
		deallocate (A, B, C, D, stat = dealloc_error)
			if (dealloc_error /= 0.0) then 
			write (*,*) "Error al liberar memoria"; stop
			end if
end program h5pr1
