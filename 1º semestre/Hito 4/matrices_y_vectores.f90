program matrices_y_vectores
implicit none
	integer, parameter :: n=3
	real :: A(n,n)
	real :: U (n,1)
	real :: v(n)
	real :: w(n)
	integer :: B, C
	real :: v_1, v_2, v_3, w_1, w_2, w_3, a_11, a_12, a_13, a_21, a_22, a_23, a_31, a_32, a_33, u_11, u_21, u_31, D, x
		v = (/v_1, v_2, v_3/)
		w = (/w_1, w_2, w_3/)
		A(1,:) = (/a_11, a_12, a_13/)
		A(2,:) = (/a_21, a_22, a_23/)
		A(3,:) = (/a_31, a_32, a_33/)
		U(:,1) = (/u_11, u_21, u_31/)
	write (*,*) "Elige que hacer:"
	write (*,*) "1. Sumar componentes de v"
	write (*,*) "2. Sumar componentes de A"
	write (*,*) "3. Sumar componentes de v mayores que 0"
	write (*,*) "4. Sumar componentes de A menores que 0"
	write (*,*) "5. Hacer producto escalar de los vectores v y w"
	write (*,*) "6. Hacer producto escalar del vector v y la columna N de la matriz A"
	write (*,*) "7. Multiplicar la matriz A por el vector v"
	write (*,*) "8. Escribir la matriz traspuesta de la matriz A"
	write (*,*) "9. Obtener el valor maximo de la matriz A y su posicion"
	read (*,*) B
	if (B==1) then
		write (*,*) "VECTOR v:"
		write (*,*) "Introduce un valor para v_1"
		read (*,*) v_1
		write (*,*) "Introduce un valor para v_2"
		read (*,*) v_2
		write (*,*) "Introduce un valor para v_3"
		read (*,*) v_3
		D = v_1 + v_2 + v_3
		write (*,*) "La suma de los componentes del vector v es igual a", D
	elseif (B==2) then
		write (*,*) "MATRIZ A:"
		write (*,*) "Introduce un valor para a_11"
		read (*,*) a_11
		write (*,*) "Introduce un valor para a_12"
		read (*,*) a_12
		write (*,*) "Introduce un valor para a_13"
		read (*,*) a_13
		write (*,*) "Introduce un valor para a_21"
		read (*,*) a_21
		write (*,*) "Introduce un valor para a_22"
		read (*,*) a_22
		write (*,*) "Introduce un valor para a_23"
		read (*,*) a_23
		write (*,*) "Introduce un valor para a_31"
		read (*,*) a_31
		write (*,*) "Introduce un valor para a_32"
		read (*,*) a_32
		write (*,*) "Introduce un valor para a_33"
		read (*,*) a_33
		D = a_11 + a_12 + a_13 + a_21 + a_22 + a_23 + a_31 + a_32 + a_33
		write (*,*) "La suma de los elementos de la matriz A es igual a", D
	else if (B==3) then
		write (*,*) "VECTOR v:"
		write (*,*) "Introduce un valor para v_1"
		read (*,*) v_1
		write (*,*) "Introduce un valor para v_2"
		read (*,*) v_2
		write (*,*) "Introduce un valor para v_3"
		read (*,*) v_3
		if (v_1<=0) then
		v_1 = 0
		end if
		if (v_2<=0) then
		v_2 = 0
		end if
		if (v_3<=0) then
		v_3 = 0
		end if
		D = v_1 + v_2 + v_3
		write (*,*) "La suma de los componentes de v mayores que cero es igual a", D
	else if (B==4) then
		write (*,*) "MATRIZ A:"
		write (*,*) "Introduce un valor para a_11"
		read (*,*) a_11
		write (*,*) "Introduce un valor para a_12"
		read (*,*) a_12
		write (*,*) "Introduce un valor para a_13"
		read (*,*) a_13
		write (*,*) "Introduce un valor para a_21"
		read (*,*) a_21
		write (*,*) "Introduce un valor para a_22"
		read (*,*) a_22
		write (*,*) "Introduce un valor para a_23"
		read (*,*) a_23
		write (*,*) "Introduce un valor para a_31"
		read (*,*) a_31
		write (*,*) "Introduce un valor para a_32"
		read (*,*) a_32
		write (*,*) "Introduce un valor para a_33"
		read (*,*) a_33
		if (a_11>=0) then
		a_11 = 0
		end if
		if (a_12>=0) then
		a_12 = 0
		end if
		if (a_13>=0) then
		a_13 = 0
		end if
		if (a_21>=0) then
		a_21 = 0
		end if
		if (a_22>=0) then
		a_22 = 0
		end if
		if (a_23>=0) then
		a_23 = 0
		end if
		if (a_31>=0) then
		a_31 = 0
		end if
		if (a_32>=0) then
		a_32 = 0
		end if
		if (a_33>=0) then
		a_33 = 0
		end if
		D = a_11 + a_12 + a_13 + a_21 + a_22 + a_23 + a_31 + a_32 + a_33
		write (*,*) "La suma de todos los elementos de la matriz A menores que cero es igual a", D
	else if (B==5) then
		write (*,*) "VECTOR v:"
		write (*,*) "Introduce un valor para v_1"
		read (*,*) v_1
		write (*,*) "Introduce un valor para v_2"
		read (*,*) v_2
		write (*,*) "Introduce un valor para v_3"
		read (*,*) v_3
		write (*,*) "VECTOR w:"
		write (*,*) "Introduce un valor para w_1"
		read (*,*) w_1
		write (*,*) "Introduce un valor para w_2"
		read (*,*) w_2
		write (*,*) "Introduce un valor para w_3"
		read (*,*) w_3
		D = v_1*w_1 + v_2*w_2 + v_3*w_3
		write (*,*) "El valor del producto escalar entre los vectores v y w es igual a", D
	else if (B==6) then
		write (*,*) "VECTOR v:"
		write (*,*) "Introduce un valor para v_1"
		read (*,*) v_1
		write (*,*) "Introduce un valor para v_2"
		read (*,*) v_2
		write (*,*) "Introduce un valor para v_3"
		read (*,*) v_3
		write (*,*) "MATRIZ A:"
		write (*,*) "Introduce un valor para a_11"
		read (*,*) a_11
		write (*,*) "Introduce un valor para a_12"
		read (*,*) a_12
		write (*,*) "Introduce un valor para a_13"
		read (*,*) a_13
		write (*,*) "Introduce un valor para a_21"
		read (*,*) a_21
		write (*,*) "Introduce un valor para a_22"
		read (*,*) a_22
		write (*,*) "Introduce un valor para a_23"
		read (*,*) a_23
		write (*,*) "Introduce un valor para a_31"
		read (*,*) a_31
		write (*,*) "Introduce un valor para a_32"
		read (*,*) a_32
		write (*,*) "Introduce un valor para a_33"
		read (*,*) a_33
		write (*,*) "Elige columna de la matriz A"
		write (*,*) "1. Columna 1"
		write (*,*) "2. Columna 2"
		write (*,*) "3. Columna 3"
		read (*,*) C
			if (C==1) then
			D = v_1*a_11 + v_2*a_21 + v_3*a_31
			write (*,*) "El valor del producto escalar entre el vector v y la primera columna de la matriz A es igual a", D
			elseif (C==2) then
			D = v_1*a_12 + v_2*a_22 + v_3*a_32
			write (*,*) "El valor del producto escalar entre el vector v y la segunda columna de la matriz A es igual a", D
			elseif (C==3) then
			D = v_1*a_13 + v_2*a_23 + v_3*a_33
			write (*,*) "El valor del producto escalar entre el vector v y la tercera columna de la matriz A es igual a", D
			elseif (C<=0) then
			write (*,*) "Escoge o 1 o 2 o 3. Ahora vuelve a empezar."
			elseif (C>=4) then
			write (*,*) "Escoge o 1 o 2 o 3. Ahora vuelve a empezar."
			end if
	else if (B==7) then
		write (*,*) "MATRIZ A:"
		write (*,*) "Introduce un valor para a_11"
		read (*,*) a_11
		write (*,*) "Introduce un valor para a_12"
		read (*,*) a_12
		write (*,*) "Introduce un valor para a_13"
		read (*,*) a_13
		write (*,*) "Introduce un valor para a_21"
		read (*,*) a_21
		write (*,*) "Introduce un valor para a_22"
		read (*,*) a_22
		write (*,*) "Introduce un valor para a_23"
		read (*,*) a_23
		write (*,*) "Introduce un valor para a_31"
		read (*,*) a_31
		write (*,*) "Introduce un valor para a_32"
		read (*,*) a_32
		write (*,*) "Introduce un valor para a_33"
		read (*,*) a_33
		write (*,*) "VECTOR v:"
		write (*,*) "Introduce un valor para v_1"
		read (*,*) v_1
		write (*,*) "Introduce un valor para v_2"
		read (*,*) v_2
		write (*,*) "Introduce un valor para v_3"
		read (*,*) v_3
		write (*,*) "Multiplicando la matriz A por el vector v se obtiene una matriz columna U que tiene por componentes:"
		u_11 = a_11*v_1 + a_12*v_2 + a_13*v_3
		u_21 = a_21*v_1 + a_22*v_2 + a_23*v_3
		u_31 = a_31*v_1 + a_32*v_2 + a_33*v_3
		write (*,*) "u_11 = ", u_11
		write (*,*) "u_21 = ", u_21
		write (*,*) "u_31 = ", u_31
	else if (B==8) then
		write (*,*) "MATRIZ A:"
		write (*,*) "Introduce un valor para a_11"
		read (*,*) a_11
		write (*,*) "Introduce un valor para a_12"
		read (*,*) a_12
		write (*,*) "Introduce un valor para a_13"
		read (*,*) a_13
		write (*,*) "Introduce un valor para a_21"
		read (*,*) a_21
		write (*,*) "Introduce un valor para a_22"
		read (*,*) a_22
		write (*,*) "Introduce un valor para a_23"
		read (*,*) a_23
		write (*,*) "Introduce un valor para a_31"
		read (*,*) a_31
		write (*,*) "Introduce un valor para a_32"
		read (*,*) a_32
		write (*,*) "Introduce un valor para a_33"
		read (*,*) a_33
		write (*,*) "La matriz traspuesta de A tiene por componentes:"
		write (*,*) "at_11 = ", a_11
		write (*,*) "at_12 = ", a_21
		write (*,*) "at_13 = ", a_31
		write (*,*) "at_21 = ", a_12
		write (*,*) "at_22 = ", a_22
		write (*,*) "at_23 = ", a_32
		write (*,*) "at_31 = ", a_13
		write (*,*) "at_32 = ", a_23
		write (*,*) "at_33 = ", a_33
	else if (B==9) then
		write (*,*) "MATRIZ A:"
		write (*,*) "Introduce un valor para a_11"
		read (*,*) a_11
		write (*,*) "Introduce un valor para a_12"
		read (*,*) a_12
		write (*,*) "Introduce un valor para a_13"
		read (*,*) a_13
		write (*,*) "Introduce un valor para a_21"
		read (*,*) a_21
		write (*,*) "Introduce un valor para a_22"
		read (*,*) a_22
		write (*,*) "Introduce un valor para a_23"
		read (*,*) a_23
		write (*,*) "Introduce un valor para a_31"
		read (*,*) a_31
		write (*,*) "Introduce un valor para a_32"
		read (*,*) a_32
		write (*,*) "Introduce un valor para a_33"
		read (*,*) a_33
		A(1,:) = (/a_11, a_12, a_13/)
		A(2,:) = (/a_21, a_22, a_23/)
		A(3,:) = (/a_31, a_32, a_33/)
		x = maxval(A)
		write (*,*) "El maximo valor de la matriz A es", x
	else if (B<=0) then
		write (*,*) "Opcion no valida"
	else if (B>=10) then
		write (*,*) "Opcion no valida"
	end if
end program
	
