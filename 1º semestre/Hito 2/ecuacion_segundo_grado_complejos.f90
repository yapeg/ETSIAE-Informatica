program ecuacion_segundo_grado_complejos
	implicit none
	real:: a, b, c, D
	complex:: z_1, z_2
		write (*,*) "Introduce valores de a, b, c."
		read (*,*) a
		read (*,*) b
		read (*,*) c
		D = (b**2)-(4*a*c)
	if (D<0.0) then
		z_1 = complex (-b,(-D)**(1./2))
		z_1 = z_1/(2*a)
		z_2 = complex (-b,-(-D)**(1./2))
		z_2 = z_2/(2*a)
		write (*,*) "z_1=", z_1
		write (*,*) "z_2=", z_2
	end if
end program ecuacion_segundo_grado_complejos																															
