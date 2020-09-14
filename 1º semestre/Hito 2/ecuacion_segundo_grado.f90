program ecuacion_segundo_grado																																																																		
	implicit none 
	real :: a, b, c, D, x, x_1, x_2
		write (*,*) "Introduce valores de a,b,c."
		read (*,*), a
		read (*,*), b
		read (*,*), c
		D = ((b**2)-(4*a*c))
	if (a==0.0) then
		x = -(c/b)
		write (*,*) "x=", x
	elseif (D>0.0) then
		x_1 = (1/(2*a))*(-b+(D)**(1/2.))
		x_2 = (1/(2*a))*(-b-(D)**(1/2.))
		write (*,*) "x_1=", x_1
		write (*,*) "x_2=", x_2
	elseif (D==0.0) then
		x_1 = (-b)/(2*a)
		x_2 = (-b)/(2*a)
		write (*,*) "x_1=", x_1
		write (*,*) "x_2=", x_2
	end if
end program ecuacion_segundo_grado	


																																																																											
																																												
