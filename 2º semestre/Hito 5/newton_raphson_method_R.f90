program ceros_ecuaciones_no_lineales_newton_raphson_R
use function_root_search_module_R
implicit none

real(8) :: x_0
real(8) :: deltax
real(8) :: cota_error
integer :: iter_max

write(*,*) "Punto de aproximacion a la solucion?"
read(*,*) x_0
write(*,*)

deltax = 1d-7
cota_error = 1d-7
iter_max = 1d5

call newton_raphson_R(x = x_0, f = i, dx = deltax, cota_error = cota_error, iter_max = iter_max)

write(*,*)
write(*,*) "La raiz de la funcion se encuentra en el punto 'x':"
write(*,*)
write(*,*) " ", x_0
write(*,*) 
	
write(*,*)
write(*,*) "Y el numero de iteraciones necesarias ha sido:"
write(*,*)
write(*,*) " ", iter_max
write(*,*)

read(*,*)
end program 
