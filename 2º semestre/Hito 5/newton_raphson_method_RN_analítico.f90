program RaicesNewtonRaphson_Analitico
use function_root_search_module_RN
implicit none

	! Cambiar parámetro (dimensión problema) según función escogida
	integer, parameter :: m = 2
	real(8), dimension(m) :: x_0
	real(8), parameter :: deltax = 1d-5
	real(8), parameter :: cota_error = 1d-7
	integer :: iter_max
		
	! Es necesario que el punto a escoger sea próximo al resultado
	write(*,*) "Escoge un punto de aproximacion a la solucion:"
	read(*,*) x_0
	write(*,*)

	
	iter_max = 1d5
	! Cambiar 'f' y 'J_ana' según función escogida
	call AnalitycalNewtonRaphsonMethod_RN(x = x_0, f = i, J_ana = J_i, dx = deltax, cota_error = cota_error, iter_max = iter_max)
	
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
