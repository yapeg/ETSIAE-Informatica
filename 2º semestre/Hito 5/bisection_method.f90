program bisection_method
use function_root_search_module_R
implicit none

real(8) :: a_0, b_0, x
real(8) :: cota_error
integer :: iter_max

!Elección de extremos de intervalo "aproximada" a la función (intentar que contenga la raíz)
write(*,*) "Extremo inferior del intervalo?"
read(*,*) a_0
write(*,*)
write(*,*) "Extremo superior del intervalo?"
read(*,*) b_0
write(*,*)

iter_max = 1d5
cota_error = 1d-7

call bisection(a = a_0, b = b_0, x = x, f = i, cota_error = cota_error, iter_max = iter_max)

write(*,*) "La funcion tiene una raiz en el punto x =", x
write(*,*)
write(*,*) "Y el numero de iteraciones necesarias ha sido:", iter_max
write(*,*)

read(*,*)
end program

