program integral_rectangulo
use function_module
use integral_module
implicit none

real(8) :: a_1, b_1, deltax_1
real(8) :: integral_1

write(*,*) "Escoge el extremo inferior del intervalo:"
read(*,*) a_1
write(*,*)
write(*,*) "Escoge el extremo superior del intervalo:"
read(*,*) b_1
write(*,*)
write(*,*) "Escoge el incremento de 'x' para el problema:"
read(*,*) deltax_1
write(*,*)

call rectangulo (a = a_1, b = b_1, f = g, deltax = deltax_1, integral = integral_1)

write(*,*) "El valor de la integral es:"
write(*,*) abs(integral_1)

write(*,*)
read(*,*)
end program 
