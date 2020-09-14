module function_module
implicit none

contains

function g(x)
real(8), intent(in) :: x
real(8) :: g

g = x**2

end function g

function h(x)
real(8), intent(in) :: x
real(8) :: h

h = exp(x)

end function h

function i(x)
real(8), intent(in) :: x
real(8) :: i

i = 1./x

end function i

function j(x)
real(8), intent(in) :: x
real(8) :: j

j = exp(-x**2)

end function j

end module 
