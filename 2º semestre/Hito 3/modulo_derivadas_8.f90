module modulo_derivadas_8

contains

subroutine centrada (x, deltax, dpc, error_dpc)
real(8), intent (in):: x, deltax
real(8), intent (out):: dpc, error_dpc									!Derivada primera centrada

dpc = (1/(2*deltax))*(exp(x+deltax) - exp(x-deltax))
error_dpc = abs(1 - dpc)
end subroutine

subroutine des_inf (x, deltax, dpdi, error_dpdi)						
real(8), intent (in):: x, deltax
real(8), intent (out):: dpdi, error_dpdi								!Derivada primera descentrada inferior 

dpdi = (1/(2*deltax))*(-3*exp(x) + 4*exp(x+deltax) - exp(x+2*deltax))
error_dpdi = abs(1 - dpdi)
end subroutine

subroutine des_sup (x, deltax, dpds, error_dpds)
real(8), intent (in):: x, deltax
real(8), intent (out):: dpds, error_dpds								!Derivada primera descentrada superior

dpds = (1/(2*deltax))*(exp(x-2*deltax) - 4*exp(x-deltax) + 3*exp(x))
error_dpds = abs(1 - dpds)
end subroutine

subroutine adelantada (x, deltax, dpa, error_dpa)
real(8), intent (in):: x, deltax
real(8), intent (out):: dpa, error_dpa									!Derivada primera adelantada (según definición)

dpa = (1/deltax)*(exp(x+deltax) - exp(x))
error_dpa = abs(1 - dpa)
end subroutine

subroutine segunda (x, deltax, ds, error_ds)
real(8), intent (in):: x, deltax
real(8), intent (out):: ds, error_ds									!Derivada segunda

ds = (1/(deltax**2))*(exp(x+deltax) - 2*exp(x) + exp(x-deltax))
error_ds = abs(1 - ds)
end subroutine


end module 
