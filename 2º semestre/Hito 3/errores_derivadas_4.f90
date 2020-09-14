program errores_4
use modulo_derivadas_4

real(4), parameter :: deltax = 1d-5 									!Incremento
real(4) :: x															!Punto de la función
real(4) :: dpc, error_dpc												!Derivada primera centrada
real(4) :: dpdi, error_dpdi												!Derivada primera descentrada inferior
real(4) :: dpds, error_dpds												!Derivada primera descentrada superior
real(4) :: dpa, error_dpa												!Derivada primera adelantada (según definición)
real(4) :: ds, error_ds													!Derivada segunda

x = 0.0

write(*,*)"El incremento de x es:"
write(*,*) deltax
write(*,*)

call centrada (x, deltax, dpc, error_dpc)
write(*,*) "La derivada primera centrada es:", dpc
write(*,*) "El error, en simple precision, asociado a esta derivada es:", error_dpc
write(*,*)

call des_inf (x, deltax, dpdi, error_dpdi)
write(*,*) "La derivada primera descentrada inferior es:", dpdi
write(*,*) "El error, en simple precision, asociado a esta derivada es:", error_dpdi
write(*,*)

call des_sup (x, deltax, dpds, error_dpds)
write(*,*) "La derivada primera descentrada superior es:", dpds
write(*,*) "El error, en simple precision, asociado a esta derivada es:", error_dpds
write(*,*)

call adelantada (x, deltax, dpa, error_dpa)
write(*,*) "La derivada primera segun la definicion es:", dpa
write(*,*) "El error, en simple precision, asociado a esta derivada es", error_dpa
write(*,*)

call segunda (x, deltax, ds, error_ds)
write(*,*) "La derivada segunda es:", ds
write(*,*) "El error, en simple precision, asociado a esta derivada es:", error_ds

write(*,*)
read(*,*)
end program
