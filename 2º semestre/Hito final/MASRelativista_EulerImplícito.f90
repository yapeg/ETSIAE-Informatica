program MASRelativista_Euler_Implicito
use metodos_numericos_Cauchy_EDOs
use funcion_MAS
implicit none

	real(8), parameter :: a = 0d0
  ! Final del intervalo
	real(8) :: b
  ! Espaciamiento temporal
	real(8) :: dt
	real(8), allocatable :: t(:)
  ! Vector que contiene posición y velocidad del problema
	real(8), dimension(2) :: U	
	integer :: i, n
  ! Posición inicial
	real(8), parameter :: x_0 = 0d0 
  ! Velocidad inicial
	real(8), parameter :: v_0 = 5d0	
  ! Aceleración
    real(8) :: ac
  ! Datos del problema (han de coincidir con los dados en el módulo-función)
    real(8), parameter :: m = 3d0 		! Masa
    real(8), parameter :: k = 15d1		! Constante elástica
    real(8), parameter :: c = 3d8 		! Velocidad de la luz


		write(*,*) "Escoja el momento en que desea conocer la posicion &
		y velocidad del problema:"
		read(*,*) b
		write(*,*)

			if (b <= a) stop

		write(*,*) "Escoja ademas el espaciamiento temporal aproximado que se &
		utilizara en el problema planteado:"
		read(*,*) dt
		write(*,*)
		
			if (dt <= 0d0) stop

		n = (b - a)/dt
		dt = (b - a)/n
		allocate (t(n))

		write(*,*) "El incremento de tiempo para el problema sera:", dt
		write(*,*)

	! Condiciones iniciales
	U = (/x_0, v_0/)
	! De estas condiciones iniciales, se deduce la aceleración inicial
	ac = (-k/m) * x_0 * (1 - (v_0/c)**2)**(3./2)

	write(*,*) " __________________________________________________________________________________________________________________&
	_________________________"
	write(*,*)
	write(*,*) "     'Tiempo'				      'Posicion' 			      'Velocidad' 			      	     'Aceleracion' "
	write(*,*) " __________________________________________________________________________________________________________________&
	_________________________"	
	write(*,*)
	write(*,*) a, "		", x_0, "		", v_0, "		", -ac

	do i = 1, n
	t(i) = dt * i

		call Euler_Implicito(dt, t(i), U, F = G)
		ac = (-k/m) * U(1) * (1 - (U(2)/c)**2)**(3./2)
	
	write(*,*) t(i), "		", U(1), "		", U(2), "		", ac
	end do

write(*,*)
read(*,*)
end program 
