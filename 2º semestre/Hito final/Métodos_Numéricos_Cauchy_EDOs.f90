!  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!	 El módulo siguiente contiene métodos numéricos para la resolución
!  	 de ecuaciones diferenciales ordinarias. Los métodos propuestos son:
!	 	- Método de Euler explícito
!		- Método de Adams-Bashforth
!		- Método de Runge-Kutta
!		- Método de Euler implícito
!  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _  
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

module Metodos_Numericos_Cauchy_EDOs
use Funcion_MAS
implicit none

contains

!  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

  subroutine Euler_Explicito(dt, t, U, F)
	real(8), intent(in) :: dt
	real(8), intent(in) :: t
	real(8), dimension(:), intent(inout) :: U

	interface
	function F(U, t)
		real(8), dimension(:), intent(in) :: U
		real(8), intent(in) :: t
		real(8), dimension(size(U)) :: F
	end function
	end interface
	real(8), dimension(size(U)) :: U_0

		U_0 = U
		
		U = U_0 + dt * F(U_0, t)

  end subroutine Euler_Explicito
  
!  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

  subroutine Adams_Bashforth(dt, t, U, F)
	real(8), intent(in) :: dt
	real(8), intent(in) :: t
	real(8), dimension(:), intent(inout) :: U

	interface
	function F(U, t)
		real(8), dimension(:), intent(in) :: U
		real(8), intent(in) :: t
		real(8), dimension(size(U)) :: F
	end function
	end interface
	real(8), dimension(size(U)) :: U_0, F_0
	real(8), save, allocatable, dimension(:) :: F_00

		U_0 = U
		F_0 = F(U_0, t)
		
		if (t == dt) then
		! Primer paso con Euler
		  U = U_0 + dt * F_0
		  allocate(F_00(size(U)))
		else
		! Pasos restantes con Adams-Bashforth
		  U = U_0 + (dt/2) * (3*F_0 - F_00)
		end if
		
		F_00 = F_0
		

  end subroutine Adams_Bashforth
  
!  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

  subroutine Runge_Kutta(dt, t, U, F)
	real(8), intent(in) :: dt
	real(8), intent(in) :: t
	real(8), dimension(:), intent(inout) :: U

	interface
	function F(U,t)
		real(8), dimension(:), intent(in) :: U
		real(8), intent(in) :: t
		real(8), dimension(size(U)) :: F
	end function
	end interface
	real(8), dimension(size(U)) :: U_0
	real(8), dimension(size(U)) :: K1, K2

		U_0 = U
		K1 = F(U_0, t)
		K2 = F(U_0 + dt*K1, t + dt)
		
		U = U_0 + (dt/2) * (K1 + K2)

  end subroutine Runge_Kutta
  
!  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

  subroutine Euler_Implicito(dt, t, U, F)
	real(8), intent(in) :: dt
	real(8), intent(in) :: t
	real(8), dimension(:), intent(inout) :: U
	
	interface
	function F(x, U, dt)
	  real(8), intent(in) :: x
	  real(8), dimension(:), intent(in) :: U
	  real(8), intent(in) :: dt
	  real(8) :: F
	end function
	end interface

	real(8), dimension(size(U)) :: U_0
  ! Tolerancia para el cálculo de raíces por el método de Newton-Raphson
	real(8), parameter :: cota_error = 1d-6
  ! Incremento utilizado para la derivada adelantada
    real(8), parameter :: dx = 1d-4

	U_0 = U
	
		call NewtonRaphson_R(U_0, dx, dt, cota_error, F = G)	
	U(2) = U_0(2)
	U(1) = U_0(1) + dt * U(2)
		
  end subroutine Euler_Implicito
  
  
	  subroutine NewtonRaphson_R(U, dx, dt, cota_error, F)
		real(8), dimension(:), intent(inout) :: U
		real(8), intent(in) :: dx
		real(8), intent(in) :: dt
		real(8), intent(in) :: cota_error		
		real(8), dimension(size(U)) :: U_00
		real(8) :: x
		
		interface
		function F(x, U, dt)
		  real(8), intent(in) :: x
		  real(8), dimension(:), intent(in) :: U
		  real(8), intent(in) :: dt
		  real(8) :: F
		end function
		end interface
		
			U_00 = U
			x = U(2) + 1d-2

			do while (abs(x - U(2)) > cota_error)
			x = U(2)
			U(2) = x - (F(x, U_00, dt))/(derivada(x, U_00, dx, dt, F = G))
			enddo

	  end subroutine NewtonRaphson_R


			function derivada(x, U, dx, dt, F)
			  real(8), intent(in) :: x
			  real(8), dimension(:), intent(in) :: U
			  real(8), intent(in) :: dx
			  real(8), intent(in) :: dt
  			  real(8) :: derivada
  			  
  			  interface
  			  function F(x, U, dt)
				real(8), intent(in) :: x
				real(8), dimension(:), intent(in) :: U
				real(8), intent(in) :: dt
				real(8) :: F
			  end function
			  end interface
			  
				  derivada = (1/dx)*(F(x+dx, U, dt) - F(x, U, dt))

			end function derivada
						

end module

