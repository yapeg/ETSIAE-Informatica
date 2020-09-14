module funcion_MAS
implicit none

contains

  ! Esta función permite calcular de forma aproximada la posción 
  ! que ocupa un cuerpo oscilante, basándose en las leyes de la 
  ! mecánica relativista para el movimiento armónico simple.

  function F(U, t)
	real(8), dimension(:), intent(in) :: U
	real(8), intent(in) :: t
	real(8), dimension(size(U)) :: F
  ! Escoger la constante de elasticidad para el problema:
	real(8), parameter :: k = 15d1
  ! Escoger la masa del cuerpo oscilante 
   	real(8), parameter :: m = 3d0
  ! 'c' es la velocidad de la luz
	real(8), parameter :: c = 3d8

		F(1) = U(2)
		F(2) = (-k/m) * U(1) * (1 - (U(2)/c)**2)**(3./2)

  end function F
  
  
  ! Esta función es la anterior "arreglada", para que sea posible 
  ! calcular su raíz por el método de Euler implícito.
  
  function G(x, U_0, dt)
	real(8), intent(in) :: x
	real(8), dimension(:), intent(in) :: U_0
	real(8), intent(in) :: dt
	real(8) :: G
  ! Escoger la constante de elasticidad para el problema:
	real(8), parameter :: k = 15d1
  ! Escoger la masa del cuerpo oscilante 
   	real(8), parameter :: m = 3d0
  ! 'c' es la velocidad de la luz
	real(8), parameter :: c = 3d8
	
  ! Ecuación despejada para obtener raíz (valor de x)
	G = U_0(2) - dt * (k/m) * (U_0(1) + dt * x) * (1 - (x/c)**2)**3./2 - x
	
  end function G
	
end module
