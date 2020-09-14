! Recursive Fortran 95 quicksort routine
! sorts real numbers into ascending numerical order
! Author: Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03
! Based on algorithm from Cormen et al., Introduction to Algorithms,
! 1997 printing

! Made F conformant by Walt Brainerd


program sortdriver
  use qsort_c_module													!Usa módulo
  implicit none
  integer :: r										
  integer :: i
  real :: random
  real, allocatable :: array(:)											!Vector con dimensión por asignar
  
  write(*,*) "Dimension del vector?"
  read(*,*) r
  write(*,*)
  
  allocate(array(r))													!Se asigna dimensión al vector
  
  do i = 1, r															!Ahora se asignan valores aleatorios por componentes
  call random_number(random)											!(Esto será especialmente efectivo cuando tratemos...
  array(i) = random														! ...con vectores de grandes dimensiones
  enddo
  
  write(*,"(a24, 20(f10.5, 2x))") "Vector desordenado:", array(:)
  
  call QsortC(array)													!Se llama a la subrutina de ordenación
  
  write(*,*)
  write(*,"(a24, 20(f10.5, 2x))") "Vector ordenado:", array(:)
   
   write(*,*)
end program sortdriver


module qsort_c_module

implicit none
private :: Partition													!Esta sentencia no será accesible desde fuera del módulo

contains

recursive subroutine QsortC(A)											!Subrutina recursiva (que se va a repetir)
  real, intent(in out), dimension(:) :: A
  integer :: iq

  if(size(A) > 1) then
     call Partition(A, iq)												!Subrutina interna que partirá el vector dado...
     call QsortC(A(:iq-1))												! ... y los recurrentes
     call QsortC(A(iq:))
  endif
end subroutine QsortC

subroutine Partition(A, marker)
  real, intent(in out), dimension(:) :: A
  integer, intent(out) :: marker
  integer :: i, j
  real :: temp
  real :: x      														!Valor pivote
  x = A(1)																!Se escoge cualquiera (en este programa, el primero)
  i= 0
  j= size(A) + 1

  do                                                                    !Se van comparando los valores en distintas posiciones...
     j = j-1															! ...e intercambiándose cuando los valores de la derecha...
     do          														! ...sean mayores que el pivote, y los de la izquierda, menores
        if (A(j) <= x) exit
        j = j-1
     end do
     i = i+1
     do
        if (A(i) >= x) exit
        i = i+1
     end do
     if (i < j) then													!cambio de A(i) por A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
     elseif (i == j) then
        marker = i+1
        return
     else
        marker = i
        return
     endif
  end do

end subroutine Partition

end module qsort_c_module


