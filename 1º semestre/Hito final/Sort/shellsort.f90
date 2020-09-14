module shellsort
 
contains
 
subroutine shell_sort(a)
 
  implicit  none
  integer :: i, j, increment
  real :: temp
  real, intent(inout) :: a(:)
 
  increment = size(a)/2
  do while (increment > 0)
      do i = increment+1, size(a)
         j = i
         temp = a(i)
         do while (j >= increment+1 .AND. a(j-increment) > temp)
            a(j) = a(j-increment)
            j = j - increment
         enddo
         a(j) = temp
      enddo
      if (increment == 2) then
   	  increment = 1
      else
         increment = increment * 5 / 11
      end if      
  enddo
 
end subroutine shell_sort
 
end module shellsort
 
program shell
 
use shellsort
 
  implicit none
  real, allocatable :: array(:)
  integer :: dimension
  
  write(*,*) "Dimension del vector?"
  read(*,*) dimension
  
  allocate (array(dimension))
 
  call random_seed
  call random_number(array)
 

  write(*,*)
  write(*,*) "Vector desordenado:"
  write(*,"(20(f9.6, 3x))"), array
  
  call shell_Sort(array)
  
  write(*,*)
  write(*,*) "Vector ordenado:"
  write(*,"(20(f9.6, 3x))"), array
 

write(*,*)
read(*,*)
end program shell
