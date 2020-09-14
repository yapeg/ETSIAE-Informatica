! --------------------------------------------------------------------
! PROGRAM  Sorting:
!    Este programa permite ordenar una serie de números. Este método es conocido 
! como ordenamiento por selección.
! --------------------------------------------------------------------

PROGRAM  Sorting
   implicit  NONE
   INTEGER, PARAMETER :: MAX_SIZE = 100
   INTEGER, DIMENSION(1:MAX_SIZE) :: InputData
   INTEGER                        :: ActualSize
   INTEGER                        :: i
   
   WRITE(*,*) "Escribe la dimension del vector:"
   READ(*,*)  ActualSize 
   WRITE(*,*)
   
   IF (ActualSize > 0) THEN
   WRITE(*,*) "Escribe el vector:"
   READ(*,*) (InputData(i), i = 1, ActualSize)
   WRITE(*,*)
   WRITE(*,*) "Vector original:"
   WRITE(*,"(100(3x, i3))") (InputData(i), i = 1, ActualSize)
   
   CALL  Sort(InputData, ActualSize)

   WRITE(*,*)
   WRITE(*,*) "Vector ordenado:"
   WRITE(*,"(100(3x, i3))") (InputData(i), i = 1, ActualSize)
   ENDIF
   
   IF (ActualSize <= 0) THEN
   WRITE(*,*) "La dimension de un vector no puede ser nula o negativa."
   ENDIF
  
   WRITE(*,*) 
CONTAINS

! --------------------------------------------------------------------
! INTEGER FUNCTION  FindMinimum():
!    This function returns the location of the minimum in the section
! between Start and End.
! --------------------------------------------------------------------

   INTEGER FUNCTION  FindMinimum(x, Start, End)
      IMPLICIT  NONE
      INTEGER, DIMENSION(1:), INTENT(IN) :: x
      INTEGER, INTENT(IN)                :: Start, End
      INTEGER                            :: Minimum
      INTEGER                            :: Location
      INTEGER                            :: i

      Minimum  = x(Start)		! assume the first is the min
      Location = Start			! record its position
      DO i = Start+1, End		! start with next elements
         IF (x(i) < Minimum) THEN	!   if x(i) less than the min?
            Minimum  = x(i)		!      Yes, a new minimum found
            Location = i                !      record its position
         END IF
      END DO
      FindMinimum = Location        	! return the position
   END FUNCTION  FindMinimum

! --------------------------------------------------------------------
! SUBROUTINE  Swap():
!    This subroutine swaps the values of its two formal arguments.
! --------------------------------------------------------------------

   SUBROUTINE  Swap(a, b)
      IMPLICIT  NONE
      INTEGER, INTENT(INOUT) :: a, b
      INTEGER                :: Temp

      Temp = a
      a    = b
      b    = Temp
   END SUBROUTINE  Swap

! --------------------------------------------------------------------
! SUBROUTINE  Sort():
!    This subroutine receives an array x() and sorts it into ascending
! order.
! --------------------------------------------------------------------

   SUBROUTINE  Sort(x, Size)
      IMPLICIT  NONE
      INTEGER, DIMENSION(1:), INTENT(INOUT) :: x
      INTEGER, INTENT(IN)                   :: Size
      INTEGER                               :: i
      INTEGER                               :: Location

      DO i = 1, Size-1			! except for the last
         Location = FindMinimum(x, i, Size)	! find min from this to last
         CALL  Swap(x(i), x(Location))	! swap this and the minimum
      END DO
   END SUBROUTINE  Sort

END PROGRAM  Sorting
