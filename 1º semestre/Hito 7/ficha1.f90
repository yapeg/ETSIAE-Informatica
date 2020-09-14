program ficha1
	implicit none
	integer :: i
	real :: A(5,5), B(5,3), U(5), V(5), W(5), T(5), C(5,2), salt_camp, salt_camp_2
	open (unit = 11, file = "input_1.dat")
	open (unit = 22, file = "output_1.dat")
		do i = 1, 5
			read(11,*) A(i,:)
		end do
		rewind (unit = 11)
			read(11,*) U(:)
			read(11,*) V(:)
			read(11,*) W(:)
			read(11,*) T(:)
		write(22,*) "A = "
		do i = 1, 5
			write(22, "(10(f9.5))") a(i,:)
		end do
		write (22, "(a3, 10(f9.5))") "U = ", U
		write (22, "(a3, 10(f9.5))") "V = ", V 
		write (22, "(a3, 10(f9.5))") "W = ", W
		write (22, "(a3, 10(f9.5))") "T = ", T
		rewind (unit = 11)
			read (11,*) T(:)
		rewind (unit = 11)
		do i = 1, 5
			read(11,*) salt_camp, B(i,:)
		end do
		write (22, "(a3, 10(f9.5))") "T = ", T
		write(22,*) "B = "
		do i = 1, 5
			write(22, "(10(f9.5))") B(i,:)
		end do
		rewind (unit = 11)
		do i = 1, 5
			read(11,*) salt_camp, U(i), salt_camp_2,C (i,:)
		end do
		write(22, "(a3, 10(f9.5))") "U = ", U
		write(22,*) "C = "
		do i = 1, 5
			write(22, "(10(f9.5))") C(i,:)
		end do
		rewind (unit = 11)
		do i = 1, 5
			read(11,*) B(i,1), B(i,2), salt_camp, B(i,3)
		end do
		write(22,*) "B = "
		do i = 1, 5
			write(22, "(10(f9.5))") B(i,:)
		end do
	close (unit = 11)
	close (unit = 22)
end program     
     


