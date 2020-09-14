program ficha2
	implicit none
	integer :: i
	real :: A(7,7), B(7,3), U(7), V(7), W(7), T(7), C(7,2), salt_camp, salt_camp_2
	open (unit = 33, file = "input_2.dat")
	open (unit = 44, file = "output_2.dat")
		do i = 1, 7
			read(33,*) A(i,:)
		end do
		rewind (unit = 33)
		read(33,*) U(:)
		read(33,*) V(:)
		read(33,*) W(:)
		read(33,*) T(:)
		write(44,*) "A = "
		do i = 1, 7
			write(44, "(10(f9.5))") a(i,:)
		end do
		write (44, "(a3, 10(f9.5))") "U = ", U
		write (44, "(a3, 10(f9.5))") "V = ", V 
		write (44, "(a3, 10(f9.5))") "W = ", W
		write (44, "(a3, 10(f9.5))") "T = ", T
		rewind (unit = 33)
		read (33,*) T(:)
		rewind (unit = 33)
		do i = 1, 7
			read(33,*) salt_camp, B(i,:)
		end do
		write (44, "(a3, 10(f9.5))") "T = ", T
		write(44,*) "B = "
		do i = 1, 7
			write(44, "(10(f9.5))") B(i,:)
		end do
		rewind (unit = 33)
		do i = 1, 7
			read(33,*) salt_camp, U(i), salt_camp_2, C(i,:)
		end do
		write(44, "(a3, 10(f9.5))") "U = ", U
		write(44,*) "C = "
		do i = 1, 7
			write(44, "(10(f9.5))") C(i,:)
		end do
		rewind (unit = 33)
		do i = 1, 7
			read(33,*) B(i,1), B(i,2), salt_camp, B(i,3)
		end do
		write(44,*) "B = "
		do i = 1, 7
			write(44, "(10(f9.5))") B(i,:)
		end do
	close (unit = 33)
	close (unit = 44)
end program 
