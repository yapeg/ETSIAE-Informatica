program ficha3
implicit none

integer::i, Ierr
real, allocatable:: A(:,:)
real, allocatable:: B(:,:)
real, allocatable:: U(:)
real, allocatable:: V(:)
real, allocatable:: W(:)
real, allocatable:: T(:)
real, allocatable:: C(:,:)
real:: salt_camp, salt_camp_2, n
write(*,*) 'introduce las dimensiones con un solo número'
read (*,*) n

open (unit=33,file="input_2.dat")
open (unit=44, file="output_2.dat")
allocate (A(n,n),stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
allocate (B(n,3),stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
allocate (C(n,2),stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
allocate (U(n),stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
allocate (V(n),stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
allocate (W(n),stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
allocate (T(n),stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'

do i=1,n
 read(33,*) A(i,:)
end do
rewind (unit=33)
read(33,*) U(:)
read(33,*) V(:)
read(33,*) W(:)
read(33,*) T(:)
write(44,*) "A= "
do i=1, n
write(44,"(10(f9.5))") a(i,:)
end do
write (44,"(a3,10(f9.5))") "U= ", U
write (44,"(a3,10(f9.5))") "V= ", V 
write (44,"(a3,10(f9.5))") "W= ", W
write (44,"(a3,10(f9.5))") "T= ", T

rewind (unit=33)
read (33,*) T(:)
rewind (unit=33)
do i=1,n
     read(33,*) salt_camp,B(i,:)
end do
write (44,"(a3,10(f9.5))") "T= ", T
write(44,*) "B= "
do i=1, n
write(44,"(10(f9.5))") B(i,:)
end do

rewind (unit=44)
do i=1,n
     read(33,*) salt_camp,U(i),salt_camp_2,C(i,:)
end do
write(44,"(a3,10(f9.5))") "U= ", U
write(44,*) "C= "
do i=1, n
write(44,"(10(f9.5))") C(i,:)
end do

rewind (unit=33)
do i=1, n
read(33,*) B(i,1), B(i,2), salt_camp, B(i,3)
end do
write(44,*) "B= "
do i=1, n
write(44,"(10(f9.5))") B(i,:)
end do
deallocate (A, stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
deallocate (B, stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
deallocate (C, stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
deallocate (U, stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
deallocate (V, stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
deallocate (W, stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
deallocate (T, stat=Ierr)
if (Ierr>0) stop '***no hay suficiente memoria***'
close (unit=33)
close (unit=44)

end program
