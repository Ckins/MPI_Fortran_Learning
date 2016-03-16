program main
implicit none  
include 'mpif.h'  
integer myid, ierr,status(MPI_STATUS_SIZE)  
real(kind=8) i, j  
i = 3.14159  
j = 4.22222  
call MPI_INIT(ierr)  
call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)  
if(myid.eq.0) then  
   call MPI_Send(i, 1, MPI_DOUBLE_PRECISION, 1, 99, MPI_COMM_WORLD,IERR)  
   print *,'sending',i  
else  
   call MPI_Recv(j, 1, MPI_DOUBLE_PRECISION, 0, 99,MPI_COMM_WORLD,STATUS,IERR)  
   print *, 'receiving',j  
endif  
   call MPI_FINALIZE(ierr)  
end program main 
