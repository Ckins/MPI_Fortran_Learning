program main
implicit none 
include 'mpif.h'

integer,parameter :: maxProcedureSize=5
integer,parameter :: rootProcedure=0
integer :: i
integer :: procedureSize
integer :: ierr
integer :: myid 
integer :: totalCounts = 0
integer,allocatable :: sendBuf(:)
integer,allocatable :: recvBuf(:)
integer :: startPos(maxProcedureSize) !各个进程起始位置
integer :: counts(maxProcedureSize)   !各个进程数据长度

call mpi_init(ierr)
call mpi_comm_rank(MPI_COMM_WORLD,myid,ierr)
call mpi_comm_size(MPI_COMM_WORLD,procedureSize)

write(*,*) 'myid:',myid,'total procedures:',procedureSize
allocate(sendBuf(myid+1))


!初始化counts和startPos,只有0号进程初始化这两个数组也是可以的，其他进程不需要计算这两个数组
if(myid == 0) then
	do i=1,procedureSize
		counts(i) = i
		totalCounts = totalCounts + counts(i)
		if(i == 1) then
			startPos(i) = 0
		else
			startPos(i) = startPos(i-1) + counts(i-1)
		end if
	end do
end if

if(myid == 0) allocate(recvBuf(totalCounts))

if(myid == 0) write(*,*) 'total counts is:',totalCounts

!初始化各个进程sendBuf中的数,0号进程中一共有1个数，1号进程2个数..
do i=1,myid+1
	sendBuf(i) = i
end do

call mpi_gatherv(sendBuf,myid+1,MPI_INTEGER,recvBuf,counts,startPos,MPI_INTEGER,rootProcedure,MPI_COMM_WORLD,ierr)

!将recvBuf中的数输出
if(myid == 0) then
	do i=1,totalCounts
		write(*,*) 'recvBuf:',recvBuf(i)
	end do
end if

call mpi_finalize(ierr)
deallocate(sendBuf)
if(myid == 0) deallocate(recvBuf)
stop
end program main


