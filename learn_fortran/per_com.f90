program percom

	implicit none
	integer*8, external :: per, comm, com
	integer*8 :: n,r,per1,com1,com2
	
	read *, n, r
	
	per1 = per(n,r)
	com1 = com(per1,r)
	com2 = comm(n,r)
	
	print *, per1,per2

end percom
