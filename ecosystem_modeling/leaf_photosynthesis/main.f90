! leaf photosynthesis

program leafphotosyn
    implicit none

    real, parameter :: ha=,hd=,rgas=
    real, parameter :: kc25,
    real :: tleaf,ft,fth,kc
    real :: arrhen 
	
	
	
    fth = fc/(1+exp((-hd+se*tleaf)/(rgas*tleaf)))
	
	kc = kc25 * ft
	

end program leafphotosyn

real function ft(ha,rgas,tfrz,tleaf)
    ft = exp(ha/(rgas*(tfrz+25))*(1-(tfrz+25)/tleaf))
    return
end function ft
