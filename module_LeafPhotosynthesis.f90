module LeafPhotosynthesis
    implicit none 

contains

real function fth25(hd,se)

    implicit none
    real, parameter :: tfrz=273.15, rgas = 8.31446
    real :: hd, se 

    fth25 = 1 + exp((-hd + se*(tfrz+25)) / (rgas*(tfrz+25)))

end function fth25

real function ft(tl,ha)

    implicit none
    real, parameter :: tfrz=273.15, rgas = 8.31446
    real :: tl, ha 

    ft = exp(ha/(rgas*(tfrz+25)) * (1-(tfrz+25)/tl))

end function ft 

real function fth(tl,hd,se,fc)
    implicit none
    real, parameter :: tfrz=273.15, rgas = 8.31446
    real tl,hd,se,fc

    fth = fc / (1 + exp((-hd+se*tl)/(rgas*tl)))

end function fth 



end module LeafPhotosynthesis