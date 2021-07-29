! leaf photosynthesis coupled with energy balance
program leaf_photosyn
    implicit none
    real :: vcmax25, jmax25, rd25
    real, parameter :: kc25= 404.9, ko25=278.4, cp25=42.75 
    real, parameter :: kcha = 79430.0, koha = 36380.0, rdha = 46390.0, cpha = 37830.0
    real, parameter :: vcmaxha = 65330.0, jmaxha  = 43540.0
    real, parameter :: rdhd = 150000.0, vcmaxhd = 150000.0, jmaxhd  = 150000.0    
    real, parameter :: rdse = 490.0, vcmaxse = 490.0,jmaxse  = 490.0 
    real, parameter :: phi_psii = 0.85, theta_j = 0.90, colim_c3 = 0.98
    real, parameter :: rho= 0.10, tau= 0.10
    real :: vcmaxc, jmaxc, rdc 

    vcmax25=60.0
    jmax25 = 1.67 * vcmax25
    rd25 = 0.015 * vcmax25
 
    vcmaxc = fth25(vcmaxhd, vcmaxse)
    jmaxc  = fth25(jmaxhd, jmaxse)
    rdc    = fth25(rdhd, rdse)

    print *, vcmaxc, jmaxc, rdc
contains
real function fth25(hd,se)

    implicit none
    real, parameter :: tfrz=273.15, rgas = 8.31446
    real :: hd, se 

    fth25 = 1 + exp((-hd + se*(tfrz+25)) / (rgas*(tfrz+25)))
end function fth25

end program leaf_photosyn


