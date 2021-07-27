! leaf photosynthesis coupled with energy balance
program leaf_photosyn
    implicit none
    real :: vcmax25, jmax25, rd25
    real, parameter :: kc25= 404.9, ko25=278.4, cp25=42.75 
    real, parameter :: kcha = 79430, koha = 36380, rdha = 46390, cpha = 37830, vcmaxha = 65330, jmaxha  = 43540  

    vcmax25=60.0
    jmax25 = 1.67 * vcmax25
    rd25 = 0.015 * vcmax25

    print *, jmaxha

end program leaf_photosyn
