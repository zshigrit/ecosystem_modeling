! leaf photosynthesis coupled with energy balance
program leaf_photosyn
    implicit none
    real :: vcmax25, jmax25, rd25 



    vcmax25=60.0
    jmax25 = 1.67 * vcmax25
    rd25 = 0.015 * vcmax25

end program leaf_photosyn
