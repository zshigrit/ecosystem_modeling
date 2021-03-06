! leaf photosynthesis coupled with energy balance
program leaf_photosyn
    use LeafPhotosynthesis
    implicit none
    
    real :: vcmax25, jmax25, rd25
    real, parameter :: tfrz=273.15, rgas = 8.31446
    real, parameter :: kc25= 404.9, ko25=278.4, cp25=42.75 
    real, parameter :: kcha = 79430.0, koha = 36380.0, rdha = 46390.0, cpha = 37830.0
    real, parameter :: vcmaxha = 65330.0, jmaxha  = 43540.0
    real, parameter :: rdhd = 150000.0, vcmaxhd = 150000.0, jmaxhd  = 150000.0    
    real, parameter :: rdse = 490.0, vcmaxse = 490.0,jmaxse  = 490.0 
    real, parameter :: phi_psii = 0.85, theta_j = 0.90, colim_c3 = 0.98
    real, parameter :: rho= 0.10, tau= 0.10
    real :: vcmaxc, jmaxc, rdc 
    real :: tleaf=298.15, kc, ko, cp 
    real :: t1,t2,vcmax,jmax,rd 
    real :: apar=1500.0
    real :: qabs,aquad,bquad,cquad,je 
    real :: co2air=380.0, ci, o2air=209.0, ac, aj, ag, an   
    integer :: delta_co2  
    !real :: test 
open(10,file='net_photosyn.dat')
write(10,'(a3,","a2)') 'co2','an'
!write(10,'(a5,",",a2)') [character(6)::'tleaf','an']
do delta_co2 = 0, 200
    co2air = 380.0 + delta_co2
    vcmax25=60.0
    jmax25 = 1.67 * vcmax25
    rd25 = 0.015 * vcmax25
 
    vcmaxc = fth25(vcmaxhd, vcmaxse)
    jmaxc  = fth25(jmaxhd, jmaxse)
    rdc    = fth25(rdhd, rdse)

    !print *, "input leaf temperature: "
    !read *, tleaf 

    kc = kc25 * ft(tleaf, kcha)
    ko = ko25 * ft(tleaf, koha)
    cp = cp25 * ft(tleaf, cpha)

    t1 = ft(tleaf, vcmaxha)
    t2 = fth(tleaf, vcmaxhd, vcmaxse, vcmaxc)
    vcmax = vcmax25 * t1 * t2

    t1 = ft(tleaf, jmaxha)
    t2 = fth(tleaf, jmaxhd, jmaxse, jmaxc)
    jmax = jmax25 * t1 * t2

    t1 = ft(tleaf, rdha)
    t2 = fth(tleaf, rdhd, rdse, rdc)
    rd = rd25 * t1 * t2

    ! --- Electron transport rate for C3 plants

    ! Solve the polynomial: aquad*je^2 + bquad*je + cquad = 0
    
    
   ! print *, 'input leaf absorbed PAR :'
    !read *, apar  
    qabs = 0.5 * phi_psii * apar
    aquad = theta_j
    bquad = -(qabs + jmax)
    cquad = qabs * jmax
    ! for Je. Correct solution is the smallest of the two roots.
    je = (-bquad - sqrt(bquad**2-4*aquad*cquad))/(2*aquad)  
    
    ! specify Ci 
    !print *, 'input CO2AIR and o2air: '
    !read *, co2air, o2air  
    ci = 0.7 * co2air

    ! --- C3: Rubisco-limited photosynthesis

    ac = vcmax * max(ci - cp, 0.0) / (ci + kc * (1 + o2air / ko))
    
    ! --- C3: RuBP regeneration-limited photosynthesis

    aj = je * max(ci - cp, 0.0) / (4 * ci + 8 * cp)

    ! using either colimitaion or the minimum of ac and aj to derive photosynthesis

    aquad = colim_c3;
    bquad = -(ac + aj)
    cquad = ac * aj
 
    ag = (-bquad - sqrt(bquad**2-4*aquad*cquad))/(2*aquad)

    ! ag = min(ac,aj)

    an = ag - rd 

    print *, 'net photosynthesis at leaf level: ', an
    write(10,'(f5.1,",",f7.2)') co2air,an 
    !write(10,'(f5.1,f7.2)') co2air,an 
    !write(10,'(F7.2,X,F5.2)') tleaf, an 
end do 

close(10)

end program leaf_photosyn


