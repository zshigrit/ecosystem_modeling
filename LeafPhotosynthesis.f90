module LeafPhotosynthesis
    implicit none 
 
    real, private, parameter :: tfrz=273.15, rgas = 8.31446

contains
! function 1: nominator in the thermal breakdown function (in 3)
real function fth25(hd,se)

    real :: hd, se 

    fth25 = 1 + exp((-hd + se*(tfrz+25)) / (rgas*(tfrz+25)))

end function fth25

! function 2: temperature scalar by arrhenius function 
real function ft(tl,ha)

    real :: tl, ha 

    ft = exp(ha/(rgas*(tfrz+25)) * (1-(tfrz+25)/tl))

end function ft 

! function 3: thermal breakdown of biochemical processess
real function fth(tl,hd,se,fc)

    real tl,hd,se,fc

    fth = fc / (1 + exp((-hd+se*tl)/(rgas*tl)))

end function fth 

! function 4: saturation vapor pressure and its change
real(8) function satvap(tc) result(esat)
! Compute saturation vapor pressure and change in saturation vapor pressure
! with respect to temperature. Polynomial approximations are from:
! Flatau et al. (1992) Polynomial fits to saturation vapor pressure.
! Journal of Applied Meteorology 31:1507-1513. Input temperature is Celsius.
implicit none 
real(kind=8), allocatable :: a(:),b(:),c(:),d(:)
real(kind=8) :: tc,desat  

    a = [6.11213476, 0.444007856, 1.43064234e-02, 2.64461437e-04, 0.305903558e-05, & 
        0.196237241e-07, 0.892344772e-10, -0.373208410e-12, 0.209339997e-15]

    b = [0.444017302, 0.286064092e-01, 0.794683137e-03, 0.121211669e-04, 0.103354611e-06, &
        0.404125005e-09, -0.788037859e-12, -0.114596802e-13, 0.381294516e-16]

    c = [6.11123516, 0.503109514, 0.188369801e-01, 0.420547422e-03, 0.614396778e-05, &
        0.602780717e-07, 0.387940929e-09, 0.149436277e-11, 0.262655803e-14]

    d = [0.503277922, 0.377289173e-01, 0.126801703e-02, 0.249468427e-04, 0.313703411e-06, &
        0.257180651e-08, 0.133268878e-10, 0.394116744e-13, 0.498070196e-16]

    tc = min(tc, 100.0)
    tc = max(tc, -75.0)

if (tc >= 0) then 
   esat  = a(1) + tc*(a(2) + tc*(a(3) + tc*(a(4) + tc*(a(5) &
         + tc*(a(6) + tc*(a(7) + tc*(a(8) + tc*a(9))))))))
   desat = b(1) + tc*(b(2) + tc*(b(3) + tc*(b(4) + tc*(b(5) &
         + tc*(b(6) + tc*(b(7) + tc*(b(8) + tc*b(9))))))))
else
   esat  = c(1) + tc*(c(2) + tc*(c(3) + tc*(c(4) + tc*(c(5) &
         + tc*(c(6) + tc*(c(7) + tc*(c(8) + tc*c(9))))))))
   desat = d(1) + tc*(d(2) + tc*(d(3) + tc*(d(4) + tc*(d(5) &
         + tc*(d(6) + tc*(d(7) + tc*(d(8) + tc*d(9))))))))
end if 

! --- Convert from mb to Pa

esat  = esat  * 100
desat = desat * 100

end function satvap

real function CiFunc (vcmax, ci, cp, kc, co2air, o2air, ko, je, rd, gbc)

    real :: vcmax, ci, cp, kc, o2air, ko, je, co2air, gbc  
    real :: ac, aj, ag, an, rd 
    real, parameter :: colim_c3 = 0.98
    real :: aquad, bquad, cquad
    real :: cs
  ! --- C3: Rubisco-limited photosynthesis

    ac = vcmax * max(ci - cp, 0.0) / (ci + kc * (1 + o2air / ko))
    
! --- C3: RuBP regeneration-limited photosynthesis

    aj = je * max(ci - cp, 0.0) / (4 * ci + 8 * cp) 

! using either colimitaion or the minimum of ac and aj to derive photosynthesis

    aquad = colim_c3
    bquad = -(ac + aj)
    cquad = ac * aj
 
    ag = (-bquad - sqrt(bquad**2-4*aquad*cquad))/(2*aquad)

    ! ag = min(ac,aj)

    an = ag - rd 

!     --- CO2 at leaf surface

    cs = co2air - an / gbc
    cs = max(cs, 1.0)

! --- Stomatal constraint function

! Saturation vapor pressure at leaf temperature

[esat, desat] = satvap ((flux.tleaf-physcon.tfrz));

! Ball-Berry stomatal conductance is a quadratic equation
! for gs given An: aquad*gs^2 + bquad*gs + cquad = 0. Correct
! solution is the larger of the two roots. This solution is
! valid for An >= 0. With An <= 0, gs = g0.

if (leaf.gstyp == 1)
   term = flux.an / flux.cs;
   if (flux.an > 0)
      aquad = 1;
      bquad = flux.gbv - leaf.g0 - leaf.g1 * term;
      cquad = -flux.gbv * (leaf.g0 + leaf.g1 * term * atmos.eair / esat);
      pcoeff = [aquad bquad cquad];
      proots = roots(pcoeff);
      flux.gs = max(proots(1), proots(2));
   else
      flux.gs = leaf.g0;
   end
end

% Quadratic equation for Medlyn stomatal conductance

if (leaf.gstyp == 0)
   vpd = max((esat - atmos.eair), 50) * 0.001;
   term = 1.6 * flux.an / flux.cs;
   if (flux.an > 0)
      aquad = 1;
      bquad = -(2 * (leaf.g0 + term) + (leaf.g1 * term)^2 / (flux.gbv * vpd));
      cquad = leaf.g0 * leaf.g0 + (2 * leaf.g0 + term * (1 - leaf.g1 * leaf.g1 / vpd)) * term;
      pcoeff = [aquad bquad cquad];
      proots = roots(pcoeff);
      flux.gs = max(proots(1), proots(2));
   else
      flux.gs = leaf.g0;
   end
end

% --- Diffusion (supply-based) photosynthetic rate

% Leaf CO2 conductance (mol CO2/m2/s)

gleaf = 1 / (1 / flux.gbc + 1.6 / flux.gs);

% Calculate Ci from the diffusion rate

cinew = atmos.co2air - flux.an / gleaf;

% --- Return the difference between the current Ci and the new Ci

if (flux.an >= 0)
   ci_dif = cinew - ci_val;
else
   ci_dif = 0;
end

    

end function CiFunc


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module LeafPhotosynthesis