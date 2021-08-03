module LeafPhotosynthesis
    implicit none 
 
    real, private, parameter :: tfrz=273.15, rgas = 8.31446

contains

real function fth25(hd,se)

<<<<<<< HEAD
    implicit none
    
=======
    real, parameter :: tfrz=273.15, rgas = 8.31446
>>>>>>> 9f5413660e112d1438b26d324f1e116f2d1aedb9
    real :: hd, se 

    fth25 = 1 + exp((-hd + se*(tfrz+25)) / (rgas*(tfrz+25)))

end function fth25

real function ft(tl,ha)

<<<<<<< HEAD
    implicit none
    
=======
    real, parameter :: tfrz=273.15, rgas = 8.31446
>>>>>>> 9f5413660e112d1438b26d324f1e116f2d1aedb9
    real :: tl, ha 

    ft = exp(ha/(rgas*(tfrz+25)) * (1-(tfrz+25)/tl))

end function ft 

real function fth(tl,hd,se,fc)
<<<<<<< HEAD
    implicit none
    
=======
    
    real, parameter :: tfrz=273.15, rgas = 8.31446
>>>>>>> 9f5413660e112d1438b26d324f1e116f2d1aedb9
    real tl,hd,se,fc

    fth = fc / (1 + exp((-hd+se*tl)/(rgas*tl)))

end function fth 

real function CiFunc (physcon, atmos, leaf, flux, ci_val)

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

    % --- CO2 at leaf surface

flux.cs = atmos.co2air - flux.an / flux.gbc;
flux.cs = max(flux.cs, 1);

% --- Stomatal constraint function

% Saturation vapor pressure at leaf temperature

[esat, desat] = satvap ((flux.tleaf-physcon.tfrz));

% Ball-Berry stomatal conductance is a quadratic equation
% for gs given An: aquad*gs^2 + bquad*gs + cquad = 0. Correct
% solution is the larger of the two roots. This solution is
% valid for An >= 0. With An <= 0, gs = g0.

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


end module LeafPhotosynthesis