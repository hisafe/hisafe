! *****************************************************
! /**
!  * calcul de la macroporosite
!  *
!  * @param P_codefente
!  * @param da
!  * @param hcc
!  * @param hmin
!  * @param P_epc
!  *
!  * @return: un réel
!  */
!> calculation of the macroporosity
real function macroporosite(P_codefente,da,hcc,hmin,P_epc)

  ! les paramètres entrants
  integer, intent(IN) :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 
  real,    intent(IN) :: da   !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real,    intent(IN) :: hcc  
  real,    intent(IN) :: hmin  
  real,    intent(IN) :: P_epc   !> // PARAMETER // Thickness of the horizon H   (table) // cm   // PARSOL // 1 	  // OUTPUT // Thickness of the horizon 2 // cm  
  
  real :: macroporositecod0  !>  
  real ::  macroporositecod1  
  
  if (P_codefente == 1) then
    macroporosite = macroporositecod1(da, hcc, hmin, P_epc)
  else
  ! la macroporosité des sols gonflants est proportionnelle à la RU
    macroporosite = macroporositecod0(da, hcc, P_epc)
  endif
  
end function





! /**
!  * calcul de la macroporosite pour le cas P_codefente désactivé (0 ou 2)
!  *
!  * @param da
!  * @param hcc
!  * @param P_epc
!  *
!  * @return: un réel
!  */
real function macroporositecod0(da, hcc, P_epc)

! les paramètres entrants
  real, intent(IN) :: da   !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real, intent(IN) :: hcc  
  real, intent(IN) :: P_epc    !> // PARAMETER // Thickness of the horizon H   (table) // cm   // PARSOL // 1   // OUTPUT // Thickness of the horizon 2 // cm

  macroporositecod0 = ((1.0 - da / 2.66) - (hcc / 100. * da)) * 10. * P_epc
  
end function


! /**
!  * calcul de la macroporosite pour le cas P_codefente activé (1)
!  *
!  * @param da
!  * @param hcc
!  * @param hmin
!  * @param P_epc
!  *
!  * @return: un réel
!  */

real function macroporositecod1(da,hcc,hmin,P_epc)

  ! les paramètres entrants
  real, intent(IN) :: da      !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real, intent(IN) :: hcc  
  real, intent(IN) :: hmin  
  real, intent(IN) :: P_epc    !> // PARAMETER // Thickness of the horizon H   (table) // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon 2 // cm
  
  ! la macroporosité des sols gonflants est proportionnelle à la RU
  macroporositecod1 = 0.5 * (hcc - hmin) / 100. * da * 10. * P_epc
  
end function
 
 
