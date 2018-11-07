!======================================================================================!
!======================================================================================!
!======================================================================================!
real function calcul_UDev(temp,P_tdmax,P_tdmin,P_tcxstop)

  real, intent(IN) :: temp  
  real, intent(IN) :: P_tdmax  !> // PARAMETER // Maximum threshold temperature for development // degree C // PARPLT // 1
  real, intent(IN) :: P_tdmin  !> // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
  real, intent(IN) :: P_tcxstop  !> // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1

  real :: udev ! pour alléger l'écriture, utilisation variable temporaire udev  

    udev = max(0.,temp - P_tdmin)
    if (P_tcxstop >= 100.0) then
      if (temp > P_tdmax) udev = P_tdmax - P_tdmin
    else
      if (temp > P_tdmax) then
        udev = (P_tdmax - P_tdmin) / (-P_tcxstop + P_tdmax) * (temp - P_tcxstop)
        udev = max(0.,udev)
      endif
    endif

    calcul_UDev = udev ! on affecte la valeur de retour

return
end

!======================================================================================!
!======================================================================================!
!======================================================================================!
!> calculating the hourly temperatures
function calcul_TemperaturesHoraires(tmin, tmin_demain, tmax)

  real, intent(IN)               :: tmin            !> arg. Température minimum  	  // OUTPUT // Minimum active temperature of air // degree C
  real, intent(IN)               :: tmin_demain     !> arg. Température minimum du lendemain  
  real, intent(IN)               :: tmax            !> arg. Température maximum  	  // OUTPUT // Maximum active temperature of air // degree C

  real, dimension(24)            :: calcul_TemperaturesHoraires ! variable de retour  

  integer :: ih ! locale  

  do ih = 1,12
    calcul_TemperaturesHoraires(ih) = tmin + (ih * (tmax - tmin)/12.0)
  end do

  do ih = 13,24
    calcul_TemperaturesHoraires(ih) = tmax - ((ih-12) * (tmax - tmin_demain)/12.0)
  end do

return
end

!======================================================================================!
!======================================================================================!
!======================================================================================!
!> calculating the hourly temperatures
real function calcul_GDH(thor,P_tdmin,P_tdmax)

  real, intent(IN), dimension(24) :: thor  ! arg. Temperatures Horaires sur 24 heures  
  real, intent(IN)                :: P_tdmin ! arg. Température de développement minimum 	  // PARAMETER // Minimum threshold temperature for development // degree C // PARPLT // 1
  real, intent(IN)                :: P_tdmax ! arg. Température de développement maximum 	  // PARAMETER // Maximum threshold temperature for development // degree C // PARPLT // 1

  integer :: ih       ! locale  
  real :: udh         ! locale  

    calcul_GDH = 0.
    do ih = 1,24
      udh = thor(ih) - P_tdmin
      if (thor(ih) < P_tdmin) udh = 0.0
      if (thor(ih) > P_tdmax) udh = P_tdmax - P_tdmin
      calcul_GDH = calcul_GDH + udh
    end do

return
end


!======================================================================================!
!> slowly effect of the photoperiod on plant development
real function cRFPI(P_sensiphot,P_phosat,P_phobase,phoi)

  real, intent(IN) :: P_sensiphot  !> // PARAMETER //  photoperiod sensitivity (1=insensitive) // SD // PARPLT // 1 
  real, intent(IN) :: P_phosat  !> // PARAMETER // saturating photoperiod // hours // PARPLT // 1 
  real, intent(IN) :: P_phobase  !> // PARAMETER // Base photoperiod  // hours // PARPLT // 1 
  real, intent(IN) :: phoi   !> // OUTPUT // Photoperiod // hours

    cRFPI = (1.0 - P_sensiphot) / (P_phosat - P_phobase) * (phoi - P_phosat) + 1.0
    cRFPI = min(cRFPI,1.0)
    cRFPI = max(cRFPI,P_sensiphot)

return
end function cRFPI

!======================================================================================!
! fonction de calcul de l'humidité correspondant à un potentiel du sol
! utile pour la germination
! NB 15/02 2007
real function humpotsol(P_psihucc,P_psihumin,humin,hucc,dacouche,psiref,P_codefente)

!: ARGUMENTS
  real,    intent(IN) :: P_psihucc  !> // PARAMETER // soil potential corresponding to field capacity  // Mpa // PARAM // 1 
  real,    intent(IN) :: P_psihumin  !> // PARAMETER // soil potential corresponding to wilting point // Mpa // PARAM // 1 
  real,    intent(IN) :: hucc  
  real,    intent(IN) :: humin  
  real,    intent(IN) :: dacouche  
  real,    intent(IN) :: psiref  
  integer, intent(IN) :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 

!: Variables locales
  real :: bpsisol  
  real :: psisols  
  real :: wsat  

  ! Calcul des paramètres de la courbe de rétention
    if (P_codefente == 1) then
      wsat = ((1.5 * hucc) - (0.5 * humin)) / 10.
    else
      wsat = 1. - (dacouche / 2.66)
    endif

    bpsisol = log(P_psihucc / P_psihumin) / log(humin / hucc)
    psisols = P_psihumin * ((humin / (wsat *10 ))**bpsisol)

  ! Calcul de l'humidité
    humpotsol = wsat * 10. * ((psiref / psisols)**(-1/bpsisol))

return
end function humpotsol
 
 
