! calcul du rayonnement atmosphérique (Brutsaert)
! rapport global/global extra-terrestre
!      rg en MJm-2,pm en mbars, tm en degree C, Ratm en MJm-2
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 6.6.1.b, page 114
!>
!! This subroutine calculates the atmospheric radiation (Ratm) and the emissivity of the atmosphere (emissa) thanks to Brutsaert’s formula (1982)
! ------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine ratmo(rg,P_aangst,P_bangst,P_latitude,jul,tpm,tmoy,Ratm)

USE Divers, only: RGEX

  implicit none

!: Arguments
  real,    intent(IN)    :: rg  
  real,    intent(IN)    :: P_aangst  !> // PARAMETER // coefficient of the Angstrom's relationship for extraterrestrial radiation // SD // STATION // 1 
  real,    intent(IN)    :: P_bangst  !> // PARAMETER // coefficient of the Angstrom's relationship for extraterrestrial radiation // SD // STATION // 1 
  real,    intent(IN)    :: P_latitude  !> // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0 
  integer, intent(IN)    :: jul  
  real,    intent(IN)    :: tpm   !> // OUTPUT // Vapour pressure in air // mbars
  real,    intent(IN)    :: tmoy   !> // OUTPUT // Mean active temperature of air // degree C

  real,    intent(OUT)   :: Ratm   !> // OUTPUT // Atmospheric radiation  // Mj.m-2

!: Variables locales
  real :: nuage  
  real :: fracinsol  
  real :: sigma  
  real :: RsRso  
  real :: EaBrut  
  real :: emissa  


      sigma =  5.67e-8

      RsRso = rg / RGEX(P_latitude / 180 *3.14, jul)
      fracinsol = (RsRso - P_aangst) / P_bangst
      fracinsol = max(fracinsol, 0.0)
      fracinsol = min(fracinsol, 1.0)

      ! calcul de l'ennuagement moyen
      nuage = 1.0 - fracinsol

      ! calcul du rayonnement atmosphérique
      EaBrut = 1.24 * (tpm / (tmoy + 273.15))**(1.0 / 7.0)

      emissa = Eabrut + nuage * (1.0 - EaBrut) * (1.0 - 4.0 * 11.0 / (273.15 + tmoy))

      Ratm = sigma * emissa * (tmoy + 273.15)**4.0
      Ratm = Ratm * 3600.0 *24.0 * 1e-6

return
end subroutine ratmo
 
 
