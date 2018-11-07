! *--------------------------------------* c
! * version 5.0                          * c
! * calcul du rayonnemnt net le 11/06    * c
! *--------------------------------------* c
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This subroutine calculates the net radiation.
!> - Stics book paragraphe 6.6.1, page 113-114
!>
!! Net radiation takes account of the surface albedo (albedolai) applied to solar radiation (trg) and long wave radiation (rglo).
!! The albedo of the surface (albedolai) varies between the soil value (albsol) and the vegetation value (albveg) which is equal to =0.23 (Ritchie, 1985).
!! The soil albedo (albsol) varies as a function of soil type (albedo of dry soil), moisture in the surface layer, and the presence of any plastic or plant cover.
!! It decreases linearly with the water content of the surface layer (hur) according to a relationship established from experimental results obtained for
!! different types of soil (hucc and humin being the water content at field capacity and wilting point respectively).
!!
!!
!! Long wave radiation:
!!
!! Two formula are proposed to calculate long wave radiation (rglo in MJ m-2) based on crop temperature (tcult in °C), the insolation fraction (fracinsol) and
!! the vapour pressure (tpm in mbars). Brunt’s formula (1932), is used in many applications in particular in Penman’s potential evapotranspiration formula (1948),
!! while Brutsaert’s formula (1982) is supposed to be more precise (Guyot, 1997). It illustrates clearly the soil and atmospheric components of rglo
!! using the Stefan-Boltzman law and the emissivity of the atmosphere (see the module ratmo.f90).
!!
!! The insolation fraction (fracinsol) is estimated using Angström's formula, the parameters of which are aangstg=0.18 and bangstg=0.62.
!! Extraterrestrial radiation (rgex) is calculated using standard astronomic formulae (Grebet, 1993)in the module rgex.f90. If the vapour pressure is not available,
!! it is estimated as the saturated vapour pressure at the temperature tpm = TVAR(tmin - corecTrosee).
!!
!! The saturated vapour pressure/temperature function is in the module tvar.f90.
!!
!! The order of magnitude of the parameter corecTrosee is of a few degrees, from 0 for the wettest locations to 3°C for the driest ones.
!!
!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
!!!Ajout cellTrg et cellVisibleSky
! ------------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine calrnet(nbCouches,jul,hur,humin,hucc,lai,tauxcouv,posibsw,Ratm,     & ! IN
                   P_albedo,couvermulch,albedomulch,P_albveg,P_codernet,P_aangst,P_bangst,tmin,   &
                   tmoy,P_corecTrosee,trg,P_latitude,tutilrnet,P_codepaillage,P_codelaitr,      &
                   tpm,P_codecaltemp,albedolai,rglo,rnet, cellTrg, cellVisibleSky)                                   ! INOUT

USE Divers, only: TVAR, RGEX

  implicit none
  
!: Arguments

  integer, intent(IN)    :: nbCouches  
  integer, intent(IN)    :: jul  
  real,    intent(IN)    :: hur(nbCouches)  
  real,    intent(IN)    :: humin(nbCouches)  
  real,    intent(IN)    :: hucc(nbCouches)   
  real,    intent(IN)    :: lai                   ! laiTMP    // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: tauxcouv   !> // OUTPUT // Cover rate // SD
  logical, intent(IN)    :: posibsw  
  real,    intent(IN)    :: Ratm     !> // OUTPUT // Atmospheric radiation  // Mj.m-2
  real,    intent(IN)    :: P_albedo  !> // PARAMETER // P_albedo of the bare dry soil // SD // PARSOL // 1 
  real,    intent(IN)    :: couvermulch   !> // OUTPUT // Cover ratio of mulch  // 0-1
  real,    intent(IN)    :: albedomulch
  real,    intent(IN)    :: P_albveg  !> // PARAMETER // P_albedo of the vegetation // SD // STATION // 1 
  integer, intent(IN)    :: P_codernet  !> // PARAMETER // option of calculation of net radiation // code 1/2/3 // STATION // 0 
  real,    intent(IN)    :: P_aangst  !> // PARAMETER // coefficient of the Angstrom's relationship for extraterrestrial radiation // SD // STATION // 1 
  real,    intent(IN)    :: P_bangst    !> // PARAMETER // coefficient of the Angstrom's relationship for extraterrestrial radiation // SD // STATION // 1 
  real,    intent(IN)    :: tmin   !> // OUTPUT // Minimum active temperature of air // degree C
  real,    intent(IN)    :: tmoy   !> // OUTPUT // Mean active temperature of air // degree C
  real,    intent(IN)    :: P_corecTrosee  !> // PARAMETER // temperature to substract to Tmin to estimate dew point teñperature (in case of missing air humidity data) // degree C // STATION // 1
  real,    intent(IN)    :: trg   !> // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
  real,    intent(IN)    :: P_latitude  !> // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0 
  real,    intent(IN)    :: tutilrnet   
  integer, intent(IN)    :: P_codepaillage    !> // PARAMETER // option: 1 = no cover, 2 = plastic cover partly covering the soil   // code 1/2 // PARTEC // 0
  integer, intent(IN)    :: P_codelaitr  !> // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0 
  
  real,    intent(INOUT) :: tpm   !> // OUTPUT // Vapour pressure in air // mbars
  integer, intent(INOUT) :: P_codecaltemp  !> // PARAMETER // option of use of crop temperature for phasic development calculation : yes (2), no (1)  // code 1/2 // STATION // 0 
  real,    intent(INOUT) :: albedolai   !> // OUTPUT // P_Albedo of the crop cobining soil with vegetation // SD
  real,    intent(INOUT) :: rglo  
  real,    intent(INOUT) :: rnet   !> // OUTPUT // Net radiation  // MJ m-2

  real,     intent(IN)   :: cellTrg              !> // INPUT // Global radiation // %
  real,     intent(IN)   :: cellVisibleSky      !> // INPUT // Visible sky // %

!: Variables locales
  real    :: albsol  !>  
  real    :: albsolhum  !>  
  real    :: fracinsol  !>  
  real    :: pi  !>  
  real    :: Rsolglo  !>  
  real    :: RsRso  !>  
  real    :: sigma  

       
      sigma =   5.67e-8
      pi = atan(1.0)*4
       
      
      
      !: IMPORTANT :
      !- la température utilisée est <tutilrnet> 


      !: Calcul de l'albédo
      !- albsolhum = 0.483 * P_albedo (sec) selon données G. Richard
      albsolhum = 0.483 * P_albedo
 !write(71,*)'P_albedo',P_albedo  ,albsolhum
      !: Introduction de l'humidité du sol
      ! NB le 9/01/2006
      ! reformulation de l'P_albedo du sol
      albsol = (albsolhum - P_albedo) / (hucc(1) - humin(1)) * (hur(1) - humin(1)) + P_albedo            
! write(71,*)'albsol',albsol  ,albsolhum,hucc(1),humin(1),hur(1)

      if (albsol < albsolhum) albsol = albsolhum
      if (albsol > P_albedo) albsol = P_albedo

      !: Pondération par l'P_albedo du mulch
      !- par défaut le paramètre technique P_codepaillage est identique
      !- pour toutes les cultures de la simulation
      if (P_codepaillage /= 1) then
        albsol = albsol * (1 - couvermulch) + albedomulch * couvermulch
      endif
!write(71,*)'calrnet',couvermulch,albedomulch,albsol
      ! ** introduction du LAI ou du taux de couverture
      ! *- P_albveg = 0.23 en paramètre le 02/02/02
      ! *- en mode CAS, le paramètre P_codelaitr doit être identique pour toutes
      ! *- les cultures. On peut donc se baser sur le paramètre de la plante dominante
      ! *- on fait la somme des LAI des cultures (AO et AS)
      if (P_codelaitr == 1) then
        albedolai =  P_albveg - (P_albveg - albsol) * exp(-0.75 * lai)
      else
        albedolai =  P_albveg - (P_albveg - albsol) * (1 - tauxcouv)
      endif


      !: Calcul du rayonnement net
      !!!MODIF HISAFE 12 : Modif après détection bug
      !!!MODIF HISAFE on change la façon de faire le test
      !!!if (.not.posibsw) then
      if (posibsw) then
      else
        !: P_codernet = 3
        tpm = TVAR(tmin - P_corecTrosee)
        P_codecaltemp = 1
      endif
      
      
      ! ** utilisation des rayonnements nets de Shuttle & Wall


      !: Calcul du rayonnement net en MJm-2j-1 selon Brunt (stefce avec P_albedo variable)
      if (P_codernet == 1) then
        RsRso = trg / rgex(P_latitude / 180 * 3.14, jul)
        fracinsol = (RsRso - P_aangst) / P_bangst
        fracinsol = max(fracinsol, 0.0)
        fracinsol = min(fracinsol, 1.0)
        rglo = 4.9e-9 * ((tutilrnet + 273.16)**4) * (0.1 + 0.9 * fracinsol) * (0.56 - .08 * sqrt(tpm))
        rnet = (1 - albedolai) * trg - Rglo
      endif
      

      !: Calcul du rayonnement net en MJm-2j-1 selon Pierre Cellier
      if (P_codernet == 2) then
        call ratmo(trg,P_aangst,P_bangst,P_latitude,jul,tpm,tmoy,Ratm)
        Rsolglo = sigma * (tutilrnet + 273.15)**4
        Rglo = Ratm - (Rsolglo * 3600.0 *24.0 *1e-6)

        !!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
        !!!rnet = (1 - albedolai) * trg  + Rglo;
        rnet = (1 - albedolai) * trg * cellTrg + (Rglo * cellvisibleSky);


      endif
      
      !: Calcul du rayonnement net en MJm-2j-1 selon Mermier
      !--if (P_codernet == 3) rnet = (1 - albedolai) * trg * arnet + brnet

      !: DR 15/05/06 j'ai des rnet negatifs qui posent des pbs à voir en reunion sticsettes
      rnet = max(rnet,0.01)
    
return  
end subroutine calrnet    
 
