! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!! This module calculates the water stress indices, swfac and turfac.
!> - Stics book paragraphe 7.3.1, 7.3.2, page 138-140
!>
!! Relative transpiration, i.e. the relationship between actual transpiration and maximal transpiration (ep/eop), is a bilinear function of the available
!! water content in the root zone, teta (i.e. the water content above the wilting point in cm3 of water/cm3 of dry soil).
!!
!! The water content threshold separating the maximal transpiration stage and the reduced transpiration stage (tetstomate) depends on root density,
!! the stomatal functioning of the plant, and the evaporative demand (Brisson, 1998c). It was shown that this threshold does not depend on the soil type,
!! for example via the maximal available water content, as is commonly assumed.
!!
!! In the calculations below, cumlracz is the summation over the whole rooting depth, zrac, of effective root length density lracz,
!! psisto is the critical potential of stomatal closure (positive in bars) and rayon is the mean root radius which is assumed to be equal to 0.02 cm.
!!
!! The ep/eop ratio is equal to the stomatal stress index, swfac.  The stress turgor index turfac which affects leaf growth comes into play earlier.
!! The method for calculating it is copied from the method used for swfac using the critical potential of cell expansion psiturg.
!! Since psiturg is lower than psisto, we obtain a higher teturg threshold.  In other words, leaf growth can be inhibited even when transpiration is still at
!! its maximum level.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine stressEau(n,P_profsem,zrac,nbCouches,hur,humin,cumlracz,surf,P_codelaitr,lai,tauxcouv, &  ! IN
                     nrec,P_codcueille,eop,P_rayon,P_psisto,P_psiturg,P_rapsenturg,supres,P_swfacmin,     &
                     resrac,swfac,turfac,tetstomate,teturg,senfac,hisafeInfluence)                               ! INOUT

  implicit none

!: Arguments

  integer, intent(IN)    :: n  
  real,    intent(IN)    :: P_profsem  !> // PARAMETER // Sowing depth // cm // PARTEC // 1 
  real,    intent(IN)    :: zrac      !> // OUTPUT // Depth reached by root system // cm
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: hur(nbCouches)  
  real,    intent(IN)    :: humin(nbCouches)  
  real,    intent(IN)    :: cumlracz      !> // OUTPUT // Sum of the effective root lengths  // cm root.cm -2 soil
  real,    intent(IN)    :: surf      !> // OUTPUT // Fraction of surface in the shade // 0-1
  integer, intent(IN)    :: P_codelaitr  !> // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: lai      !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: tauxcouv      !> // OUTPUT // Cover rate // SD
  integer, intent(IN)    :: nrec  
  integer, intent(IN)    :: P_codcueille  !> // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0 
  real,    intent(IN)    :: eop      !> // OUTPUT // Maximum transpiration flux  // mm
  real,    intent(IN)    :: P_rayon  !> // PARAMETER // Average radius of roots // cm  // PARAM // 1 
  real,    intent(IN)    :: P_psisto  !> // PARAMETER // absolute value of the potential of stomatal closing // bars // PARPLT // 1 
  real,    intent(IN)    :: P_psiturg  !> // PARAMETER // absolute value of the potential of the beginning of decrease of the cellular extension // bars // PARPLT // 1 
  real,    intent(IN)    :: P_rapsenturg  !> // PARAMETER // threshold soil water content active to simulate water sensecence stress as a proportion of the turgor stress // SD // PARPLT // 1 
  real,    intent(IN)    :: supres  
  real,    intent(IN)    :: P_swfacmin  !> // PARAMETER // minimul value for drought stress index (turfac, swfac, senfac) // SD // PARAMV6 // 1 

  !!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE (extaction eau, azote et stress)
  integer, intent(IN)    :: hisafeInfluence      !>  //INPUT // Hisafe influence on water and nitrogen (1=yes 0=no)

  real,    intent(INOUT) :: resrac      !> // OUTPUT // Soil water reserve in the root zone // mm
  real,    intent(INOUT) :: swfac      !> // OUTPUT // Index of stomatic water stress  // 0-1
  real,    intent(INOUT) :: turfac      !> // OUTPUT // Index of turgescence water stress  // 0-1
  real,    intent(INOUT) :: tetstomate      !> // OUTPUT // Threshold soil water content limiting transpiration and photosynthesis  // % vol
  real,    intent(INOUT) :: teturg      !> // OUTPUT // Threshold soil water content limiting surface area growth of leaves // % vol
  real,    intent(INOUT) :: senfac      !> // OUTPUT // Water stress index on senescence // 0-1

!: Variables locales
  real    :: slrac  !>  
  real    :: teta  !>  
  real    :: cumlr  !>  
  real    :: tetsen  
  integer :: iz  



      ! ** calcul de la réserve dans la zone racinaire, resrac
      ! *- 17/06/04 - les 3 : donc à partir de P_profsem (bug !)
      resrac = 0.
      do iz = int(P_profsem), int(zrac)+1
        resrac = resrac + max(hur(iz) - humin(iz), 0.)
      end do


      ! ** PB - on pondère cumlracz par la surface de la plante
      cumlr = cumlracz * surf

!      write(618,*)'cumlr', n, cumlr, eop

      ! ** si pas de plante
      if (     (P_codelaitr == 1 .and. lai <= 0.)          &
          .or. (P_codelaitr == 2 .and. tauxcouv == 0.)             &
          .or. (nrec /= 0 .and. n > nrec .and. P_codcueille <= 1)     &
          .or. (eop <= 0.)                                 &
         ) then

        swfac  = 1.
        turfac = 1.
      else
        if (cumlr > 0.) then
!      write(618,*)'cumlr'
                ! ** calcul du seuil de teneur en eau en deça duquel il y a réduction de transpiration
          ! *- calcul de la contribution racinaire, slrac
          slrac = cumlr / (log(1 / (sqrt(3.14 * cumlr / zrac) * P_rayon)))

          ! ** calcul du seuil de fermeture stomatique,testomate
          ! *- changement des paramètres de la relation K(teta) pour être conforme avec Taylor et Klepper, 1975
          ! *- K = 10-8exp(80(teta-tetaPF)) et non K = 10-7exp(40(teta-tetaPF))

          tetstomate = 1 / 80. * log(eop / (10. * slrac * 2 * 3.14e-8 * P_psisto * 1e3))



          !: Calcul du seuil de teneur en eau en deça duquel il y a réduction d'expansion foliaire
          teturg = 1 / 80. * log(eop / (10. * slrac * 2 * 3.14 * 1e-8 * P_psiturg * 1e3))

          !write(70,'(i4,4f16.12)')n,teturg,slrac,P_psiturg,eop

          ! ** calcul du seuil de teneur en eau en deça duquel il y a accélération de sénescence
          ! ** domi - 22/05/01
          !-- tetsen = 1/80.0*log(eop/(10*slrac*2*3.14*1e-8*psisen*1e3))
          tetsen = P_rapsenturg * teturg

          !: Teneur en eau comparable aux seuils
          teta = (resrac + supres) / (zrac * 10.)

          !: Calcul de swfac
          if (teta <= tetstomate) then
            swfac = teta / tetstomate
          else
            swfac = 1.
          endif
          !write(70,*) n,'swfac=',swfac,teta,tetstomate,P_swfacmin
          swfac = max(swfac, P_swfacmin)


          !: Calcul de turfac
          if (teta <= teturg) then
            turfac = teta / teturg
!          write(618,*) n, 'turfac=',turfac, teta, teturg
          else
            turfac = 1.
          endif
          turfac = max(turfac, P_swfacmin)
!          write(618,*) n, 'turfac=',turfac, P_swfacmin

          ! ** calcul de senfac
          ! Nb le 07/07/06 on veut pouvoir annuler l'effet du stress hydrique sur la senescence
          if (P_rapsenturg > 0.0) then
            if (teta <= tetsen) then
              senfac = teta / tetsen
            else
              senfac = 1.0
            endif
            senfac = max(senfac, P_swfacmin)
          else
            senfac = 1.0
          endif

        else
          turfac = P_swfacmin
          swfac  = P_swfacmin
          senfac = P_swfacmin
        endif
      endif

return
end
 
 
