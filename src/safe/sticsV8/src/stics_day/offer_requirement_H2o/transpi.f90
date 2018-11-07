!*********************************************************************
!         calcul de la transpiration
! version de Samuel et Nadine du primtemps 2007
! == = == = == = == = == = == = == = == = == = == = == = == = == = == = == =  = 
! le 22/05/01 changement test sur cumlr au lieu de ep = 0. pour répartition
!  **** le 07/06/02
! introduction de restez dans le cas de forte secheresse ou on a pas assez d'eau pour 
! affecte epz dans la caouche , on prend ce qui est dispo et on affecte le reste 
! à la couche suivante.
! le calucl de cumlr est fait uniquement sur les couches ou on a de l'eau 
!  ****
!*********************************************************************
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> Tranpiration of the crop (ep)
!> - Stics book paragraphe 7.3, page 138-139
!>
!! This module calculates the actual tranpiration of the crop (ep)
!! To calculate actual transpiration we chose to use a relationship linking relative transpiration (ratio of actual to maximal transpiration) to soil water content.
!! Such a simplified mathematical representation was proposed by Van Bavel (1953) for the total evapotranspiration. Relying on work by Slabbers (1980),
!! we proposed an operational formula to calculate this threshold using the above-mentioned variable, derived from basic laws governing water transfer
!! in the soil-plant atmosphere continuum (Brisson, 1998b).
!!
!! On a daily time scale, root uptake can be considered to be equal to leaf transpiration.  Root uptake calculated overall is then distributed between the soil layers.
!*-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine transpi(n,nbCouches,profsol,hur,humin,lracz,surf,P_codelaitr,lai,tauxcouv,nrec,P_codcueille,eop,exobiom, &
                   epz,ep,turfac,senfac,swfac,profexteau, hisafeInfluence)
      
  implicit none

!: Arguments

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: profsol  
  real,    intent(IN)    :: hur(nbCouches)  
  real,    intent(IN)    :: humin(nbCouches)  
  real,    intent(IN)    :: lracz(nbCouches)  
  real,    intent(IN)    :: surf      !> // OUTPUT // Fraction of surface in the shade // 0-1
  integer, intent(IN)    :: P_codelaitr  !> // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: lai      !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: tauxcouv      !> // OUTPUT // Cover rate // SD
  integer, intent(IN)    :: nrec  
  integer, intent(IN)    :: P_codcueille  !> // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0 
  real,    intent(IN)    :: eop      !> // OUTPUT // Maximum transpiration flux  // mm
  real,    intent(IN)    :: exobiom      !> // OUTPUT // Index of excess water active on surface growth // 0-1
  
    !!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE (extaction eau, azote et stress)
  integer, intent(IN)    :: hisafeInfluence      !>  //INPUT // Hisafe influence on water and nitrogen (1=yes 0=no)

  real,    intent(INOUT) :: epz(nbCouches)  
  real,    intent(INOUT) :: ep      !> // OUTPUT // Actual transpiration flux  // mm j-1
  real,    intent(INOUT) :: turfac      !> // OUTPUT // Index of turgescence water stress  // 0-1
  real,    intent(INOUT) :: senfac      !> // OUTPUT // Water stress index on senescence // 0-1
  real,    intent(INOUT) :: swfac      !> // OUTPUT // Index of stomatic water stress  // 0-1
  real,    intent(INOUT) :: profexteau      !> // OUTPUT // Average depth of water absorption // cm


!: Variables locales       
  real    :: cumlr  !>  
  real    :: cumlrh  !>  
  real    :: cumlh  !>  
  real    :: dispo  !>  
  real    :: swfacpro  !>  
  real    :: distr  !>  
  real    :: cumldistr  
  real    :: h(nbCouches)  
  integer :: iz  

!write(618,*)'transpi',n,eop

      ! initialisation de epz
      !!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE (extaction azote)
      if (hisafeInfluence == 0) then
        epz(1:int(profsol)) = 0.
      end if

      ! Calcul de h(iz) = hur(iz)-humin(iz) pour les valeur positive, 0. ailleurs
      do iz = 1,int(profsol)
         h(iz) = max(0., (hur(iz)-humin(iz)))

      end do


      ! ** PB - on pondère cumlracz par la surface de la plante    
      !  nb 07/02/06 on ne calcule que sur les couches ou on a de l'eau disponible   
      !  sb 16/02/07 - on calcule le cumul d'eau là ou on a des racines  
      !  sb 21/02/07 - ces quantités ne servent plus qu'au traitement des cas limites 
      !                (car ils provoquent des discontinuités)
      cumlh = 0.
      cumlr = 0.
      do iz = 1,int(profsol)
        if (h(iz) > 0.) then
          cumlr = cumlr + lracz(iz) * surf
        endif
        if ((lracz(iz) * surf) > 0.) then
          cumlh = cumlh + h(iz)
        endif
      end do


      ! ** calcul de ep
      ! *- si pas de plante
      ! *- domi 23/03/01 - taux de couv
      if (     (P_codelaitr == 1 .and. lai <= 0.)                   &
          .or. (P_codelaitr == 2 .and. tauxcouv == 0.)              &
          .or. (nrec /= 0 .and. n > nrec .and. P_codcueille == 1)   &
          .or. (eop <= 0.)                                        &
         ) then
         
         ep = 0.
         
      else

        ! NB - le 07/01/02 - l'excès d'eau affecte le fonctionnement stomatique
        if (cumlr > 0.) then
          ep = eop * min(swfac, exobiom)

          !  sb 16/02/07 - on teste si il y a assez d'eau pour satisfaire ep
          !                si ce n'est pas le cas, on le fixe à la quantitié d'eau disponible 
          !                et on recalcule les stress
          if (ep > cumlh) then 

            swfacpro = swfac / ep * cumlh
            turfac = turfac / swfac * swfacpro
!  write(618,*)'calcul de turfac dans transpi',swfac,swfacpro,turfac
            senfac = senfac / swfac * swfacpro
            !!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE
            !!!if (hisafeInfluence == 0) then
                swfac  = swfacpro
            !!!end if
            ep     = cumlh
              
          endif



          !  sb+nb 16/02/07 - on change le calcul de la répartition de ep dans le profil ;
          !                   une boucle while assure que l'on va bien répartir la totalité de ep ;
          !                   la fonction de répartition de l'absorption d'eau donne maintenant du poids 
          !                   à la fois au profil racinaire et et au profil hydrique.
          dispo = ep



          ! Tant que l'on a pas reparti toute l'ep (le test  > 1.d-15 évite une boucle infinie qui peut 
          ! se produire si on met  > 0.)
           do while (dispo > 1.d-15)

              cumldistr = 0.

              ! on met à jour cumlrh 
              ! (en prenant en compte le fait qu'il reste moins d'eau disponible ...)
              cumlrh = 0.
              do iz = 1,int(profsol)
                 cumlrh = cumlrh + (lracz(iz) * surf * (h(iz) - epz(iz)))
              end do
              
              ! SB et DR 15/02/08 pb dans le cas ou ep = cumlh on a dispo tres petit alors que cumlrh devient nul              
              if (cumlrh == 0) EXIT 

              ! DR 25/02/08 calcul de la profondeur moyenne d'extration d'eau
              profexteau =  0.

              do iz = 1,int(profsol)
                
                distr = dispo * ((lracz(iz) * surf * (h(iz) - epz(iz))) / cumlrh)
                 
                ! si epz calculé est supérieure à la quantité d'eau disponible
                ! on lui affecte la quantité d'eau disponible et le reste
                ! sera redistribué à la prochaine itération de la boucle while ...
                !!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE
                if (hisafeInfluence == 0) then
                    if ((epz(iz) + distr) > h(iz)) then
                      distr = h(iz) - epz(iz)
                      epz(iz) = h(iz)
                    else
                      epz(iz) = epz(iz) + distr

                    endif
                end if
               


                cumldistr = cumldistr + distr
                profexteau =  profexteau + (iz * epz(iz))
              
              end do
          
              profexteau =  profexteau / ep

              dispo = dispo - cumldistr

           enddo

         else
           ep = 1e-10
         endif
      endif


return
end subroutine transpi 
