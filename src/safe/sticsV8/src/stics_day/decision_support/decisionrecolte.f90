!*----------------------------------------------------------------------------------------------------------------------------------------------------------------------------* c
!> calculation of the decision of harvest
!>-  Stics book paragraphe 6.1.4.d., page 98
!>
!! The decision to harvest can be taken as a function of crop maturity, or soil water status, sanitary or even economic considerations (handled in Stics_Recolte.f90)
!!
!! If the soil is too wet at this date, it is possible to postpone harvest to avoid compaction. In that case a period (in number of days) after the crop-dependent harvest date
!! is defined (NBJMAXAPRESRECOLTET) during which the average soil water over the depth affected by the harvesting machinery (PROFHUMRECT) is tested.
!!
!! This soil water status is considered as damaging if it is above HUMSEUILTASSRECT x HUCC in the zone between the surface and PROFHUMRECT. Yet this delay cannot
!! exceed IRECBUTOIRT which is the latest date for harvesting. (Brisson et al., 2009., paragraph 6.1.4.d)
! *----------------------------------------------------------------------------------------------------------------------------------------------------------------------------* c
!  permet de decider à partir de la date de semis lue dans le fichier tec
!  si on seme ou pas .
! possibilité de repousser le semis au plus du parametre nbjapressemis
! *-----------------------------------------------------------------------* c
subroutine decisionrecolte(n,P_prophumtassrec,P_hccf,nh,nbCouches,P_epc,hur,da,sat,   &   ! IN
                           P_profhumrecolteuse,nrecbutoir,P_nbjmaxapresrecolte,     &
                           nbjpourdecirecolte,repousserecolte,nrec)                 ! INOUT


  implicit none
  
!: Arguments

  integer, intent(IN)    :: n  
  real,    intent(IN)    :: P_prophumtassrec  !> // PARAMETER // field capacity proportion above witch compaction may occur (to delay harvest) // SD // PARAM // 1 
  real,    intent(IN)    :: P_hccf(nh)  !> // PARAMETER // gravimetric water content at field capacity of each soil layer (/fine earth) (table) // % w // PARSOL // 1
  integer, intent(IN)    :: nh  
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
  real,    intent(IN)    :: hur(nbCouches)  
  real,    intent(IN)    :: da(nh)   !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real,    intent(IN)    :: sat(nbCouches)  
  real,    intent(IN)    :: P_profhumrecolteuse  !> // PARAMETER // soil depth for which moisture is considered as a reference to allow or not harvesting in the case of soil compaction activation // cm // PARTEC // 1 
  integer, intent(IN)    :: nrecbutoir  
  integer, intent(IN)    :: P_nbjmaxapresrecolte  !> // PARAMETER // maximal delay (number of days) of harvest date in case of soil compaction activation // jours // PARTEC // 1 
  
  integer, intent(INOUT) :: nbjpourdecirecolte   !> // OUTPUT // "Number of days until harvest is launched when it's postponed by the  harvest decision  option activation" // days

  !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
  !!!logical, intent(INOUT) :: repousserecolte
  integer, intent(INOUT) :: repousserecolte
  integer, intent(INOUT) :: nrec  


!: Variables locales
  real    :: hummoy  
  integer :: numcouche  !>  
  integer :: i  !>  
  integer :: k  !>  
  integer :: jdeb  !>  
  integer :: jfin  
  real    :: humseuiltassrec  


      ! 07/03/08 mis en parametre 
      !  humseuiltasssem = 1.2*P_hccf(1)
      !  humseuiltassrec = 1.0*P_hccf(1)
      ! 21/03/08 DR ne sert à rien ici
      !  humseuiltasssem = P_prophumtasssem*P_hccf(1)
      humseuiltassrec = P_prophumtassrec * P_hccf(1)

! TODO: On pourrait faire une fonction de la boucle B1, présente dans decisionsemis et decisionrecolte
      hummoy = 0.
      jdeb = 1
      jfin = 0
      numcouche = 0

B1:   do i = 1,nh
  
        jfin = jfin + int(P_epc(i))
  
        do k = jdeb,jfin
          numcouche = numcouche + 1
          ! domi 090606 on divise sat par da pour passer en ponderal
          ! domi 13/11/06 y'avait un bug (meaculpa sur le calcul de hummoy
          !        hummoy = hummoy+hur(i)/da(i)+sat(i)/da(i)
          hummoy = hummoy + (hur(numcouche)) / da(i) + (sat(i) / da(i))
          if (numcouche == P_profhumrecolteuse) EXIT B1
        end do
  
        jdeb = jdeb + int(P_epc(i))
  
      end do B1
      
      hummoy = hummoy * 10. / P_profhumrecolteuse

      !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
      !!!if (repousserecolte .eqv. .TRUE.) then
      if (repousserecolte == 1 ) then
        if ((n >= nrec .and. nrec /= 0) .or. n == nrecbutoir) then
          if (hummoy > humseuiltassrec) then
            nbjpourdecirecolte = nbjpourdecirecolte + 1
            if (nbjpourdecirecolte <= P_nbjmaxapresrecolte) then
              !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
              !!!repousserecolte = .TRUE.
              repousserecolte = 1
              nrec = -999
            else
              !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
              !!!repousserecolte = .FALSE.
              repousserecolte = 0
              nrec = n
            endif
          else
            !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
            !!!repousserecolte = .FALSE.
            repousserecolte = 0
            nrec = n
          endif
        endif
      endif


      ! DR 02/05/06 de toute facon à recoltebutoir on recolte
      !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
      !!!if (repousserecolte .and. n == nrecbutoir) then
      if (repousserecolte == 1  .and. n == nrecbutoir) then
        nrec = nrecbutoir
        !!!MODIF HISAFE 6 : on remplace les tableaux de boolean
        !!!repousserecolte = .FALSE.
        repousserecolte = 0
      endif
      


return        
end subroutine decisionrecolte 
 
