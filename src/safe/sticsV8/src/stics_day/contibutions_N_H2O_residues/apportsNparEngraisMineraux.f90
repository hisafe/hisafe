! *-----------------------------------------------------------------------------------------------------------------------
!> fertilization
!> - Stics paragraphe 6.3, 6.3.1, 6.3.2, page 100-101
!!
!! The N inputs from mineral fertilizers can be applied either at the soil surface or at a given depth (LOCFERTI)
!! if the option 'localized fertilization' is activated (CODLOCFERTI =2).
!! We consider 8 different types of mineral fertilizers. As a simplification, urea is treated as an ammonium fertilizer
!! since its hydrolysis to ammonium carbonate is a very fast process (e.g. Recous et al, 1988; Hah, 2000).
!> Stics paragraphe 6.3, 6.3.1, 6.3.2, page 100-101
! *-----------------------------------------------------------------------------------------------------------------------
subroutine apportsNparEngraisMineraux(P_orgeng, P_voleng, P_deneng, anit, n, absoTot, P_Vabs2, P_codlocferti,           & ! IN
                         !             P_locferti, P_pHminvol, P_pHmaxvol, P_pH, P_Xorgmax, P_codedenit, P_Wh, P_engamm, &
                                      P_locferti, P_pHminvol, P_pHmaxvol, P_pH, P_Xorgmax, P_codedenit, P_engamm, &
                                      Nvoleng,Ndenit,Norgeng,QNvoleng,QNorgeng,QNdenit,Nhum,Nhuma,                    & ! INOUT
                                      amm,nit,precipN,totapN)
  implicit none

  real,    intent(IN)    :: P_orgeng      ! (P_engrais)   // PARAMETER // maximal quantity of mineral fertilizer that can be organized in the soil (fraction for type 8) // kg ha-1 // PARAM // 1
  real,    intent(IN)    :: P_voleng      ! (P_engrais)   // PARAMETER // maximal fraction of mineral fertilizer that can be volatilized  // SD // PARAM // 1
  real,    intent(IN)    :: P_deneng      ! (P_engrais)   // PARAMETER // proportion of the ñineral fertilizer that can be denitrified (useful if codenit not active) // SD // PARAM // 1
  real,    intent(IN)    :: anit          ! (n)  	      // OUTPUT // Daily nitrogen provided  // kgN.ha-1 j-1
  integer, intent(IN)    :: n  
  real,    intent(IN)    :: absoTot(5)    ! 5 derniers jours (max.)
  real,    intent(IN)    :: P_Vabs2       !> // PARAMETER // N uptake rate for which fertilizer losses are divided by 2 // kg.ha-1.d-1 // PARAM // 1
  integer, intent(IN)    :: P_codlocferti !> // PARAMETER // code of fertilisation localisation:  1: at soil surface, 2 = in the soil // code 1/2 // PARTEC // 0
  integer, intent(IN)    :: P_locferti    !> // PARAMETER // Depth of nitrogen apply (when fertiliser is applied in depth of soil) // cm // PARTEC // 1
  real,    intent(IN)    :: P_pHminvol    !> // PARAMETER // pH above which the fertilizer volatilisation is null // P_pH // PARAM // 1
  real,    intent(IN)    :: P_pHmaxvol    !> // PARAMETER // pH beyond which the fertilizer volatilisation is maximum // P_pH // PARAM // 1
  real,    intent(IN)    :: P_pH          !> // PARAMETER // P_pH of mixing soil + organic amendments  // SD // PARSOL // 1
  real,    intent(IN)    :: P_Xorgmax     !> // PARAMETER // maximal amount of immobilised N coming from the mineral fertilizer  // kg.ha-1 // PARAM // 1
  integer, intent(IN)    :: P_codedenit   !> // PARAMETER // option to allow the calculation of denitrification :yes (1), no(2) // code 1/2 // PARSOL // 0
!  real,    intent(IN)    :: P_Wh          !> // PARAMETER // ratio N/C of humus // g g–1 // PARAM // 1
  real,    intent(IN)    :: P_engamm      ! (P_engrais)   // PARAMETER // proportion of ammonium in the fertilizer // SD // PARAM // 1

  ! ici, ces variables de sortie pourraient être mises en OUT plutot que INOUT
  real,    intent(INOUT) :: Nvoleng      !> // OUTPUT // Daily volatilisation of NH3-N from fertiliser // kg.ha-1.d-1
  real,    intent(INOUT) :: Ndenit       !> // OUTPUT // "Daily denitrification of N from fertiliser or soil (if option  denitrification  is activated)" // kg.ha-1.d-1
  real,    intent(INOUT) :: Norgeng      !> // OUTPUT // Daily organisation of N from fertiliser // kg.ha-1.d-1
  real,    intent(INOUT) :: QNvoleng     !> // OUTPUT // Cumulative volatilisation of NH3-N  from fertiliser // kg.ha-1
  real,    intent(INOUT) :: QNorgeng     !> // OUTPUT // Cumulative organisation of N from fertiliser // kg.ha-1
  real,    intent(INOUT) :: QNdenit      !> // OUTPUT // "Cumulative denitrification of N from fertiliser or soil (if option  denitrification  is activated)" // kg.ha-1
  real,    intent(INOUT) :: Nhum(1:10)   ! avant Nbio(1:10,1:1) >// OUTPUT immobilised N from fertilizers by soil microbial biomass -part of humus - in the 10 first cm  // kg.ha-1
  real,    intent(INOUT) :: Nhuma        !> // OUTPUT // Amount of active N in the humus pool  // kg.ha-1
  real,    intent(INOUT) :: amm(1:max(1,P_locferti))  ! (1:max(1,itk%P_locferti))
  real,    intent(INOUT) :: nit(1:max(1,P_locferti))  
  real,    intent(INOUT) :: precipN  
  real,    intent(INOUT) :: totapN       !> // OUTPUT // Total amount of N inputs coming from fertiliser and organic residues // kg.ha-1


!: Variables locales
  integer :: iz  
  real    :: appamm  !>  
  real    :: appnit  !>  
  real    :: Norgamm  !>  
  real    :: Norgnit  



      if (anit == 0.) then
        !  pas d'apport d'engrais mineral
        Nvoleng = 0.
        Norgeng = 0.
        Ndenit = 0.
      else
        ! apport d'engrais mineral (NH4 et NO3)
        appamm = anit * P_engamm
        appnit = anit - appamm

        ! devenir de l'engrais
        call perteng(P_orgeng,P_voleng,P_deneng,anit,n,absoTot,P_Vabs2,P_codlocferti, & ! IN
                     P_pHminvol,P_pHmaxvol,P_pH,P_Xorgmax,P_codedenit,                &
                     Nvoleng,Ndenit,Norgeng,QNvoleng,QNorgeng,QNdenit)      ! INOUT

        ! partition de l'organisation aux dépens de NH4 et NO3
        Norgamm = Norgeng * P_engamm
        Norgnit = Norgeng - Norgamm

! on suppose l'azote de l'engrais organisé dans la biomasse microbienne du sol (incluse dans humus) sur 0-10 cm
        do iz=1,10
           Nhum(iz)= Nhum(iz)+ Norgeng/10.
        end do
           Nhuma = Nhuma + Norgeng
!        Nhum  = Nhum  + Norgeng
!        Nhumt = Nhumt + Norgeng
!        Chumeng = Norgeng / P_Wh / 5.

        ! marie et domi - 10/10/03 - apres modif de bruno chumt calculé dans mineral.for
        !-- Chumt = Nhumt/P_Wh

!        do iz = 1,5
!          Chum(iz) = Chum(iz) + Chumeng
!        end do

        ! répartition de l'engrais dans le sol
        if (P_codlocferti == 1) then
        ! a) cas d'un apport en surface
          amm(1) = amm(1) + appamm - Norgamm - Nvoleng
          nit(1) = nit(1) + appnit - Norgnit
!  modif Bruno aout 2012
!         if (P_codedenit == 2) nit(1) = nit(1) - Ndeneng
          if (P_codedenit == 2) nit(1) = nit(1) - Ndenit
!  write(ficdbg,'(i3,3f16.12)')n,appnit,Norgnit,Ndeneng
        else ! TODO: rajouter un test sur P_codlocferti (=2 ?) et surtout P_locferti > 0 & < nbCouchesSol
        ! b) cas d'un apport en profondeur
          amm(P_locferti) = amm(P_locferti) + appamm - Norgamm - Nvoleng
          nit(P_locferti) = nit(P_locferti) + appnit - Norgnit
!  modif Bruno aout 2012
!         if (P_codedenit == 2) nit(P_locferti) = nit(P_locferti)- Ndeneng
          if (P_codedenit == 2) nit(P_locferti) = nit(P_locferti)- Ndenit
        endif

        ! cumul des entrées d'azote
        precipN  = precipN + appnit
        totapN   = totapN  + anit
      endif

return
end subroutine apportsNparEngraisMineraux
 
 
