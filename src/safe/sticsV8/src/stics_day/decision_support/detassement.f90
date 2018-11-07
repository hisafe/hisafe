!************************************************************************   
!  Projet DST-Stage Célia Moulin
!  introduction de la possibilité de modifier les paramètres du sol
!  sous l'influence des techniques
!  le 15/02/06
!************************************************************************

!subroutine detassement(n,NH,nbCouches,sat,napS,ntrav,P_proftrav,P_proflabour,P_proftravmin,P_rugochisel,  &
!                       P_dachisel,P_codefente,P_hccf,P_hminf,beta_sol,P_rugolabour,P_dalabour,AZnit,AZamm,    &
!                       P_coderacine,lrach,P_lvopt,da_ini,zesx_ini,q0_ini,nbResidus,                   &
!                       nhe,HR,TS,P_epc,profsol,P_profhum,P_z0solnu,profhum_tass,P_infil,ncel,da,P_epd,icel, &
!                       hur,hucc,humin,tsol,izcel,izc,nit,amm,rl,flrac,Cbio,Nbio,Cres,Chum,P_zesx,P_q0)
! DR 01/02/2011 pour le detassement c'est le travail du sol qu'il faut prndre en compte

!*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------* c
!> Soil tillage implements, whether or not they invert the soil, fragment it, leading to a decrease in bulk density.
!>- Stics book paragraphe 6.5.3, page 112
!>
!! Soil tillage implements, whether or not they invert the soil, fragment it, leading to a decrease in bulk density. Depending on the type of tool, this fragmentation concerns
!! either a superficial layer (e.g. a surface tillage after harvesting) or the whole tilled layer (e.g. a mouldboard plough or a subsoiler).
!!
!!  Consequently the soil description in layers should be in agreement with the various soil tillage operations carried out. For each tool, the resulting bulk density and roughness
!! are defined as technical parameters. For a chisel and a plough those parameters are DACHISELT, DALABOURT, RUGOCHISELT and RUGOLABOURT. The modification in bulk density affects
!! infiltrability (see effettassurinfil.f90), water and nitrogen profiles. Ploughing tends to increase soil evaporation by increasing its roughness,
!! but the water balance generally remains positive due to the increase in water storage as a consequence of greater infiltrability.
!!
!! The effect of soil tillage on the incorporation of crop residues is handled in apportsOrganiquesEtTravailDuSol.f90.
! Of course secondary effects of these management techniques appear on waterlogging, denitrification, nitrate leaching, root growth and water stress, which require careful validation.

! *-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------* c



subroutine detassement(n,NH,nbCouches,sat,P_nbjtrav,numjtrav,P_proftrav,P_proflabour,P_proftravmin,P_rugochisel,  &
                       P_dachisel,P_codefente,P_hccf,P_hminf,beta_sol,P_rugolabour,P_dalabour,AZnit,AZamm,    &
                       P_coderacine,lrach,P_lvopt,da_ini,zesx_ini,q0_ini,nbResidus,                   &
                       nhe,HR,TS,P_epc,profsol,P_profhum,P_z0solnu,profhum_tass,P_infil,ncel,da,P_epd,icel, &
                       hur,hucc,humin,tsol,izcel,izc,nit,amm,rl,flrac,Cbio,Nbio,Cres,Nres,Chum,Nhum,P_zesx,P_q0)


USE Divers, only: MACROPOROSITE
USE Messages

  implicit none
  
!: Arguments

integer, intent(IN)    :: n  
integer, intent(IN)    :: NH  
integer, intent(IN)    :: nbCouches  
real,    intent(IN)    :: sat(nbCouches)  
!integer, intent(IN)    :: napS                  ! napS de la plante principale ?
integer, intent(IN)    :: P_nbjtrav                  ! napS de la plante principale ?   // PARAMETER // number of cultivations oprerations // days // PARTEC // 1

integer, intent(IN)    :: numjtrav(P_nbjtrav)           ! ntrav de la plante principale ?  
real,    intent(IN)    :: P_proftrav(P_nbjtrav)        ! P_proftrav de la plante principale ?   // PARAMETER // Depth of residue incorporation  (max. 40 cm) // cm // PARTEC // 1
real,    intent(IN)    :: P_proflabour  !> // PARAMETER // soil minimal depth for ploughing when soil compaction is activated // cm // PARAM // 1 
real,    intent(IN)    :: P_proftravmin  !> // PARAMETER // soil minimal depth for chisel tillage when soil compaction is activated // cm // PARAM // 1 
real,    intent(IN)    :: P_rugochisel  !> // PARAMETER // bare soil rugosity lenght (P_z0solnu) for a chisel when soil compaction is activated // m // PARTEC // 1 
real,    intent(IN)    :: P_dachisel  !> // PARAMETER // bulk density modification as a result of soil management (Chisel) // g.cm-3 // PARTEC // 1 
integer, intent(IN)    :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 
real,    intent(IN)    :: P_hccf(nh)  !> // PARAMETER // gravimetric water content at field capacity of each soil layer (/fine earth) (table) // % w // PARSOL // 1
real,    intent(IN)    :: P_hminf(nh)  !> // PARAMETER // gravimetric water content at wilting point of each soil layer (/fine earth) (table) // % w // PARSOL // 1
real,    intent(IN)    :: beta_sol(2)           ! dimension 2 ? 2 plantes ? 2 couches ?  
real,    intent(IN)    :: P_rugolabour  !> // PARAMETER // bare soil rugosity lenght (P_z0solnu) for a plough when soil compaction is activated // m // PARPLT // 1 
real,    intent(IN)    :: P_dalabour  !> // PARAMETER // bulk density modification as a result of soil management (Plough) // g.cm-3 // PARTEC // 1 
real,    intent(IN)    :: AZnit(nh)   !> // OUTPUT // Amount of nitric nitrogen in the horizon 3 (table)  // kgN.ha-1
real,    intent(IN)    :: AZamm(nh)   !> // OUTPUT // Amount of  mineral nitrogen in the horizon 3 (table)  // kgN.ha-1
integer, intent(IN)    :: P_coderacine  !> // PARAMETER // Choice of estimation module of growth root in volume: standard profile (1) or by actual density (2) // code 1/2 // PARPLT // 0 
real,    intent(IN)    :: lrach(nh)   !> // OUTPUT // Root density in the horizon 1 // cm.cm-3
real,    intent(IN)    :: P_lvopt  !> // PARAMETER // Optimum root density // cm root.cm-3 soil // PARAM // 1 
real,    intent(IN)    :: da_ini(2)             ! dimension 2 ? 2 plantes ? 2 couches ?  
real,    intent(IN)    :: zesx_ini  
real,    intent(IN)    :: q0_ini  
integer, intent(IN)    :: nbResidus  

integer, intent(INOUT) :: nhe  
real,    intent(INOUT) :: HR(nh)   !> // OUTPUT // Water content of the horizon 5 (table)    // % pond.
real,    intent(INOUT) :: TS(nh)   !> // OUTPUT // Mean soil temperature (mean on the horizon 4) // degree C
real,    intent(INOUT) :: P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
real,    intent(INOUT) :: profsol  
real,    intent(INOUT) :: P_profhum  !> // PARAMETER // Humification depth  (max.60 cm) // cm // PARSOL // 1 
real,    intent(INOUT) :: P_z0solnu  !> // PARAMETER // roughness length of bare soil // m // PARSOL // 1 
real,    intent(INOUT) :: profhum_tass(2)       ! dimension 2 ? 2 plantes ? 2 couches ?  
real,    intent(INOUT) :: P_infil(2)  !> // PARAMETER // infiltrability parameter at the base of each horizon if codemacropor = 1) // mm day-1 // PARSOL // 1   // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1
integer, intent(INOUT) :: ncel  
real,    intent(INOUT) :: da(nh)   !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
integer,    intent(INOUT) :: P_epd(nh)  !> // PARAMETER //  mixing cells thickness (=2 x dispersivity) // cm // PARSOL // 1
integer, intent(INOUT) :: icel(0:nbCouches)  
real,    intent(INOUT) :: hur(nbCouches)  
real,    intent(INOUT) :: hucc(nbCouches)  
real,    intent(INOUT) :: humin(nbCouches)  
real,    intent(INOUT) :: tsol(nbCouches)  
integer, intent(INOUT) :: izcel(nh)  
integer, intent(INOUT) :: izc(nh)  
real,    intent(INOUT) :: nit(nbCouches)  
real,    intent(INOUT) :: amm(nbCouches)  
real,    intent(INOUT) :: rl(nbCouches)  
real,    intent(INOUT) :: flrac(nbCouches)  
real,    intent(INOUT) :: Cbio(nbCouches,nbResidus)  
real,    intent(INOUT) :: Nbio(nbCouches,nbResidus)  
real,    intent(INOUT) :: Cres(nbCouches,nbResidus)
real,    intent(INOUT) :: Nres(nbCouches,nbResidus)
real,    intent(INOUT) :: Chum(nbCouches)  
real,    intent(INOUT) :: Nhum(nbCouches)
real,    intent(INOUT) :: P_zesx  !> // PARAMETER // maximal depth of soil affected by soil evaporation // cm // PARSOL // 1 
real,    intent(INOUT) :: P_q0  !> // PARAMETER // Parameter of the end of the maximum evaporation stage  // mm // PARSOL // 1 

!: Variables locales
  integer :: i  !>  
  integer :: is  !>  
  integer :: nhel  !>  
  integer :: npro  !>  
  integer :: j  !>  
  integer :: nepc  !>  
  integer :: decalprof  
  real    :: datravail  !>  
  real    :: xx  !>  
  real    :: qeaucouche1  !>  
  real    :: qeaucouche2  
  !-- real msmax,Nmineral,nitsurf,nitloci,Norgamm,Norgnit
  !-- real qnplmax,propvolat,travsol

  !-- real hummoy,daini
  !-- integer numcouche
  integer :: ir  !>  
  integer :: izmax  

  real :: cumCbio(2,nbResidus),cumCres(2,nbResidus),cumNres(2,nbResidus),cumNbio(2,nbResidus),CumChum(2),cumNhum(2)
  !-- integer decalprof1,decalprof2,k
  !-- real epcini(2)
  real :: epci(5)  
  real :: macropor  
  integer :: iz  !>  
  integer :: nhec  


  real :: F_effettasssurinfil

      ! DR 28/07/08 je rajoute le calcul de TS qu'on a pas encore car on est pas passé dans sortie
      ! je m'en suis rendu compte car on avait pas de vminh le premier jour du à tsol = 0.0      
      nhe = 0         
      do i = 1,NH
        HR(i) = 0.
        TS(i) = 0.
        izmax = int(P_epc(i))
        do iz = 1,izmax
          HR(i) = HR(i) + HUR(nhe+iz)+sat(nhe+iz)
          !if(n==1) write(71,*) n,i,iz,hr(i),hur(nhe+iz),sat(nhe+iz)
          TS(i) = TS(i) + tsol(nhe+iz)
        end do                       
        nhe = nhe + izmax
        HR(i) = HR(i) / da(i) * 10 / P_epc(i)
        !if(n==1) write(71,*) n,i,hr(i),da(i),P_epc(i)
        TS(i) = TS(i) / P_epc(i)
      end do
      
      !write(71,*)n,'avant detass',(hr(i),i = 1,4),(aznit(i),i = 1,4)

      ! DR - 050506 : On essaie car on a un pb d'arrondi 
      profsol = 0
      do iz = 1,NH
        epci(iz) = int(P_epc(iz))
        profsol = profsol + epci(iz)
      end do

      ! dr - 12/05/06 : On fait le cumul de Cbio,Nbio,Cres Chum pour pouvoir faire la rerepartition en cas de tassement
      CumCbio(:,:) = 0.
      CumNbio(:,:) = 0.
      CumCres(:,:) = 0.
      CumNres(:,:) = 0.
      CumCHum(:)   = 0.
      CumNHum(:)   = 0.

      nhec = 0
      do i = 1,2
        do iz = 1,int(P_epc(i))
          do ir = 1,nbResidus
            cumCbio(i,ir) = CumCbio(i,ir) + Cbio(iz + nhec,ir)
            cumNbio(i,ir) = CumNbio(i,ir) + Nbio(iz + nhec,ir)
            cumCres(i,ir) = CumCres(i,ir) + Cres(iz + nhec,ir)
            cumNres(i,ir) = CumNres(i,ir) + Nres(iz + nhec,ir)
          end do
          CumChum(i) = CumChum(i) + Chum(iz + nhec)
          CumNhum(i) = CumNhum(i) + Nhum(iz + nhec)
        end do
        nhec = nhec + int(P_epc(i))
      end do


      do is = 1,P_nbjtrav
        if (n == numjtrav(is)) then
!          write(146,*)'on travaille le sol'
!          write(146,*)n,'P_epc(iz),iz = 1,4,P_infil(1),P_infil(2),P_profhum,da(1),da(2),P_epd(1),P_epd(2)'
!          write(146,1461)ansemis,n,(P_epc(iz),iz = 1,4),P_infil(1),P_infil(2),P_profhum,da(1),da(2),P_epd(1),P_epd(2)
!1461      format(2i5,9f10.2,2i5)

          ! on décide qu'il existe un seuil de profondeur définissant le Labour
          ! on est forcément dans un sol comprenant 2 couches superficielles
          ! l'une pour le Chisel et l'autre pour le Labour
          qeaucouche1 = HR(1) * da(1) / 10 * int(epci(1))
          qeaucouche2 = HR(2) * da(2) / 10 * int(epci(2))
          if (P_proftrav(is) <= P_proflabour .and. P_proftrav(is) > P_proftravmin) then
            ! on est dans le cas "Chisel"
            write(146,*)'on est en chisel'
            P_z0solnu = P_rugochisel
            if (da(1) < P_dachisel) then
              datravail = da(1)
            else
              datravail = P_dachisel
            endif
            epci(1) = epci(1) * da(1) / datravail
            ! DR 30/07/08 je recalcule ca suite aux pb de P_profhum qui devient trop grand
            ! nouvelle proposition de Julie
            !-- P_profhum = P_profhum*da(1)/datravail
            profhum_tass(1) = profhum_tass(1) * da(1) / datravail
            P_profhum = profhum_tass(1) + profhum_tass(2)

            ! DR - 180406 : On verifie la valeur de da pour pas avoir de macropor negatif
            ! ** calcul de la macroporosité
            macropor = MACROPOROSITE(P_codefente,da(1),P_hccf(1),P_hminf(1),epci(1))
            if (P_codefente /= 1 .and. macropor <= 0) then
              datravail = ((1/10.*epci(i))-1.0) / ((-1/2.66)-(P_hccf(i)/100.))
            endif
            !  domi 18/04/06 si on a modifié da on modifie P_infil
            if (da(1) > datravail) P_infil(1) = F_effettasssurinfil(datravail,beta_sol(1))
            ! fin modif DR

            da(1) = datravail
          endif
          
          if (P_proftrav(is) > P_proflabour) then
            write(146,*)'on est en labour'
            ! on est dans le cas "Labour"
            P_z0solnu = P_rugolabour
            if (da(1) < P_dalabour) then
              datravail = da(1)
            else
              datravail = P_dalabour
            endif
            ! 13/03/08 nint ajouté
            epci(1) = nint(epci(1)*da(1)/datravail)

            ! DR 180406 on verifie la valeur de da pour pas avoir de macropor negatif
            ! ** calcul de la macroporosité
            macropor = MACROPOROSITE(P_codefente,da(1),P_hccf(1),P_hminf(1),epci(1))
            if (P_codefente /= 1 .and. macropor <= 0) then
              datravail = ((1/10.*epci(i))-1.0) / ((-1/2.66)-(P_hccf(i)/100.))
            endif
            !  domi 18/04/06 si on a modifié da on modifie P_infil
            if (da(1) > datravail) P_infil(1) = F_effettasssurinfil(datravail,beta_sol(1))
            ! fin modif DR

            da(1) = datravail
            ! on fait l'hypothèse que P_profhum > P_epc(1)
            !        profhum1 = epci(1)
            !        profhum2 = P_profhum-profhum1
            profhum_tass(1) = profhum_tass(1)*da(1)/datravail
            if (da(2) < P_dalabour) then
              datravail = da(2)
            else
              datravail = P_dalabour
            endif

            ! DR 180406 on verifie la valeur de da pour pas avoir de macropor negatif
            ! ** calcul de la macroporosité
            macropor = MACROPOROSITE(P_codefente,da(2),P_hccf(2),P_hminf(2),epci(2))
            if (P_codefente /= 1 .and. macropor <= 0) then
              datravail = ((1/10.*epci(i))-1.0) / ((-1/2.66)-(P_hccf(i)/100.))
            endif
            !  domi 18/04/06 si on a modifié da on modifie P_infil
            if (da(2) > datravail) P_infil(2) = F_effettasssurinfil(datravail,beta_sol(2))
            ! fin modif DR
  
            ! 13/03/08 nint ajouté
            epci(2) = nint(epci(2)*da(2)/datravail)

            ! DR 30/07/08 je recalcule ca suite aux pb de P_profhum qui devient trop grand
            ! nouvelle proposition de Julie
            !        P_profhum = profhum1+profhum2*da(2)/datravail
            profhum_tass(2) = profhum_tass(2) * da(2) / datravail
            P_profhum = profhum_tass(1) + profhum_tass(2)
            da(2) = datravail

          endif

          ! on recalcule profsol
          xx = profsol
          profsol = 0.0
          
          do i = 1,5 ! TODO: 5 ou nh ?
            if (epci(i) /= 0.0) then
              profsol = profsol + epci(i)
              ! 20/03/06 essai domi ca me semble bizarre qu'on ai un decalage entre profsol et le reste
              !          profsol = profsol + int(P_epc(i))
              izc(i) = nint(profsol)
            endif
          end do
          
          ! domi 22/03/06 je mets un test mais c'est à voir avce nadine
          if (P_profhum > profsol) P_profhum = profsol
          decalprof = nint(profsol-xx)
          ! domi 22/03/06 j'ai raouté int pour xx
          !              decalprof = nint(profsol-int(xx))
          ! recalcul des couches de transition
          icel(0) = 0
          ncel    = 0
          npro    = 0
          nhe     = 0
          do i = 1,nh
            nepc = nint(epci(i))
            nhe  = nhe+int(epci(i))
            npro = npro + nepc
            if (P_epd(i) <= 0) then
              call EnvoyerMsgHistorique(198)
              P_epd(i) = 1
            endif
        
            nhel = int(nepc/P_epd(i))
            if (nhel > nepc) nhel = nepc
            if (nhel < 1) nhel  = 1
            P_epd(i) = int(nepc/nhel)
            do j = 1,nhel
              ncel = ncel +1
              icel(ncel) = icel(ncel-1) + P_epd(i)
            end do
            if (icel(ncel) /= npro) then
              ncel = ncel + 1
              icel(ncel) = npro
            endif
            izcel(i) = ncel
          end do
  
          ! réaffectation des humidités pour les horizons profonds
          do i = 1,int(profsol)
            
            j = int(profsol)-i+1
            
            if (j > izc(2)) then
              hur(j) = hur(j-decalprof)
              hucc(j) = hucc(j-decalprof)
              humin(j) = humin(j-decalprof)
              nit(j) = nit(j-decalprof)
              amm(j) = amm(j-decalprof)
              tsol(j) = tsol(j-decalprof)

              flrac(j) = flrac(j-decalprof)
              ! DR 12/05/06 on avait oublié de mettre ce qui concerne la mineralisation
              Chum(j) = Chum(j-decalprof)
              Nhum(j) = Nhum(j-decalprof)
              do ir = 1,nbResidus
                Cbio(j,ir) = Cbio(j-decalprof,ir)
                Cres(j,ir) = Cres(j-decalprof,ir)
                Nbio(j,ir) = Nbio(j-decalprof,ir)
                Nres(j,ir) = Nres(j-decalprof,ir)
              end do
              if (P_coderacine == 2) rl(j) = rl(j-decalprof)
            endif
            
            if (j > izc(1) .and. j <= izc(2) .and. P_proftrav(is) <= P_proflabour .and. P_proftrav(is) > P_proftravmin) then
              hur(j) = hur(j-decalprof)
              hucc(j) = hucc(j-decalprof)
              humin(j) = humin(j-decalprof)
              nit(j) = nit(j-decalprof)
              amm(j) = amm(j-decalprof)
              tsol(j) = tsol(j-decalprof)

              flrac(j) = flrac(j-decalprof)
              ! DR 12/05/06 on avait oublié de mettre ce qui concerne la mineralisation
              Chum(j) = Chum(j-decalprof)
              Nhum(j) = Nhum(j-decalprof)
              do ir = 1,nbResidus
                Cbio(j,ir) = Cbio(j-decalprof,ir)
                Cres(j,ir) = Cres(j-decalprof,ir)
                Nbio(j,ir) = Nbio(j-decalprof,ir)
                Nres(j,ir) = Nres(j-decalprof,ir)
              end do
              if (P_coderacine == 2) rl(j) = rl(j-decalprof)
            endif
            
            if (j <= izc(1)) then
              hur(j) = qeaucouche1 / int(epci(1))
              hucc(j) = hucc(1)
              humin(j) = humin(1)
              tsol(j) = TS(1)
              nit(j) = AZnit(1)/int(epci(1))
              amm(j) = AZamm(1)/int(epci(1))
              if (P_coderacine == 2) then
                rl(j) = lrach(1)
              else
                flrac(j) = lrach(1)/P_lvopt
              endif
              do ir = 1,nbResidus
                Cbio(j,ir) = CumCbio(1,ir)/int(epci(1))
                Nbio(j,ir) = CumNbio(1,ir)/int(epci(1))
                Cres(j,ir) = CumCres(1,ir)/int(epci(1))
                Nres(j,ir) = CumNres(1,ir)/int(epci(1))
              end do
              Chum(j) = CumChum(1)/int(epci(1))
              Nhum(j) = CumNhum(1)/int(epci(1))
            endif
          
            if (j > izc(1) .and. j <= izc(2) .and. P_proftrav(is) > P_proflabour) then
              hur(j)   = qeaucouche2 / int(epci(2))
              hucc(j)  = hucc(2)
              humin(j) = humin(2)
              tsol(j)  = TS(2)
              nit(j)   = AZnit(2) / int(epci(2))
              amm(j)   = AZamm(2) / int(epci(2))
              if (P_coderacine == 2) then
                rl(j) = lrach(2)
              else
                flrac(j) = lrach(2) / P_lvopt
              endif
              do ir = 1,nbResidus
                Cbio(j,ir) = CumCbio(2,ir) / int(epci(2))
                Nbio(j,ir) = CumNbio(2,ir) / int(epci(2))
                Cres(j,ir) = CumCres(2,ir) / int(epci(2))
                Nres(j,ir) = CumNres(2,ir) / int(epci(2))
              end do
              Chum(j) = CumChum(2)/int(epci(2))
              Nhum(j) = CumNhum(2)/int(epci(2))
            endif
  
          end do
           
          write(146,*) n,profsol,'travail du sol'

          do iz = 1,nh
            P_epc(iz) = epci(iz)
          end do
  
          ! DR 050506 on essaie car on a un pb d'arrondi 
          profsol = 0
          do iz = 1,nh
            epci(iz) = int(P_epc(iz))
            profsol = profsol + epci(iz)
          end do
  
          ! DR le 15 02 07 je le remonte sinon on le calcule pour rien autnat de fois que de travail de sol
          ! NB le 15/02/07
          ! DR 24/07/08 on modifi la fonction
          !      call modifWsol(P_epc(1),da(1),da(2),P_zesx,P_q0) 
          call modifWsol(P_epc(1),da(1),da(2),da_ini(1),da_ini(2),zesx_ini,q0_ini,P_zesx,P_q0)
          
          
        endif
!--        write(146,1461)ansemis,n,(P_epc(iz),iz = 1,4),P_infil(1),P_infil(2),P_profhum,da(1),da(2),P_epd(1),P_epd(2)

      end do

      nhe = 0         
      do i = 1,NH
        HR(i) = 0.
        izmax = int(P_epc(i))
        do iz = 1,izmax
          HR(i) = HR(i) + HUR(nhe+iz)+sat(nhe+iz)
          !if(n==1) write(71,*) n,i,iz,hr(i),hur(nhe+iz),sat(nhe+iz)
        end do
        nhe = nhe+izmax
        HR(i) = HR(i)/da(i)*10/P_epc(i)
        !if(n==1) write(71,*) n,i,hr(i),da(i),P_epc(i)
      end do

      !write(71,*)n,'apres detass',(hr(i),i = 1,4),(aznit(i),i = 1,4)


return
end subroutine detassement       

 
 
