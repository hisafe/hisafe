!************************************************************************   
!  Projet DST-Stage Célia Moulin
!  introduction de la possibilité de modifier les paramètres du sol
!  sous l'influence des techniques
!  le 15/02/06
!************************************************************************
! *------------------------------------------------------------------------------------------------------------------------------------------* c
!> compaction of the soil
!>- Stics book paragraphe 6.5.2, page 112
!!
!! It was assumed that the machines likely to cause severe compaction are only those involved in sowing and harvesting operations.
!! The parameters involved, for sowing and harvesting separately, are the average soil water content above which compaction occurs
!! (HUMSEUILTASSSEMT and HUMSEUILTASSRECT in proportion to the field capacity), the soil depth affected (PROFHUMSEMT and PROFHUMRECT in cm)
!! and the resulting bulk density (DASEMT and DARECT in g cm-3). The maximum effect of compaction is in the two top soil layers.
!!
!! In the model, compaction results in an increase in bulk density, a decrease in layer thickness and a decrease in their infiltrability.
!!
!! The modification of the soil geometry has repercussions on water and nitrogen profiles: a conservation of the intra-layer amounts
!! is assumed with uniform partitioning within each layer.
! *------------------------------------------------------------------------------------------------------------------------------------------* c

subroutine tassesemisrecolte(n,nh,nbCouches,P_codeplante,nplt,nrec,P_profhumsemoir,P_prophumtasssem,              & ! IN
                             P_hccf,P_dasemis,P_profhumrecolteuse,P_prophumtassrec,P_darecolte,sat,beta_sol,          &
                             P_codeDSTnbcouche,P_coderacine,da_ini,zesx_ini,q0_ini,nbResidus,Hinit,             &
                             profsol,P_infil,da,profhum_tass,P_obstarac,icel,ncel,izcel,nhe,P_profhum,            & ! INOUT
                             izc,P_epc,P_epd,hur,nit,amm,flrac,rl,Cbio,Cres,Nres,Nbio,Chum,Nhum,hucc,humin,tsol,P_zesx,P_q0,HR)


  USE Messages, only: EnvoyerMsgHistorique

  implicit none
  
  
!: Arguments
  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nh  
  integer, intent(IN)    :: nbCouches  
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!! 1=snu
!!! 2=vig
!!! 3=pom
!!! 4=bet
!!! 5=men
!!! 6=fou
!!! 7=qui
!!!  character(len=3), intent(IN) :: P_codeplante  !> // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0
  integer, intent(IN) :: P_codeplante  !> // PARAMETER // Name code of the plant in 3 letters // * // PARPLT // 0
  integer, intent(IN)    :: nplt  
  integer, intent(IN)    :: nrec  
  real,    intent(IN)    :: P_profhumsemoir  !> // PARAMETER // soil depth for which moisture is considered as a reference to allow or not sowing in the case of soil compaction activation // cm // PARTEC // 1 
  real,    intent(IN)    :: P_prophumtasssem  !> // PARAMETER // field capacity proportion above witch compaction may occur (to delay sowing) // SD // PARAM // 1 
  real,    intent(IN)    :: P_hccf(nh)  !> // PARAMETER // gravimetric water content at field capacity of each soil layer (/fine earth) (table) // % w // PARSOL // 1
  real,    intent(IN)    :: P_dasemis  !> // PARAMETER // bulk density modification as a result of sowing // g.cm-3 // PARTEC // 1 
  real,    intent(IN)    :: P_profhumrecolteuse  !> // PARAMETER // soil depth for which moisture is considered as a reference to allow or not harvesting in the case of soil compaction activation // cm // PARTEC // 1 
  real,    intent(IN)    :: P_prophumtassrec  !> // PARAMETER // field capacity proportion above witch compaction may occur (to delay harvest) // SD // PARAM // 1 
  real,    intent(IN)    :: P_darecolte  !> // PARAMETER // bulk density modification as a result of harvest // g.cm-3 // PARSOL // 1 
  real,    intent(IN)    :: sat(nbCouches)  
  real,    intent(IN)    :: beta_sol(2)  
  integer, intent(IN)    :: P_codeDSTnbcouche  !> // PARAMETER // activation of the number of compacted soil layers: one layer (1), two layers (2) // code 1/2 // PARTEC // 0 
  integer, intent(IN)    :: P_coderacine  !> // PARAMETER // Choice of estimation module of growth root in volume: standard profile (1) or by actual density (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: da_ini(2)  
  real,    intent(IN)    :: zesx_ini  
  real,    intent(IN)    :: q0_ini  
  integer, intent(IN)    :: nbResidus  
  real,    intent(IN)    :: Hinit(2)  
  
  real,    intent(INOUT) :: profsol  
  real,    intent(INOUT) :: P_infil(nh)  !> // PARAMETER // infiltrability parameter at the base of each horizon (if codemacropor = 1)// mm day-1 // PARSOL // 1   // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1
  real,    intent(INOUT) :: da(nh)   !> // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real,    intent(INOUT) :: profhum_tass(2)  
  real,    intent(INOUT) :: P_obstarac  !> // PARAMETER // Soil depth which will block the root growth  // cm // PARSOL // 1 
  integer, intent(INOUT) :: icel(0:nbCouches)  
  integer, intent(INOUT) :: ncel  
  integer, intent(INOUT) :: izcel(nh)  
  integer, intent(INOUT) :: nhe  
  real,    intent(INOUT) :: P_profhum  !> // PARAMETER // Humification depth  (max.60 cm) // cm // PARSOL // 1 
  integer, intent(INOUT) :: izc(nh)  
  real,    intent(INOUT) :: P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
  integer, intent(INOUT) :: P_epd(nh)  !> // PARAMETER //  mixing cells thickness (=2 x dispersivity) // cm // PARSOL // 1
  real,    intent(INOUT) :: hur(nbCouches)  
  real,    intent(INOUT) :: nit(nbCouches)  
  real,    intent(INOUT) :: amm(nbCouches)  
  real,    intent(INOUT) :: flrac(nbCouches)  
  real,    intent(INOUT) :: rl(nbCouches)                 ! n  
  real,    intent(INOUT) :: Cbio(nbCouches,nbResidus)  
  real,    intent(INOUT) :: Cres(nbCouches,nbResidus)
  real,    intent(INOUT) :: Nres(nbCouches,nbResidus)
  real,    intent(INOUT) :: Nbio(nbCouches,nbResidus)  
  real,    intent(INOUT) :: Chum(nbCouches)
  real,    intent(INOUT) :: Nhum(nbCouches)
  real,    intent(INOUT) :: hucc(nbCouches)  
  real,    intent(INOUT) :: humin(nbCouches)  
  real,    intent(INOUT) :: tsol(nbCouches)  
  real,    intent(INOUT) :: P_zesx  !> // PARAMETER // maximal depth of soil affected by soil evaporation // cm // PARSOL // 1 
  real,    intent(INOUT) :: P_q0  !> // PARAMETER // Parameter of the end of the maximum evaporation stage  // mm // PARSOL // 1 
  real,    intent(INOUT) :: HR(nh)   !> // OUTPUT // Water content of the horizon 5 (table)    // % pond.


!: Variables locales
  integer :: i  !>  
  integer :: nhel  !>  
  integer :: npro  !>  
  integer :: j  !>  
  integer :: nepc  !>  
  integer :: decalprof  
  real    :: xx  
  real    :: hummoy  !>  
  real    :: daini  
  integer :: numcouche  !>  
  integer :: k  !>  
  integer :: ir  

  real    :: tothur1  !>  
  real    :: totnit1  !>  
  real    :: totamm1  !>  
  real    :: totflrac1  !>  
  real    :: totrl1  
  real    :: tothur2  !>  
  real    :: totnit2  !>  
  real    :: totamm2  !>  
  real    :: totflrac2  !>  
  real    :: totrl2  
  real    :: totCbio1(nbResidus),totCres1(nbResidus),totNbio1(nbResidus),totNres1(nbResidus),totChum1,totNhum1
  real    :: totCbio2(nbResidus),totCres2(nbResidus),totNbio2(nbResidus),totNres2(nbResidus),totChum2,totNhum2

  integer :: decalprof1  !>  
  integer :: decalprof2  
  real    :: epcini(2)  
  real    :: humseuiltass  !>  
  real    :: datass  

  real    :: profhumtass  
  real    :: epci(5)  
  integer :: iz  !>  
  integer :: jdeb  !>  
  integer :: jfin  

  integer :: izmax  

  real    :: F_effettasssurinfil


      ! DR - 07/02/07 : juste pour verif car y'avait des trucs bizarre sur le calcul de hr
      nhe = 0
      do i = 1,nh
        HR(i)    = 0.
        izmax = int(P_epc(i))
        do iz = 1,izmax
          HR(i) = HR(i) + HUR(nhe+iz)+sat(nhe+iz)
          !if(n==1) write(71,*) n,i,iz,hr(i),hur(nhe+iz),sat(nhe+iz)
        end do
        nhe = nhe+izmax
        HR(i) = HR(i)/da(i)*10/int(P_epc(i))
        !if(n==1) write(71,*) n,i,hr(i),da(i),P_epc(i)
      end do
      !write(71,*)'avanttasssemis',(hr(i),i = 1,4)

! domi 15/03/06 si on fait un travail du sol le premier jour de la simulation
!  on a un pb car on a pas encore calculé HR qui est dans sortie et donc
! les valeurs à zero entraine tout un tas de calculs foireux qui finissent par
! un azote dans les premieres couche = NAN !!
! je rajoute ici le calcul de HR pour n=1
      if (n == 1)then
        HR(1) = Hinit(1)
        HR(2) = Hinit(2)
      endif

      !write(71,*)'avanttasssemis-2',(hr(i),i = 1,4)

      ! DR - 050506 : on essaie car on a un pb d'arrondi 
      profsol = 0
      do iz = 1,NH
        epci(iz) = P_epc(iz)
        profsol = profsol + epci(iz)
      end do

      ! DR et celia on essaie d'introduire le tassement eventuel du jour de semis et de recolte
!!!      if (P_codeplante /= 'snu') then
      if (P_codeplante /= 1) then
        if (n == nplt .or. n == nrec) then
          
          ! on  traite le semis
          if (n == nplt) then
            profhumtass = P_profhumsemoir
            ! DR 23/10/07 parametre remplacé par 1.2 *hcc
            !      humseuiltass = humseuiltasssem
            humseuiltass = P_prophumtasssem * P_hccf(1)
            datass = P_dasemis
            !--write(146,*)profsol,'semis'
          endif

          ! on  traite la recolte
          if (n == nrec) then
            profhumtass = P_profhumrecolteuse
            ! DR 23/10/07 parametre remplacé par 1.0 *hcc
            !      humseuiltass = humseuiltassrec
            humseuiltass = P_prophumtassrec * P_hccf(1)
            datass = P_darecolte
            !--write(146,*)profsol,'rec'
          endif

          !  on calcule l'humidite de reference
          decalprof1 = 0
          decalprof2 = 0

          hummoy = 0.
          jdeb = 1
          jfin = 0
          numcouche = 0
B1:       do i = 1,nh
            ! domi 13/11/06 on remplace j par jfin
            jfin = jfin + int(epci(i))
            do k = jdeb,jfin
              numcouche = numcouche + 1
              ! domi 09/06/06 on divise sat par da pour etre en ponderal
              ! domi 13/11/06 y'avait un bug sur le calcul de hummoy
              !        hummoy = hummoy+hur(i)/da(i)+sat(i)/da(i)
              hummoy = hummoy + hur(numcouche) / da(i) + sat(i) / da(i)
              if (numcouche == profhumtass) EXIT B1
            end do
            jdeb = jdeb + int(P_epc(i))
          end do B1
          hummoy = hummoy*10./profhumtass

          !--write(146,*)'hummoy',hummoy,'humseuiltass',humseuiltass
          !--write(146,*)'da(1)',da(1),'datass',datass
          !--write(146,1461)ansemis,(P_epc(iz),iz = 1,4),P_infil(1),P_infil(2),P_profhum,da(1),da(2),P_epd(1),P_epd(2)
          !--1461  format(2i5,9f10.2,2i5)

          ! on teste si on est en decision de semis
          if (hummoy >= humseuiltass) then
            !--write(146,*)'**** on est en tassement'

            daini = da(1)
            da(1) = datass

            !  domi 24/07/08 si on a modifié da on modifie P_infil
            P_infil(1) = F_effettasssurinfil(datass,beta_sol(1))

            epcini(1) = epci(1)
            ! NB le 13/03/08
            epci(1) = nint(epci(1)*daini/da(1))
            if (epci(1) <= 1) epci(1) = 1.0
            decalprof1 = nint(epcini(1)-epci(1))

            ! DR 30/07/08 je recalcule ca suite aux pb de P_profhum qui devient trop grand
            ! nouvelle proposition de Julie
            !      P_profhum = P_profhum*daini/da(1)
            profhum_tass(1) = profhum_tass(1)*daini/da(1)
            ! si on affect qu'une couche profhum_tass(2) reste inchange
            if (P_codeDSTnbcouche == 2) then
              daini = da(2)
              da(2) = datass
              !  domi 24/07/08 si on a modifié da on modifie P_infil
              P_infil(2) = F_effettasssurinfil(datass,beta_sol(2))

              epcini(2) = epci(2)
              ! NB le 13/03/08
              epci(2) = nint(epci(2)*daini/da(2))
              decalprof2 = nint(epcini(2)-epci(2))
              profhum_tass(2) = profhum_tass(2)*daini/da(2)
            endif    
            P_profhum = profhum_tass(1)+profhum_tass(2)

            ! on recalcule profsol
            xx = profsol
            profsol = 0.0
            do i = 1,5
              if (epci(i) /= 0.0) then
                if (i == 1) profsol = profsol + epcini(1)-decalprof1
                if (i == 2 .and. P_codeDSTnbcouche == 2) profsol = profsol + epcini(2)-decalprof2
                if (i == 2 .and. P_codeDSTnbcouche == 1) profsol = profsol + epci(2)
                if (i >= 3) profsol = profsol+epci(i)
                izc(i) = nint(profsol)
              endif
            end do

            ! domi 22/03/06 je mets un test mais c'est à voir avce nadine
            if (P_profhum > profsol) P_profhum = profsol
            decalprof = nint(profsol-xx)
            ! domi 03/05/06 si on tasse le sol faut verifier que les racines ne puisse pas aller 
            ! plus loin que profsol
            if (profsol < P_obstarac) P_obstarac = profsol
            ! recalcul des couches de transition
            icel(0) = 0
            ncel = 0
            npro = 0
            nhe = 0
            do i = 1,nh
              nepc = nint(epci(i))
              nhe = nhe+int(epci(i))
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

            ! dr et celia 03/04/06 on calcule le total d'humidité,nit et amm des couches affectes par le tassemnt
            !   pour les reaffecter dans les couches de 1cm restant dans chaque horizon (1 et 2)
            tothur1 = 0.
            totnit1 = 0.        
            totamm1 = 0.
            totflrac1 = 0.
            totrl1 = 0.
            tothur2 = 0.
            totnit2 = 0.        
            totamm2 = 0.
            totflrac2 = 0.
            totrl2 = 0.
            ! DR 12/05/06
            do ir = 1,nbResidus
              totCbio1(ir) = 0.
              totCres1(ir) = 0.
              totNbio1(ir) = 0.
              totNres1(ir) = 0.
              totCbio2(ir) = 0.
              totCres2(ir) = 0.
              totNbio2(ir) = 0.
              totNres2(ir) = 0.
            end do
            
            totChum1 = 0.
            totChum2 = 0.
            totNhum1 = 0.
            totNhum2 = 0.
            
            do i = 1, decalprof1
              
              tothur1 = tothur1+hur(i)
              totnit1 =  totnit1+nit(i)
              totamm1 = totamm1+amm(i)
              totflrac1 = totflrac1+flrac(i)
            ! dr 12/05/06
              do ir = 1,nbResidus
                totCbio1(ir) = totCbio1(ir)+Cbio(i,ir)
                totCres1(ir) = totCres1(ir)+Cres(i,ir)
                totNbio1(ir) = totNbio1(ir)+Nbio(i,ir)
                totNres1(ir) = totNres1(ir)+Nres(i,ir)
              end do
              totChum1 = totChum1+Chum(i)
              totNhum1 = totNhum1+Nhum(i)
              if (P_coderacine == 2) totrl1 = totrl1+rl(i)

            end do
            
            do i = 1,decalprof2
              
              j = i+int(epcini(1))
              tothur2 = tothur2+hur(j)
              totnit2 =  totnit2+nit(j)
              totamm2 = totamm2+amm(j)
              totflrac2 = totflrac2+flrac(j)
              ! dr 12/05/06
              do ir = 1,nbResidus
                totCbio2(ir) = totCbio2(ir)+Cbio(j,ir)
                totCres2(ir) = totCres2(ir)+Cres(j,ir)
                totNbio2(ir) = totNbio2(ir)+Nbio(j,ir)
                totNres2(ir) = totNres2(ir)+Nres(j,ir)
              end do
              totChum2 = totChum2+Chum(j)
              totNhum2 = totNhum2+Nhum(j)

              if (P_coderacine == 2) totrl2 = totrl2+rl(j)
            
            end do
  
            do i = 1,int(profsol)
              
              ! on fait la couche 1
              if (i <= izc(1)) then
                hur(i) = hur(i+decalprof1)+(tothur1/int(epci(1)))
                nit(i) = nit(i+decalprof1)+(totnit1/int(epci(1)))
                amm(i) = amm(i+decalprof1)+(totamm1/int(epci(1)))
                flrac(i) = flrac(i+decalprof1) + (totflrac1/int(epci(1)))
                if (P_coderacine == 2) rl(i) = rl(i+decalprof1)+(totrl1/int(epci(1)))
                ! dr 12/05/06
                do ir = 1,nbResidus
                  Cbio(i,ir) = Cbio(i+decalprof1,ir) + (totCbio1(ir)/int(epci(1)))
                  Cres(i,ir) = Cres(i+decalprof1,ir) + (totCres1(ir)/int(epci(1)))
                  Nbio(i,ir) = Nbio(i+decalprof1,ir) + (totNbio1(ir)/int(epci(1)))
                  Nres(i,ir) = Nres(i+decalprof1,ir) + (totNres1(ir)/int(epci(1)))
                end do
                Chum(i) = Chum(i+decalprof1)+(totChum1/int(epci(1)))
                Nhum(i) = Nhum(i+decalprof1)+(totNhum1/int(epci(1)))
                hucc(i) = hucc(i+decalprof1)
                humin(i) = humin(i+decalprof1)
                tsol(i) = tsol(i+decalprof1)
              endif

              ! on fait la couche 2
              if (P_codeDSTnbcouche == 2) then
                
                if (i > izc(1) .and. i <= izc(2)) then
                  hur(i) = hur(i+decalprof1)+(tothur2/int(epci(2)))
                  nit(i) = nit(i+decalprof1)+(totnit2/int(epci(2)))
                  amm(i) = amm(i+decalprof1)+(totamm2/int(epci(2)))
                  flrac(i) = flrac(i+decalprof1) + (totflrac2/int(epci(2)))
                  if (P_coderacine == 2) rl(i) = rl(i+decalprof1+decalprof2) + (totrl2/int(epci(2)))
                  ! dr 12/05/06
                  do ir = 1,nbResidus
                    Cbio(i,ir) = Cbio(i+decalprof1,ir) + (totCbio2(ir)/int(epci(2)))
                    Cres(i,ir) = Cres(i+decalprof1,ir) + (totCres2(ir)/int(epci(2)))
                    Nbio(i,ir) = Nbio(i+decalprof1,ir) + (totNbio2(ir)/int(epci(2)))
                    Nres(i,ir) = Nres(i+decalprof1,ir) + (totNres2(ir)/int(epci(2)))
                  end do
                  Chum(i) = Chum(i+decalprof1)+(totChum2/int(epci(2)))
                  Nhum(i) = Nhum(i+decalprof1)+(totNhum2/int(epci(2)))
                  hucc(i) = hucc(i+decalprof1+decalprof2)
                  humin(i) = humin(i+decalprof1+decalprof2)
                  tsol(i) = tsol(i+decalprof1+decalprof2)
                endif
                
                if (i > izc(2)) then
                  hur(i) = hur(i+decalprof1+decalprof2)
                  nit(i) = nit(i+decalprof1+decalprof2)
                  amm(i) = amm(i+decalprof1+decalprof2)
                  flrac(i) = flrac(i+decalprof1+decalprof2)
                  ! dr 12/05/06
                  do ir = 1,nbResidus
                    Cbio(i,ir) = Cbio(i+decalprof1+decalprof2,ir)
                    Cres(i,ir) = Cres(i+decalprof1+decalprof2,ir)
                    Nbio(i,ir) = Nbio(i+decalprof1+decalprof2,ir)
                    Nres(i,ir) = Nres(i+decalprof1+decalprof2,ir)
                  end do
                  Chum(i) = Chum(i+decalprof1+decalprof2)
                  Nhum(i) = Nhum(i+decalprof1+decalprof2)
                
                  if (P_coderacine == 2) rl(i) = rl(i+decalprof1)
                  hucc(i) = hucc(i+decalprof1)
                  humin(i) = humin(i+decalprof1)
                  tsol(i) = tsol(i+decalprof1)
                endif

              else
                if (i > izc(1)) then
                  hur(i) = hur(i+decalprof1+decalprof2)
                  nit(i) = nit(i+decalprof1+decalprof2)
                  amm(i) = amm(i+decalprof1+decalprof2)
                  flrac(i) = flrac(i+decalprof1+decalprof2)
                  if (P_coderacine == 2) rl(i) = rl(i+decalprof1+decalprof2)
                  ! dr 12/05/06
                  do ir = 1,nbResidus
                    Cbio(i,ir) = Cbio(i+decalprof1+decalprof2,ir)
                    Cres(i,ir) = Cres(i+decalprof1+decalprof2,ir)
                    Nbio(i,ir) = Nbio(i+decalprof1+decalprof2,ir)
                    Nres(i,ir) = Nres(i+decalprof1+decalprof2,ir)
                  end do
                  Chum(i) = Chum(i+decalprof1+decalprof2)
                  Nhum(i) = Nhum(i+decalprof1+decalprof2)
                
                  hucc(i) = hucc(i+decalprof1+decalprof2)
                  humin(i) = humin(i+decalprof1+decalprof2)
                  tsol(i) = tsol(i+decalprof1+decalprof2)
                endif
              endif
            
            end do

            do i = int(profsol)+1,nbCouches !1000
              hur(i) = 0.
              nit(i) = 0.
              amm(i) = 0.
              flrac(i) = 0.
              if (P_coderacine == 2) rl(i) = 0.
              ! dr 12/05/06
              do ir = 1,nbResidus
                Cbio(i,ir) = 0.
                Cres(i,ir) = 0.
                Nbio(i,ir) = 0.
                Nres(i,ir) = 0.
              end do
              Chum(i) = 0.
              Nhum(i) = 0.
              hucc(i) = 0.
              humin(i) = 0.
              tsol(i) = 0.
            end do
            
          endif

        endif
    
      endif



      ! DR 050506 on essaie car on a un pb d'arrondi 
      ! DR 07/02/07 je prends la partie entiere de P_epc sinon on a des pb quand on recalcule 
      ! HR dans sortie car on a des P_epc reelles
      profsol = 0
      do iz = 1,nh
        P_epc(iz)  = int(epci(iz))
        epci(iz) = int(epci(iz))
        profsol  = profsol + epci(iz)
      end do

      ! DR 07/02/07 juste pour verif car y'avait des trucs bizarre sur le calcul de hr
!      nhe = 0         
!      do i = 1,NH
!        HR(i)    = 0.
!        izmax = int(P_epc(i))
!        do iz = 1,izmax
!          HR(i) = HR(i) + HUR(nhe+iz)+sat(nhe+iz)
!        end do
!        nhe = nhe+izmax
!        HR(i) = HR(i)/da(i)*10/int(P_epc(i))
!      end do
      
      !write(71,*)'apres tasssemis',(hr(i),i = 1,4)

      !--call modifWsol(P_epc(1),da(1),da(2),P_zesx,P_q0) 
      call modifWsol(P_epc(1),da(1),da(2),da_ini(1),da_ini(2),zesx_ini,q0_ini,P_zesx,P_q0) 

      !--write(146,1461)ansemis,(P_epc(iz),iz = 1,4),P_infil(1),P_infil(2),P_profhum,da(1),da(2),P_epd(1),P_epd(2)

return
end subroutine tassesemisrecolte
 
 
