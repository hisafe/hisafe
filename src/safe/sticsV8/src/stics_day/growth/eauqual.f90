! ************************************************* c
! *    calcul de la teneur en eau des organes     * c
! *    NB le 10/09/01                             * c
! * P_stdrpdes = somme de température correspondant * c
! *    au jour de début de deshydratation         * c
! ************************************************* c
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module deals with the quality of harvested organs: water content and biochemical composition
!> - Stics book paragraphe 4.3, page 81-83
!>
!> Water content:
!>
!! For harvested organs, it is assumed that the water content is constant (h2ofrvert) up to the stage idebdes. This stage may occur before physiological maturity.
!! For indeterminate plants, it does not occur at the same time for all fruit cohorts but it corresponds to one of the age classes.
!! We shall call this stage "onset of fruit water dynamics" that can be hydration or dehydration which results from the concomitant water and dry matter influx
!! into the fruit or grain. As from this stage, we assume that there is a "programmed" time course in the water content of fruits, and this is expressed
!! using the deshydbase parameter (g water.g FM-1.d-1), which day after day will modify the fruit water content (teaugrain) from its initial value h2ofrvert.
!! For dehydration deshydbase is positive; if the programme evolution tends towards hydration, deshydbase is negative. Dehydration may be accelerated (or provoked)
!! by water stress, which is characterised by the difference between the crop and air temperatures. The proportionality coefficient is called tempdeshyd
!! in g water.g FM-1. degreesC-1.
!>
!> Biochemical composition:
!>
!! To complete the components of the quality of simulated harvested organs, we propose a very simple estimate of the sugar and oil contents.
!! From the beginning of fruit/grain filling until physiological maturity, we assume that there is a gradual increase in the proportions of these two types
!! of components in the dry matter of fruits. This increase is determined using the virpropsucre and vitprophuile parameters expressed in g.g DM-1.degree.day-1.
!! The combination of this evolution and the evolution in the water content in fruits produces contents based on fresh matter, which depends on the development
!! of each crop. For indeterminate crops, the calculation is made for each age category separately, and then combined for all age categories.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine eauqual(n,P_deshydbase,P_tempdeshyd,tcult,tairveille,ndrp,nrec,P_codeindetermin,P_nboite,P_stdrpdes,P_dureefruit, & ! IN
                   nfruit,pousfruit,pdsfruit,P_h2ofrvert,frplusp,P_vitpropsucre,P_vitprophuile,magrain,                      &
                   somcourdrp,nmat,maenfruit,nlev,masec,mafeuilverte,P_h2ofeuilverte,mafeuiljaune,P_h2ofeuiljaune,           &
                   matigestruc,P_h2otigestruc,resperenne,P_h2oreserve,                                                       &
                   deshyd,eaufruit,ndebdes,teaugrain,h2orec,sucre,huile,sucreder,huileder,sucrems,huilems,               & ! INOUT
                   pdsfruitfrais,mafraisrec,CNgrain,mafraisfeuille,mafraistige,mafraisres,mafrais)

USE Messages

  implicit none
  
!: Arguments 
  
  integer, intent(IN)    :: n  
  real,    intent(IN)    :: P_deshydbase  !> // PARAMETER // phenological rate of evolution of fruit water content (>0 or <0) // g water.g MF-1.degree C-1 // PARPLT // 1
  real,    intent(IN)    :: P_tempdeshyd  !> // PARAMETER // increase in the fruit dehydration due to the increase of crop temperature (Tcult-Tair) // % water degree C-1 // PARPLT // 1
  real,    intent(IN)    :: tcult   !> // OUTPUT // Crop surface temperature (daily average) // degree C
  real,    intent(IN)    :: tairveille   !> // OUTPUT // Mean air temperature the previous day // degree C
  integer, intent(IN)    :: ndrp  
  integer, intent(IN)    :: nrec  
  integer, intent(IN)    :: P_codeindetermin  !> // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0 
  integer, intent(IN)    :: P_nboite  !> // PARAMETER // "Number of  box  or  age class  of fruits for the fruit growth for the indeterminate crops " // SD // PARPLT // 1
  real,    intent(IN)    :: P_stdrpdes  !> // PARAMETER // phasic duration between the DRP stage and the beginning of the water fruit dynamics  // degree.days // PARPLT // 1 
  real,    intent(IN)    :: P_dureefruit  !> // PARAMETER // total growth period of a fruit at the setting stage to the physiological maturity // degree.days // PARPLT // 1 
  real,    intent(IN)    :: nfruit(P_nboite)        ! 1 to P_nboite    // OUTPUT // Number of fruits in box 5 // nb fruits
  real,    intent(IN)    :: pousfruit   !> // OUTPUT // Number of fruits transferred from one box to the next  // nb fruits
  real,    intent(IN)    :: pdsfruit(P_nboite)      ! 1 to P_nboite    // OUTPUT // Weight of fruits in box 3 // g m-2
  real,    intent(IN)    :: P_h2ofrvert  !> // PARAMETER // water content of fruits before the beginning of hydrous evolution (DEBDESHYD) // g water g-1 MF // PARPLT // 1 
  real,    intent(IN)    :: frplusp  
  real,    intent(IN)    :: P_vitpropsucre  !> // PARAMETER // increase rate of sugar harvest index  // g sugar g MS-1  j-1 // PARPLT // 1 
  real,    intent(IN)    :: P_vitprophuile  !> // PARAMETER // increase rate of oil harvest index  // g oil g MS-1 j-1 // PARPLT // 1 
  real,    intent(IN)    :: magrain  
  real,    intent(IN)    :: somcourdrp  
  integer, intent(IN)    :: nmat  
  real,    intent(IN)    :: maenfruit   !> // OUTPUT // Dry matter of harvested organ envelopes // t.ha-1
  integer, intent(IN)    :: nlev  
  real,    intent(IN)    :: masec   !> // OUTPUT // Aboveground dry matter  // t.ha-1
  real,    intent(IN)    :: mafeuilverte   !> // OUTPUT // Dry matter of green leaves // t.ha-1
  real,    intent(IN)    :: P_h2ofeuilverte  !> // PARAMETER // water content of green leaves // g water g-1 MF // PARPLT // 1 
  real,    intent(IN)    :: mafeuiljaune   !> // OUTPUT // Dry matter of yellow leaves // t.ha-1
  real,    intent(IN)    :: P_h2ofeuiljaune  !> // PARAMETER // water content of yellow leaves // g water g-1 MF // PARPLT // 1 
  real,    intent(IN)    :: matigestruc   !> // OUTPUT // Dry matter of stems (only structural parts) // t.ha-1
  real,    intent(IN)    :: P_h2otigestruc  !> // PARAMETER // structural stem part water content // g eau g-1 MF // PARPLT // 1 
  real,    intent(IN)    :: resperenne   !> // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
  real,    intent(IN)    :: P_h2oreserve  !> // PARAMETER // reserve water content // g eau g-1 MF // PARPLT // 1 
  
  
  real,    intent(INOUT) :: deshyd(0:P_nboite)       ! 0 to P_nboite
  real,    intent(INOUT) :: eaufruit(0:P_nboite)     ! 0 to P_nboite
  integer, intent(INOUT) :: ndebdes  
  real,    intent(INOUT) :: teaugrain  
  real,    intent(INOUT) :: h2orec   !> // OUTPUT // Water content of harvested organs // %
  real,    intent(INOUT) :: sucre   !> // OUTPUT // Sugar content of fresh harvested organs // % (of fresh weight)
  real,    intent(INOUT) :: huile   !> // OUTPUT // Oil content of fresh harvested organs // % (of fresh weight)
  real,    intent(INOUT) :: sucreder  
  real,    intent(INOUT) :: huileder  
  real,    intent(INOUT) :: sucrems  
  real,    intent(INOUT) :: huilems  
  real,    intent(INOUT) :: pdsfruitfrais   !> // OUTPUT // Total weight of fresh fruits // g m-2
  real,    intent(INOUT) :: mafraisrec  
  real,    intent(INOUT) :: CNgrain   !> // OUTPUT // Nitrogen concentration of grains  // %
  real,    intent(INOUT) :: mafraisfeuille  
  real,    intent(INOUT) :: mafraistige  
  real,    intent(INOUT) :: mafraisres  
  real,    intent(INOUT) :: mafrais   !> // OUTPUT // Aboveground fresh matter // t.ha-1
  
  
!: Variables locales 
  integer :: nboitedes  !>  
  integer :: i  
  real :: pdevfruit(P_nboite),default  

! Pour les fonctions
  real DESHYDRATE,BIOCHFR
      


      DESHYDRATE = P_deshydbase + P_tempdeshyd * (tcult - tairveille)

  ! TODO : une fonction eausucrehuile(...), on externalise le test.
      
      !if (ndrp == 0 .or. (nrec > 0 .and. n >= nrec)) then

      if (ndrp > 0 .and. (nrec == 0 .or. n < nrec)) then

        ! ** les organes de récolte
      
        ! ** initialisation à drp
        if (P_codeindetermin == 2) then
  
          if (n == ndrp .and. ndrp > 0) then
            ! *- domi - 17/10/2003 - modif qu'on ecrit le commentaire sinon Marie elle rouspete
            do i = 0, P_nboite
              deshyd(i) = 0.0
              eaufruit(i) = 0.0
            end do
          endif
  
          ! ** calcul de la boite de début de deshydratation
          do i = 1, P_nboite
            if (P_stdrpdes > P_dureefruit * i / P_nboite) then
              nboitedes = i + 1
              if (P_stdrpdes > P_dureefruit) nboitedes = P_nboite
            endif
          end do
          
          ! NB le 13/04/06 ajout du test sur P_stdrpdes pour Inaki
          if (P_stdrpdes < P_dureefruit / P_nboite) nboitedes = 1
  
          ! ** ML - le 23/03/04
          ! *- nboitedes ne peut pas prendre la valeur 0: dans ce cas cela
          ! *- signifie que P_stdrpdes a été mal parametre; on ajoute un
          ! *- message d'erreur et on stoppe le programme
          if (nboitedes == 0.) then
            call EnvoyerMsgHistorique(314)
           !stop
           call exit(9)
          endif
  
          !: NB - le 18/12
          if (nfruit(nboitedes) > 0 .and. ndebdes == 0) then
            ndebdes = n
          endif
          
          ! ** calcul de la deshydratation
          do i = 1, P_nboite
            if (i >= nboitedes .and. nfruit(i) > 0 .and. nrec == 0) then
              if (pousfruit <= 0.0) then
                deshyd(i) = deshyd(i) + DESHYDRATE
              else
                deshyd(i) = deshyd(i-1) + DESHYDRATE
              endif
            endif
          end do
  
          !: Quantité d'eau dans les fruits
          !- ML - 07/04/06 : modif pour éviter que d'avoir eaufruit = 0 dans le cas où
          !- on a des fruits mais non encore remplis (si la température empêche le remplissage)
          !--  eaufruit(1) = pdsfruit(1)*P_h2ofrvert / (1.0-P_h2ofrvert)
          if (pdsfruit(1) /= 0.) then
            eaufruit(1) = pdsfruit(1) * P_h2ofrvert / (1.0 - P_h2ofrvert)
         else
            eaufruit(1) = P_h2ofrvert
         endif
          
        
          ! ** quantité d'eau dans les fruits en croissance
          ! *- NB - le 10/09/01 - boucle jusqu'à P_nboite-1 
          do i = 2, P_nboite-1
            if ((P_h2ofrvert - deshyd(i)) <= 0.0) then
              call EnvoyerMsgHistorique(311)
             !stop
             call exit(9)
            endif
            eaufruit(i) = pdsfruit(i) * (P_h2ofrvert - deshyd(i)) / (1 - P_h2ofrvert + deshyd(i))
          end do
  
          ! ** quantité d'eau dans les fruits murs
          if (nfruit(P_nboite) > 0.) then
            if (pousfruit == 0) then
              eaufruit(P_nboite) = pdsfruit(P_nboite) * (P_h2ofrvert-deshyd(P_nboite)) /(1-P_h2ofrvert+deshyd(P_nboite))
            else
              eaufruit(P_nboite) = eaufruit(P_nboite)+frplusp * (P_h2ofrvert-deshyd(P_nboite-1)) /   &
                                  (1-P_h2ofrvert+deshyd(P_nboite-1))
            endif
            teaugrain = eaufruit(P_nboite) / (pdsfruit(P_nboite) + eaufruit(P_nboite))
          endif
  
          ! ** attention test en cas d'hydratation du fruit NB le 24/08/01
          if (teaugrain >= 1.0) then
            call EnvoyerMsgHistorique(312)
            !stop
            call exit(9)
          endif
  
          ! ** quantité d'eau de tous les fruits
          h2orec = 0.0
          do i = 1,P_nboite
            h2orec = h2orec+eaufruit(i)
          end do
  
          ! ** calcul des teneurs en sucre et en huile
          sucre = 0.0
          huile = 0.0
          do i = 1,P_nboite-1
            pdevfruit(i) = float(i)/P_nboite
            sucre = sucre + BIOCHFR(pdevfruit(i),P_vitpropsucre,P_dureefruit)*pdsfruit(i)
            huile = huile + BIOCHFR(pdevfruit(i),P_vitprophuile,P_dureefruit)*pdsfruit(i)
          end do
  
          ! ** ajout de la quantité de sucre de la dernière boite
          ! *- qui est à la même teneur /MS que l'avant dernière
          sucreder = BIOCHFR(pdevfruit(P_nboite-1),P_vitpropsucre,P_dureefruit) * pdsfruit(P_nboite)
       
          sucre = sucre + sucreder
  
          huileder = BIOCHFR(pdevfruit(P_nboite-1),P_vitprophuile,P_dureefruit) * pdsfruit(P_nboite)
  
          huile = huile+huileder
  
          ! ** domi 11/05/01 test sur calcul
          if ((magrain + h2orec) > 0.0) then
  
            ! ** prop en sucre et huile /MS
            ! *- Nb le 23/05 déplacement dans le test
            sucrems = sucre / magrain
            huilems = huile / magrain
            ! ** conversion du poids en matière fraiche pour la dernière boite
            pdsfruitfrais = (magrain + h2orec) / 100.
            !-- pdsfruitfrais = (pdsfruit(P_nboite)+eaufruit(P_nboite))/100.
  
            ! ** prop en sucre et huile /MF
            sucre = sucre / (magrain+h2orec)
            huile = huile / (magrain+h2orec)
            ! ** teneur en eau de l'ensemble des fruits
            h2orec = h2orec / (magrain+h2orec)
            ! ** prop en sucre et huile /MF pour la dernière boite
            sucreder = sucreder / (pdsfruit(P_nboite)+teaugrain)
            huileder = huileder / (pdsfruit(P_nboite)+teaugrain)
          endif
        ! ** fin indéterminées
        endif
  
  
        if (P_codeindetermin == 1) then
          ! ** eau
          if (somcourdrp <= P_stdrpdes) then
            teaugrain = P_h2ofrvert
          else
            ! ** NB - le 18/12
            if (ndebdes == 0) ndebdes = n
            teaugrain = teaugrain - DESHYDRATE
          endif
          h2orec = teaugrain
          pdsfruitfrais = magrain / (1.0-teaugrain)/100.
          ! ** sucre et huile
          if (nmat == 0 .or. n == nmat) then
            sucrems = BIOCHFR(1.0,P_vitpropsucre,somcourdrp)
            huilems = BIOCHFR(1.0,P_vitprophuile,somcourdrp)
          endif
          ! ** NB - le 02/05 - sucre et huile exprimés en % matière fraiche
          sucre = sucrems * (1.0-h2orec)
          huile = huilems * (1.0-h2orec)
        endif
        
        mafraisrec = (magrain/100. + maenfruit)/(1.0-h2orec)
  
        ! ** controle du bilan
        if (1.0 < (CNgrain/100.0 + sucrems+huilems)) then
  
          default = -1 + CNgrain/100. + sucrems+huilems
       
          sucrems = sucrems*(1.0-default)
          huilems = huilems*(1.0-default)
          CNgrain = CNgrain*(1.0-default)
          sucre = sucrems*(1.0-h2orec)
          huile = huilems*(1.0-h2orec)
          call EnvoyerMsgHistorique(313)
          
        endif
      
      endif
      
  ! TODO : on coupe ici => eauorganesvegetatifs(...)   
      
      !: 2/ Les organes végétatifs
  
      if (nlev == 0 .or. masec <= 0.) then
        mafraisfeuille = 0.0
        mafraistige = 0.0
        mafraisres = 0.0
      else
        ! ** faire des tests dans initial pour les différents h2o <1
        mafraisfeuille = mafeuilverte/(1.0-P_h2ofeuilverte) + mafeuiljaune/(1.0-P_h2ofeuiljaune)
     
        mafraistige = matigestruc/(1.0-P_h2otigestruc) 
     
        mafraisres = resperenne/(1.0-P_h2oreserve)
     
      endif

      ! ** cumul des matières fraiches NB le 08/05
      mafrais = mafraisfeuille+mafraistige + mafraisres+mafraisrec
      
      
      

return
end subroutine eauqual   
      
! ***************************************************************** c                                   
! * fonction de calcul de la teneur en eau des organes récoltés   * c
! ***************************************************************** c
! --      function DESHYDRATE(tcult,tairveille)

! ** P_deshydbase  = deshydratation nominale en % eau/jour
! *- tcult =  température de culture
! *- tairveille =  température de l'air
! *- P_tempdeshyd = facteur multiplicatif de (tcult-tairveille) pour augmenter la deshydratation

! --      real P_deshydbase,tcult,tairveille,P_tempdeshyd
! --      common/eaupl/P_deshydbase,P_tempdeshyd
      
! --      DESHYDRATE = P_deshydbase+P_tempdeshyd*(tcult-tairveille)
  
! --      return
! --      end

    
! **************************************************** c
! *    teneur en sucre ou en huile des fruits        * c
! **************************************************** c
real function BIOCHFR(pdevfruit,vit,P_dureefruit)

!: Arguments            
  real, intent(IN) :: pdevfruit  
  real, intent(IN) :: vit  
  real, intent(IN) :: P_dureefruit  !> // PARAMETER // total growth period of a fruit at the setting stage to the physiological maturity // degree.days // PARPLT // 1 

      BIOCHFR = vit * pdevfruit * P_dureefruit
      BIOCHFR = min(BIOCHFR, 1.0)
    
return
end
 
 
