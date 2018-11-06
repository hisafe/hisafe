!> Module of soil
!>- Description of the structure Sol_
!>-  reading of soil parameters
module Sol


USE Messages
!DR 17/09/2012 probablement introduit pas PB en prevision de stics spatialisé
! je garde pour memo mais je mets en commentaire
!type grid
!      real,allocatable,dimension(:,:)::A
!      real,allocatable,dimension(:,:,:)::B
!end type grid LRACH




TYPE , BIND(C) :: Sol_

! dr 20/09/2012 à mettre dans le type
  integer :: nbCouchesSol_max

  integer :: itrav1  
  integer :: itrav2  
  integer :: nstoc0  
  integer :: profcalc  

  real :: Ndenit      !< // OUTPUT // Daily denitrification of nitrogen from fertiliser or soil (if option  denitrification  is activated)" // kg.ha-1.j-1
  real :: Norgeng      !< // OUTPUT // Daily organisation of nitrogen from fertiliser // kgN.ha-1.j-1
  real :: Nvolorg  
  real :: Nvoleng      !< // OUTPUT // Daily volatilisation of nitrogen from fertiliser // kgN.ha-1.j-1
  real :: QNdenit      !< // OUTPUT // "Cumulated denitrification of nitrogen from fertiliser or soil (if option  denitrification  is activated)" // kgN.ha-1
  real :: QNorgeng      !< // OUTPUT // Cumulated organisation of nitrogen from fertiliser // kgN.ha-1
  real :: QNvolorg      !< // OUTPUT // Cumulated volatilisation of nitrogen from organic inputs // kgN.ha-1
  real :: Nvolatorg  
  real :: Qminrcult  
  real :: cumvminr      !< // OUTPUT // daily mineral nitrogen arising from humus // kgN.ha-1.j-1
  real :: qdraincum      !< // OUTPUT // Cumulated quantity of water evacuated towards drains // mm
  real :: qdrain      !< // OUTPUT // Flow rate towards drains // mm j-1
  real :: remontee      !< // OUTPUT // Capillary uptake in the base of the soil profile // mm j-1
  real :: hmax      !< // OUTPUT // Maximum height of water table between drains // cm
  real :: hnappe      !< // OUTPUT // Height of water table with active effects on the plant // cm
  real :: hph      !< // OUTPUT // Maximum depth of perched water table // cm
  real :: hpb      !< // OUTPUT // Minimum depth of perched water table // cm
  real :: qlesd      !< // OUTPUT // Cumulated N-NO3 leached into drains // kgN.ha-1
  real :: Ldrains  
  real :: nitrifj      !< // OUTPUT // Daily nitrification of nitrogen (if option  nitrification  is activated) // kg.ha-1
  real :: profnappe      !< // OUTPUT // Depth of water table // cm
  real :: condenit      !< // OUTPUT // Denitrifying condition rate regard to the potential // 0-1
  real :: concno3les      !< // OUTPUT // Nitrate concentration in drainage water // mg NO3 l-1
  real :: azlesd      !< // OUTPUT // Nitric nitrogen flow in drains // kgN.ha-1 j-1
  real :: AZnit(5)      !< // OUTPUT // Amount of nitric nitrogen in the horizon 3 (table)  // kgN.ha-1
  real :: QNvoleng      !< // OUTPUT // Cumulated volatilisation of nitrogen from fertiliser // kgN.ha-1
  real :: sumes00  
  real :: sumes10  
  real :: sumes20  
  real :: supres0  
  real :: ses2j00  
  real :: sesj00  
  real :: smes020  
  real :: stoc0  
  real :: minoffrN  
  real :: NO3init(5)  
!dr 27/07/2013 ont ete transfere dans sc par bruno
!  real :: CO2sol      !< // OUTPUT // CO2 mass flow from the soil // mgCO2.m-2.d-1
!  real :: QCO2sol
  real :: vminr  
  real :: aevap  
  real :: amm(1000)
  real :: nit(0:1000)  ! pourquoi demarrer à 0 ?

  real :: P_profdenit  !< // PARAMETER // soil depth on which denitrification is active (with the appropriate option) // cm // PARSOL // 1
  integer :: P_codedenit  !< // PARAMETER // option to allow the calculation of denitrification :yes (1), no(2) // code 1/2 // PARSOL // 0
  real :: P_zesx  !< // PARAMETER // maximal depth of soil affected by soil evaporation // cm // PARSOL // 1
  real :: P_cfes  !< // PARAMETER // parameter defining the soil contribution to evaporation as a function of depth  // SD // PARSOL // 1
  real :: P_vpotdenit  !< // PARAMETER // potential rate of denitrification (per 1 cm soil layer) // kg ha-1 j-1 cm-1 // PARSOL // 1
  real :: P_z0solnu  !< // PARAMETER // roughness length of bare soil // m // PARSOL // 1

! ** parametres sol
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=12) :: P_typsol  !< // PARAMETER // Soil type // * // PARSOL // 1

  integer :: P_numsol      !< // PARAMETER // Soil number in the file PARAM.SOL // * // PARSOL // 1
  integer :: P_typecailloux(5)     !< // PARAMETER // Pebbles type defined by a volumetric mass value (masvolx) and a field capacity moisture value (HCCCX) only used  if codecailloux=1 . (typecailloux= 1:calcaire B1,  2:calcaire B2,  3:calcaire L,  4:caillasse L,  5:gravier m,  6:silex, 7:granite, 8:calcaire J, 9-10:others) // SD // PARSOL // 1
  integer :: P_epd(5)     !< // PARAMETER //  mixing cells thickness (=2 x dispersivity) // cm // PARSOL // 1
  integer :: P_codecailloux  !< // PARAMETER // option of accounting of pebbles in the soil balances (1 = yes; 2 = no)  // code1/2 // PARSOL // 0
  integer :: P_codemacropor  !< // PARAMETER // "Simulation option of water flux in the macroporosity of soils to estimate water excess and drip by  overflowing : yes(1), no (0)" // code 1/2 // PARSOL // 0
  integer :: P_codefente  !< // PARAMETER // option allowing an additional water compartment for the swelling soils (1 = yes; 2 = no) // code1/2 // PARSOL // 0
  integer :: P_codrainage  !< // PARAMETER // artificial drainage (1 = yes; 2 = no)  // code1/2 // PARSOL // 0
  integer :: P_codenitrif  !< // PARAMETER // option to activate nitrification calculation (1 = yes; 2 = no) // code 1/2 // PARSOL // 0
  integer :: P_coderemontcap  !< // PARAMETER // option to activate capillary rise (1 = yes; 2 = no)  // code1/2 // PARSOL // 0
  integer :: izcel(5)  
  integer :: izc(5)  
  integer :: ncel  
  integer :: icel(0:1000)

  real :: P_NO3initf(5)     !< // PARAMETER // initial nitric nitrogen in the soil per horizon  // kg N ha-1 // INIT // 1
  real :: P_profhum  !< // PARAMETER // Humification depth  (max.60 cm) // cm // PARSOL // 1
  real :: P_pH  !< // PARAMETER // P_pH of mixing soil + organic amendments  // SD // PARSOL // 1
  real :: P_q0  !< // PARAMETER // Parameter of the end of the maximum evaporation stage  // mm // PARSOL // 1
  real :: P_ruisolnu  !< // PARAMETER // fraction of drip rainfall (by ratio at the total rainfall) on a bare soil  // between 0 and 1 // PARSOL // 1
  real :: P_obstarac  !< // PARAMETER // Soil depth which will block the root growth  // cm // PARSOL // 1
  real :: P_profimper  !< // PARAMETER // Upper depth of the impermeable layer (from the soil surface). May be greater than the soil depth // cm // PARSOL // 1
  real :: P_ecartdrain  !< // PARAMETER // inbetween drains distance // cm // PARSOL // 1
  real :: P_Ksol  !< // PARAMETER // hydraulic conductivity in the soil above and below the drains // SD // PARSOL // 1
  real :: P_profdrain  !< // PARAMETER // drain depth // cm // PARSOL // 1
  real :: P_DAF(5)     !< // PARAMETER // Table of bulk density of the fine earth fraction in each soil layer  // g cm-3 // PARSOL // 1
  real :: P_hminf(5)     !< // PARAMETER // gravimetric water content at wilting point of each soil layer (/fine earth) (table) // % w // PARSOL // 1
  real :: P_hccf(5)     !< // PARAMETER // gravimetric water content at field capacity of each soil layer (/fine earth) (table) // % w // PARSOL // 1
  real :: da(5)      !< // OUTPUT // Bulk density in the horizon 1 // g cm-3
  real :: P_epc(5)     !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
  real :: hcc(5)  
  real :: hmin(5)  
  real :: P_cailloux(5)     !< // PARAMETER // stone volumetric content per horizon // m3 m-3 // PARSOL // 1
  real :: P_infil(0:5)     !< // PARAMETER // infiltrability parameter at the base of each horizon (if codemacropor = 1) // mm day-1 // PARSOL // 1      // OUTPUT // Infiltrability parameter at the base of the horizon 1 // mm day-1
  real :: P_calc  !< // PARAMETER // percentage of calcium carbonate in the surface layer // % // PARSOL // 1
  real :: P_argi  !< // PARAMETER // percentage of clay in the surface layer  // % // PARSOL // 1
  real :: P_Norg  !< // PARAMETER // Organic Nitrogen  content of the tilled layer  (supposed constant on the depth  P_profhum) // % pondéral // PARSOL // 1
  real :: profsol  
  real :: P_albedo  !< // PARAMETER // P_albedo of the bare dry soil // SD // PARSOL // 1
  real :: P_humcapil  !< // PARAMETER // threshold of soil gravimetric water content under which capillary rise occurs // % w // PARSOL // 1
  real :: P_capiljour  !< // PARAMETER // capillary rise upward water flux // mm j-1 // PARSOL // 1
  real :: P_concseuil  !< // PARAMETER // Minimum Concentration of soil to NH4 // kgN ha-1 mm-1 // PARSOL // 1
  real :: da_ini(2)  
  real :: q0_ini  
  real :: zesx_ini  
  real :: cumvminh      !< // OUTPUT // daily mineral nitrogen arising from organic residues // kgN.ha-1.j-1
  real :: profhum_tass(2)  

! battance
  real :: P_pluiebat  !< // PARAMETER // minimal rain quantity for the crust occurrence // mm day-1 // PARSOL // 1
  real :: P_mulchbat  !< // PARAMETER // mulch depth from which a crust occurs // cm // PARSOL // 1

  real :: pHvol      !< // OUTPUT // soil surface P_pH varying after organic residue application (such as slurry) // SD
  real :: dpH  
  real :: P_CsurNsol   !< // PARAMETER // Initial C to N ratio of soil humus // SD // PARSOL // 1 ! Bruno-declaration du parametre CN du sol
  real :: P_penterui   !< // PARAMETER // runoff coefficient taking account for plant mulch // SD // PARSOL // 1 ! 27/07/2012 ajout de ce parametre pour le bresil

!DR 10/09/2012 j'ajoute le sol pour Patrice
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=50) :: ficsol
  real  :: N_volatilisation  !< // OUTPUT // Cumulated volatilisation of nitrogen from organic inputs and fertiliser // kgN.ha-1
  real  :: epc_recal(5)
  real  :: infil_recal(5)


!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
  real :: P_Hinitf(5)     !< // PARAMETER // Table of initial gravimetric water content of each soil layer (/fine earth) // % w // INIT // 1
  real :: P_NH4initf(5)    !< // PARAMETER // Amounts of initial mineral N in the 5 soil layers (fine earth) // kg.ha-1 // INIT // 1
  real :: NH4init(5)
  real :: Hinit(5)
  real :: HR(5)      !< // OUTPUT // Water content of the horizon 5 (table)    // % pond.
  real :: AZamm(5)   !< // OUTPUT // Amounts of NH4-N in the 5 soil horizons // kg.ha-1
  real :: TS(5)      !< // OUTPUT // Mean soil temperature (mean of the 5 layers) // degree C

!!!MODIF HISAFE 5 : suppression variable inutile
!!!  real :: Ninitf(5)
!!!  real :: Ninit(5)

end type Sol_

contains

subroutine Sol_Lecture_V6(soil,path,pathsol)

implicit none

    type(Sol_), intent(INOUT) :: soil  

   ! enabling_record :le chemin pour accéder à la config
   character(len=255), intent(IN) :: path ! enabling_record
   ! enabling_record :le chemin pour accéder directement à la config
   character(len=255), intent(IN) :: pathsol ! enabling_record

!!!MODIF HISAFE on supprime les parametre de type char
   character(len=12) :: P_typsol  !< // PARAMETER // Soil type // * // PARSOL // 1

! Variables locales
      integer :: icou  

! DR 19/11/2013 Record
      integer ib0                                            ! enabling_record
      integer ib1                                            ! enabling_record
      character(len=300) :: filepluspath                     ! enabling_record
! DR 19/11/2013 Record

!!!MODIF HISAFE on supprime les parametre de type char
  character(len=50) :: ficsol

      soil%nbcouchessol_max= 1000

! DR 19/11/2013 Record
      ! to get the full path
      ib0 = len_trim(pathsol)                             ! enabling_record
      if (ib0 .ne. 0 ) then                                  ! enabling_record
         filepluspath =  pathsol                          ! enabling_record
      else
         ib1 = len_trim(path)                                ! enabling_record
         if (ib1 .eq. 0 ) then                                  ! enabling_record
            filepluspath = ficsol                          ! enabling_record
         else                                                   ! enabling_record
            filepluspath = path(1:ib1) // '/' // ficsol ! enabling_record
         endif                                                  ! enabling_record
      endif
!


    ! ouverture du fichier sol (TODO : nom à paramétrer)
    ! DR 10/09/2012 on parametre le nom du sol pour optimistics
!      open(3,file = soil%ficsol,status = 'old')
      open(3,file = filepluspath,status = 'old') ! enabling_record

 ! Bruno 18/05/12 ajout de la lecture du rapport C/N du sol (en option)
 !     soil%P_CsurNsol = 0.
 !     soil%P_penterui = 0.33
 !     read(3,*,err = 110) soil%P_numsol,soil%P_typsol,soil%P_argi,soil%P_Norg,soil%P_profhum,soil%P_calc,soil%P_pH,    &
 !                         soil%P_concseuil,soil%P_albedo,soil%P_q0,soil%P_ruisolnu,soil%P_obstarac,soil%P_pluiebat, &
 !                         soil%P_mulchbat,soil%P_zesx,soil%P_cfes,soil%P_z0solnu,soil%P_CsurNsol, soil%P_penterui
 ! 27/07/2012 les 3 on ajoute le param P_penterui pour les bresiliens (avant c'etait à 0.33 en dur dans le code : etatsurf)
 !     goto 115
 ! 110 continue
 !     call EnvoyerMsgHistorique(137)
 !     backspace(3)
 ! DR 22/08/2012 j'ajoute les 2 parametres soil%P_CsurNsol, soil%P_penterui
      read(3,*,err = 120) soil%P_numsol,P_typsol,soil%P_argi,soil%P_Norg,soil%P_profhum,soil%P_calc,soil%P_pH,    &
                          soil%P_concseuil,soil%P_albedo,soil%P_q0,soil%P_ruisolnu,soil%P_obstarac,soil%P_pluiebat, &
                          soil%P_mulchbat,soil%P_zesx,soil%P_cfes,soil%P_z0solnu,soil%P_CsurNsol, soil%P_penterui
!  115 continue
    !Bruno fin de modif pour lecture CN du sol

      read(3,*,err = 120) soil%P_numsol,soil%P_codecailloux,soil%P_codemacropor,soil%P_codefente,   &
                          soil%P_codrainage,soil%P_coderemontcap,soil%P_codenitrif,soil%P_codedenit

! DR 31/05/2011 on lit pplus dans le meme ordre
!      read(3,*,err = 120) soil%P_numsol,soil%P_capiljour,soil%P_humcapil,soil%P_profimper,soil%P_ecartdrain,  &
!                          soil%P_ksol,soil%P_profdrain,soil%P_profdenit,soil%P_vpotdenit
      read(3,*,err = 120) soil%P_numsol,soil%P_profimper,soil%P_ecartdrain,soil%P_ksol,soil%P_profdrain,     &
                          soil%P_capiljour,soil%P_humcapil,soil%P_profdenit,soil%P_vpotdenit

      do icou = 1,5
        read(3,*,ERR = 120) soil%P_numsol,soil%P_epc(icou),soil%P_hccf(icou),soil%P_hminf(icou),soil%P_DAF(icou), &
                            soil%P_cailloux(icou),soil%P_typecailloux(icou),soil%P_infil(icou),soil%P_epd(icou)

      end do

    ! fermeture du fichier
      close(3)

    ! si on est arrivé jusque là, pas d'erreur, on quitte la routine
      return

120   continue
      call EnvoyerMsgHistorique(137)
    ! fermeture du fichier
      close(3)
      !stop
      call exit(9)

return
end subroutine Sol_Lecture_V6


subroutine Sol_Tests(soil,nbCouches,P_masvolcx,beta_sol)

implicit none

    type(Sol_), intent(INOUT) :: soil
    integer,    intent(IN)    :: nbCouches
    real,       intent(IN)    :: P_masvolcx(10)   ! taille fixe, à définir/paramétrer ? 	  // PARAMETER // volumetric mass (bulk) of pebbles // g cm-3 // PARAM // 1 
    real,       intent(INOUT) :: beta_sol(2)  

! Variables locales
    integer :: i  !>  
    integer :: icou
    real    :: dax  

   ! Boucle pour unix et tests
    ! test sur P_typecailloux (ML le 22/03/04)
      do i = 1,5
        if (soil%P_epc(i) /= 0.) then
          if (soil%P_codecailloux == 2) then
            soil%P_cailloux(i) = 0.
            soil%P_typecailloux(i) = 1
          else
        ! domi 17/01/06 si on a coché cailloux et pas defini P_cailloux y'a un souci
        !  je rajoute un test
            if (soil%P_cailloux(i) /= 0 .and. soil%P_typecailloux(i) == 0) then
              call EnvoyerMsgHistorique(216)
              !stop
              call exit(9)
            endif
            if (soil%P_cailloux(i) == 0 .and. soil%P_typecailloux(i) == 0) then
              soil%P_typecailloux(i) = 1
            endif
             if (soil%P_typecailloux(i) .gt. 0.and. (soil%P_cailloux(i) .lt.0 .or.soil%P_cailloux(i) .gt.100) ) then
               call EnvoyerMsgHistorique(210)
               call exit(9)
            endif

          endif
        endif
      end do

!!!MODIF HISAFE 11 : Supression code inutile
!!!NE SERT PAS
!!!      if (soil%P_numsol /= soil%P_ichsl) then
!!!        call EnvoyerMsgHistorique(136)
!!!        !stop
!!!        call exit(9)
!!!      endif

      if (soil%P_profhum > nbCouches) then  ! il faudrait remplacer 1000 par nbCouches
        call EnvoyerMsgHistorique(138)
        !stop
        call exit(9)
      endif

      if (soil%P_profhum <= 0.) then
        call EnvoyerMsgHistorique(211)
         !stop
        call exit(9)
      endif
! NB le 15/07/05 activation de P_coderemontcap
      if (soil%P_coderemontcap == 2.) soil%P_capiljour = 0.

    ! domi - 27/08/07
    ! on force les epaisseurs en entier
      do icou = 1,5
        soil%P_epc(icou) = int(soil%P_epc(icou))
      enddo

    ! domi - 02/07/2002 - test pour pas avoir de pb d'unite avec hmin et P_hccf
      do icou = 1,5
        if (soil%P_epc(icou) == 0.) CYCLE
        if (soil%P_hccf(icou) > 0 .and. soil%P_hccf(icou) < 1.) then
          call EnvoyerMsgHistorique(212)
          !stop
          call exit(9)
        endif
        if (soil%P_hminf(icou) > 0 .and. soil%P_hminf(icou) < 1.) then
          call EnvoyerMsgHistorique(213)
          !stop
          call exit(9)
        endif
      ! dr 10/01/08 on rajoute un test pour que P_hccf ne soit pas < P_hminf
        if (soil%P_hccf(icou) > 0 .and. (soil%P_hccf(icou) < soil%P_hminf(icou))) then
          call EnvoyerMsgHistorique(217)
          !stop
          call exit(9)
        endif

      end do

    ! profondeur maximale du sol = 10m
      if (soil%P_obstarac == 1000) soil%P_obstarac = 1000-1

    ! domi - 03/07/02
      if (soil%P_profhum <= 0.) then
        call EnvoyerMsgHistorique(214,soil%P_profhum)
        !stop
        call exit(9)
      endif

    ! DR et Julie 24/07/08 on introduit une constante sol_beta dependant
    ! de l'infiltrabilite et de la densite apparente initial entrant dans le
    ! calcul de l'infiltrabilite modifi en cas de detassement ou tassement

      do icou = 1,2

      ! DR 31/07/08 on prend en compte les P_cailloux on verra avec nadine s'il fallait le faire
      ! DA
        dax = P_masvolcx(soil%P_typecailloux(icou))
        soil%da(icou) = (soil%P_cailloux(icou) * dax + (100. - soil%P_cailloux(icou)) * soil%P_daf(icou)) / 100.
      ! DR 07/08/08 on rejoute un test pour les valeurs de P_infil à zero sinon plantade
        if (soil%P_infil(icou) <= 0) soil%P_infil(icou) = 0.001
        beta_sol(icou) = (log(soil%P_infil(icou)) + 3.9) / (soil%da(icou) - 2.7)
      ! on conserve aussi da
        soil%da_ini(icou) = soil%da(icou)

      enddo

      soil%q0_ini = soil%P_q0
      soil%zesx_ini = soil%P_zesx
      soil%profhum_tass(1) = soil%P_epc(1)
      soil%profhum_tass(2) = soil%P_profhum - soil%P_epc(1)

return
end subroutine Sol_Tests


subroutine sol_test_itk(itk,profhum)

USE Itineraire_Technique

    type(ITK_),        intent(IN) :: itk
    real,       intent(IN)    :: profhum
    integer i

    ! le 24/09/2012  test sur proftrav > P_profhum
      do i = 1,itk%P_nbjtrav
        if (itk%P_proftrav(i) > profhum) then
          call EnvoyerMsgHistorique(170)
           !stop
          call exit(9)
        endif
      enddo
end subroutine sol_test_itk



subroutine Sol_Ecriture(soil,pg)

USE Parametres_Generaux
USE Stics
USE Messages

implicit none

    type(Sol_),                 intent(IN) :: soil  
    type(Parametres_Generaux_), intent(IN) :: pg

    character(len=250) :: tmp  
    integer :: icou  

   character(len=12) :: P_typsol  !< // PARAMETER // Soil type // * // PARSOL // 1
   P_typsol = "           "

       call EnvoyerMsgHistorique('')
       call EnvoyerMsgHistorique(135)
       call EnvoyerMsgHistorique(5005)
       call EnvoyerMsgHistorique(506)
!       write(tmp,'(a12,10f7.2)') soil%P_typsol,soil%P_argi,soil%P_Norg,soil%P_profhum,  &
!                                 soil%P_calc,soil%P_pH,soil%P_concseuil,soil%P_albedo,  &
!                                 soil%P_q0,soil%P_ruisolnu,soil%P_obstarac

      write(tmp,'(a12,17f12.2)')P_typsol,soil%P_argi,soil%P_Norg,soil%P_profhum,soil%P_calc, &
                    soil%P_pH,soil%P_concseuil,soil%P_albedo,soil%P_q0,soil%P_ruisolnu,soil%P_obstarac,         &
                    soil%P_pluiebat,soil%P_mulchbat,soil%P_zesx,soil%P_cfes,soil%P_z0solnu ,soil%P_CsurNsol,    &
                    soil%P_penterui



       call EnvoyerMsgHistorique(tmp)


      call EnvoyerMsgHistorique(503)
! DR 03/11/2005 rajout message pour expliquer les codes
      call EnvoyerMsgHistorique(215)
      write(tmp,'(7i16)')soil%P_codecailloux,soil%P_codemacropor,soil%P_codefente,    &
                             soil%P_codrainage,soil%P_coderemontcap,soil%P_codenitrif,soil%P_codedenit


      call EnvoyerMsgHistorique(tmp)
      call EnvoyerMsgHistorique(504)
!      write(tmp,'(5x,6f9.1)') soil%P_capiljour,soil%P_humcapil,soil%P_profimper,  &
!                              soil%P_ecartdrain,soil%P_ksol,soil%P_profdrain
      write(tmp,'(8f13.2)') soil%P_profimper,soil%P_ecartdrain,soil%P_ksol,soil%P_profdrain,     &
                    soil%P_capiljour,soil%P_humcapil,soil%P_profdenit,soil%P_vpotdenit
      call EnvoyerMsgHistorique(tmp)

!      call EnvoyerMsgHistorique('P_codedenit P_profdenit  P_vpotdenit')
!      write(tmp,'(5x,2i6,2f9.1)')soil%P_codedenit, soil%P_profdenit,soil%P_vpotdenit
!      call EnvoyerMsgHistorique(tmp)

      call EnvoyerMsgHistorique(507)

      do icou = 1,5
!        write(tmp,73) soil%P_epc(icou),soil%P_hccf(icou),soil%P_hminf(icou),      &
!                      soil%P_DAF(icou),soil%P_cailloux(icou),soil%P_infil(icou),  &
!                      soil%P_epd(icou),soil%P_typecailloux(icou)

        write(tmp,703) soil%P_epc(icou),soil%P_hccf(icou),soil%P_hminf(icou),  &
      ! domi 22/03/06 j'ecris da et non P_daf
      ! DR 10/04/2012 voir avce Marie pour savoir comment on gere le tassement et detassement et les cailloux
                      soil%P_DAF(icou),soil%P_cailloux(icou),soil%P_typecailloux(icou),    &
                      soil%P_infil(icou),soil%P_epd(icou)

        call EnvoyerMsgHistorique(tmp)
!73      format(f7.0,2f7.1,f7.2,2f7.0,2i7)
703   format(5f14.2,i14,f14.2,i14)

      end do

      if (soil%P_codenitrif == 1) then
        call EnvoyerMsgHistorique('P_hminn',pg%P_hminn)
        call EnvoyerMsgHistorique('P_hoptn',pg%P_hoptn)
        call EnvoyerMsgHistorique('P_fnx',pg%P_fnx)
        call EnvoyerMsgHistorique('P_pHminnit',pg%P_pHminnit)
        call EnvoyerMsgHistorique('P_pHmaxnit',pg%P_pHmaxnit)
        call EnvoyerMsgHistorique('P_tnitmin',pg%P_tnitmin)
        call EnvoyerMsgHistorique('P_tnitopt',pg%P_tnitopt)
        call EnvoyerMsgHistorique('P_tnitmax',pg%P_tnitmax)
        !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
        !!!call EnvoyerMsgHistorique('NH4initf1',sc%P_NH4initf(1))
        !!!call EnvoyerMsgHistorique('NH4initf2',sc%P_NH4initf(2))
        !!!call EnvoyerMsgHistorique('NH4initf3',sc%P_NH4initf(3))
        !!!call EnvoyerMsgHistorique('NH4initf4',sc%P_NH4initf(4))
        !!!call EnvoyerMsgHistorique('NH4initf5',sc%P_NH4initf(5))
        call EnvoyerMsgHistorique('NH4initf1',soil%P_NH4initf(1))
        call EnvoyerMsgHistorique('NH4initf2',soil%P_NH4initf(2))
        call EnvoyerMsgHistorique('NH4initf3',soil%P_NH4initf(3))
        call EnvoyerMsgHistorique('NH4initf4',soil%P_NH4initf(4))
        call EnvoyerMsgHistorique('NH4initf5',soil%P_NH4initf(5))
      endif
      if (soil%P_codedenit == 1) then
        call EnvoyerMsgHistorique('P_profdenit',soil%P_profdenit)
        call EnvoyerMsgHistorique('P_vpotdenit',soil%P_vpotdenit)
        call EnvoyerMsgHistorique('P_ratiodenit',pg%P_ratiodenit)
      endif

return
end subroutine Sol_Ecriture



subroutine Sol_Zero(soil)

implicit none

  type(Sol_),intent(inout) :: soil  

      soil%itrav1 = 0
      soil%itrav2 = 0
      soil%nstoc0 = 0
      soil%profcalc = 0
      soil%P_profdenit = 0
      soil%P_codedenit = 0
      soil%P_numsol = 0
      soil%P_typecailloux(:) = 0
      soil%P_epd(:) = 0
      soil%P_codecailloux = 0
      soil%P_codemacropor = 0
      soil%P_codefente = 0
      soil%P_codrainage = 0
      soil%P_codenitrif = 0
      soil%P_coderemontcap = 0
      soil%izcel(:) = 0
      soil%izc(:) = 0
      soil%ncel = 0
      soil%icel(:) = 0
! réels
      soil%Ndenit = 0.
      soil%Norgeng = 0.
      soil%Nvolorg = 0.
      soil%Nvoleng = 0.
      soil%QNdenit = 0.
      soil%QNorgeng = 0.
      soil%QNvolorg = 0.
      soil%Nvolatorg = 0.
      soil%Qminrcult = 0.
      soil%cumvminr = 0.
      soil%qdraincum = 0.
      soil%qdrain = 0.
      soil%remontee = 0.
      soil%hmax = 0.
      soil%hnappe = 0.
      soil%hph = 0.
      soil%hpb = 0.
      soil%qlesd = 0.
      soil%Ldrains = 0.
      soil%nitrifj = 0.
      soil%profnappe = 0.
      soil%condenit = 0.
      soil%concno3les = 0.
      soil%azlesd = 0.
      soil%AZnit(:) = 0.
      soil%QNvoleng = 0.
      soil%sumes00 = 0.
      soil%sumes10 = 0.
      soil%sumes20 = 0.
      soil%supres0 = 0.
      soil%ses2j00 = 0.
      soil%sesj00 = 0.
      soil%smes020 = 0.
      soil%stoc0 = 0.
      soil%minoffrN = 0.
      soil%NO3init(:) = 0.
     ! sc%CO2sol = 0. Bruno  transférés dans STICS_Zero
     ! sc%QCO2sol = 0.
      soil%vminr = 0.
      soil%aevap = 0.
      soil%amm(:) = 0.
      soil%nit(:) = 0.
      soil%P_zesx = 0.
      soil%P_cfes = 0.
      soil%P_vpotdenit = 0.
      soil%P_z0solnu = 0.
!      soil%qmulch0 = 0.
!      soil%couvermulch = 0.
      soil%P_NO3initf(:) = 0.
      soil%P_profhum = 0.
      soil%P_pH = 0.
      soil%P_q0 = 0.
      soil%P_ruisolnu = 0.
      soil%P_obstarac = 0.
      soil%P_profimper = 0.
      soil%P_ecartdrain = 0.
      soil%P_Ksol = 0.
      soil%P_profdrain = 0.
      soil%P_DAF(:) = 0.
      soil%P_hminf(:) = 0.
      soil%P_hccf(:) = 0.
      soil%da(:) = 0.
      soil%P_epc(:) = 0.
      soil%hcc(:) = 0.
      soil%hmin(:) = 0.
      soil%P_cailloux(:) = 0.
      soil%P_infil(:) = 0.
      soil%P_calc = 0.
      soil%P_argi = 0.
      soil%P_Norg = 0.
      soil%profsol = 0.
      soil%P_albedo = 0.
      soil%P_humcapil = 0.
      soil%P_capiljour = 0.
      soil%P_concseuil = 0.
      soil%da_ini(:) = 0.
      soil%q0_ini = 0.
      soil%zesx_ini = 0.
      soil%cumvminh = 0.
      soil%profhum_tass(:) = 0.
      soil%P_pluiebat = 0.
      soil%P_mulchbat = 0.
      soil%P_penterui = 0.
      soil%P_CsurNsol = 0.


!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
  soil%P_Hinitf(:) = 0.0
  soil%P_NH4initf(:) = 0.0
  soil%NH4init(:) = 0.0
 !!! soil%Ninitf(:) = 0.0
  soil%AZamm(:) = 0.0
  soil%HR(:) = 0.0
  soil%TS(:) = 0.0
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      soil%P_typsol = ''

return
end subroutine Sol_Zero


subroutine Sol_Ecriture_Hisafe(soil)

    implicit none

    type(Sol_), intent(INOUT) :: soil
   integer :: icou


    call EnvoyerMsgHistorique('   ')
    call EnvoyerMsgHistorique('Sol initialisé par Hisafe')
    call EnvoyerMsgHistorique('*********************************************')


    call EnvoyerMsgHistorique('P_argi=', soil%P_argi)
     call EnvoyerMsgHistorique('P_Norg=', soil%P_Norg)
     call EnvoyerMsgHistorique('P_profhum=', soil%P_profhum)
     call EnvoyerMsgHistorique('P_calc=', soil%P_calc)
     call EnvoyerMsgHistorique('P_pH=', soil%P_pH)
     call EnvoyerMsgHistorique('P_concseuil=', soil%P_concseuil)
     call EnvoyerMsgHistorique('P_albedo=', soil%P_albedo)
     call EnvoyerMsgHistorique('P_q0=', soil%P_q0)
     call EnvoyerMsgHistorique('P_ruisolnu=', soil%P_ruisolnu)
     call EnvoyerMsgHistorique('P_obstarac=', soil%P_obstarac)
     call EnvoyerMsgHistorique('P_pluiebat=', soil%P_pluiebat)
     call EnvoyerMsgHistorique('P_mulchbat=', soil%P_mulchbat)
     call EnvoyerMsgHistorique('P_zesx=', soil%P_zesx)
     call EnvoyerMsgHistorique('P_cfes=', soil%P_cfes)
     call EnvoyerMsgHistorique('P_z0solnu=', soil%P_z0solnu)
     call EnvoyerMsgHistorique('P_CsurNsol=', soil%P_CsurNsol)
     call EnvoyerMsgHistorique('P_penterui=', soil%P_penterui)


   call EnvoyerMsgHistorique('P_codecailloux=', soil%P_codecailloux)
   call EnvoyerMsgHistorique('P_codemacropor=', soil%P_codemacropor)
   call EnvoyerMsgHistorique('P_codefente=', soil%P_codefente)
   call EnvoyerMsgHistorique('P_codrainage=', soil%P_codrainage)
   call EnvoyerMsgHistorique('P_coderemontcap=', soil%P_coderemontcap)
   call EnvoyerMsgHistorique('P_codenitrif=', soil%P_codenitrif)
   call EnvoyerMsgHistorique('P_codedenit=', soil%P_codedenit)


  call EnvoyerMsgHistorique('P_profimper=', soil%P_profimper)
  call EnvoyerMsgHistorique('P_ecartdrain=', soil%P_ecartdrain)
  call EnvoyerMsgHistorique('P_ksol=', soil%P_ksol)
  call EnvoyerMsgHistorique('P_capiljour=', soil%P_capiljour)
  call EnvoyerMsgHistorique('P_humcapil=', soil%P_humcapil)
  call EnvoyerMsgHistorique('P_profdenit=', soil%P_profdenit)
  call EnvoyerMsgHistorique('P_vpotdenit=', soil%P_vpotdenit)


      do icou = 1,5
         call EnvoyerMsgHistorique('icou=', icou)
         call EnvoyerMsgHistorique('P_epc=', soil%P_epc(icou))
         call EnvoyerMsgHistorique('P_hccf=', soil%P_hccf(icou))
         call EnvoyerMsgHistorique('P_hminf=', soil%P_hminf(icou))
         call EnvoyerMsgHistorique('P_DAF=', soil%P_DAF(icou))
         call EnvoyerMsgHistorique('P_cailloux=', soil%P_cailloux(icou))
         call EnvoyerMsgHistorique('P_typecailloux=', soil%P_typecailloux(icou))
         call EnvoyerMsgHistorique('P_infil=', soil%P_infil(icou))
         call EnvoyerMsgHistorique('P_epd=', soil%P_epd(icou))
      end do




end subroutine Sol_Ecriture_Hisafe

end module Sol
 
 
