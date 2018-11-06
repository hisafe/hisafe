!> write to the file history.sti
!!
!! plant parameters
subroutine Plante_Ecriture(p,sc,itk,pg)

USE Plante
USE Stics
!!!USE Station
USE Itineraire_Technique
USE Parametres_Generaux
USE Messages

implicit none


    type(Plante_),              intent(INOUT) :: p
    type(Stics_Communs_),       intent(INOUT) :: sc
!!!    type(Station_),             intent(INOUT) :: sta
    type(ITK_),                 intent(INOUT) :: itk  
    type(Parametres_Generaux_), intent(INOUT) :: pg  

!: Tests de cohérence et message dans history.sti

    call EnvoyerMsgHistorique('   ')
    call EnvoyerMsgHistorique('FICHIER PLANTE')
    call EnvoyerMsgHistorique('*********************************************')



    ! le profil type
      if (p%P_zpente >= p%P_zprlim .or. p%P_zlabour >= p%P_zpente .or. p%P_zlabour >= p%P_zprlim) then
        call EnvoyerMsgHistorique(117)
        !stop
        call exit(9)
      endif

      if (sc%P_nbplantes > 1 .and. p%P_coderacine /= 2) then
        call EnvoyerMsgHistorique(377)
        !stop
        call exit(9)
      endif

      if (p%P_coderacine == 2 .and. p%P_draclong < p%P_dlaimax*1e5) then
        call EnvoyerMsgHistorique(119)
      endif

    ! en CAS, P_codebeso obligatoirement égal à 2
      if (sc%P_nbplantes > 1 .and. p%P_codebeso /= 2) then
        p%P_codebeso = 2
        call EnvoyerMsgHistorique(438)
      endif

    ! on ne peut pas avoir 2 stades consécutifs nuls
      if (p%P_stlevamf(itk%P_variete) <= 0. .and. p%P_stamflax(itk%P_variete) <= 0.) then
        call EnvoyerMsgHistorique(5110)
        !stop
        call exit (9)
      endif
!
      if (p%P_stamflax(itk%P_variete) <= 0.0 .and. p%P_stlaxsen(itk%P_variete) <= 0.0) then
        call EnvoyerMsgHistorique(5111)
         !stop
        call exit(9)
      endif
!
      if (p%P_stlaxsen(itk%P_variete) <= 0.0 .and. p%P_stsenlan(itk%P_variete) <= 0.0) then
        call EnvoyerMsgHistorique(5112)
        !stop
        call exit(9)
      endif

      if (p%P_sensrsec >= 1.) then
        p%P_sensrsec = 1
        call EnvoyerMsgHistorique(5113)
      endif

      if (p%P_nbgrmax(itk%P_variete) <= 0 .and. p%P_codeindetermin == 1) then
        call EnvoyerMsgHistorique(370)
        !stop
        call exit(9)
      endif

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!   if (p%P_stoprac /= 'sen' .and. p%P_stoprac /= 'lax'  .and. p%P_stoprac /= 'flo' .and. p%P_stoprac /= 'rec') then
      if (p%P_stoprac /= 11 .and. p%P_stoprac /= 6  .and. p%P_stoprac /= 7 .and. p%P_stoprac /= 13) then
        call EnvoyerMsgHistorique(371)
        !stop
        call exit(9)
      endif

      if (p%P_codeperenne == 2) then
        call EnvoyerMsgHistorique(372)
      endif

    ! DR 28/08/07
      if (p%P_codeperenne == 1 .and. p%P_julvernal > sc%iplt(p%ipl)) then
        p%P_julvernal = sc%iplt(p%ipl)
        call EnvoyerMsgHistorique(448)
      endif


! ** écriture des paramètres actifs dans le mouchard HISTORY.STI
!!!      call EnvoyerMsgHistorique('   ')
!!!      call EnvoyerMsgHistorique(121)
!!!      call EnvoyerMsgHistorique(5005)
!!!      call EnvoyerMsgHistorique('P_fplt ',p%P_fplt)
! plant name and group
      call EnvoyerMsgHistorique('P_codeplante ',p%P_codeplante)
 ! DR 21/03/2014 je verifie que tous les params y soient et j'explicite les messages suivant les codes
      if(p%P_codemonocot == 1) call EnvoyerMsgHistorique(5195,p%P_codemonocot)
      if(p%P_codemonocot == 2) call EnvoyerMsgHistorique(5196,p%P_codemonocot)
! effect of atmospheric CO2 concentration
!!!MODIF HISAFE on evite de passer l'objet sta à ce niveau
!!!      if (sta%P_codeclichange == 2) then
        call EnvoyerMsgHistorique('P_alphaCO2',p%P_alphaCO2)
      ! test sur alpha co2
        if (p%P_alphaco2 >= 2.) then
          call EnvoyerMsgHistorique(361)
          !stop
          call exit(9)
        endif
!!!      endif
! phasic development
      call EnvoyerMsgHistorique('P_tdmin ',p%P_tdmin)
      call EnvoyerMsgHistorique('P_tdmax ',p%P_tdmax)
      call EnvoyerMsgHistorique('P_codetemp ',p%P_codetemp)
      if (p%P_codetemp == 1) then
        call EnvoyerMsgHistorique(122)
        if (p%P_codegdh == 1) call EnvoyerMsgHistorique(373)
        if (p%P_codegdh == 2) call EnvoyerMsgHistorique(374)
      else
        call EnvoyerMsgHistorique(123)
        call EnvoyerMsgHistorique('P_coeflevamf ',p%P_coeflevamf)
        call EnvoyerMsgHistorique('P_coefamflax ',p%P_coefamflax)
        call EnvoyerMsgHistorique('P_coeflaxsen ',p%P_coeflaxsen)
        call EnvoyerMsgHistorique('P_coefsenlan ',p%P_coefsenlan)
        call EnvoyerMsgHistorique('P_coeflevdrp ',p%P_coeflevdrp)
        call EnvoyerMsgHistorique('P_coefdrpmat ',p%P_coefdrpmat)
        call EnvoyerMsgHistorique('P_coefflodrp ',p%P_coefflodrp)
      endif
!
        call EnvoyerMsgHistorique('P_codephot ',p%P_codephot)
      if (p%P_codephot == 1) then
        call EnvoyerMsgHistorique('P_phobase ',p%P_phobase)
        call EnvoyerMsgHistorique('P_phosat ',p%P_phosat)
      endif
!
      if (p%P_coderetflo == 1) then
        call EnvoyerMsgHistorique(124)
        call EnvoyerMsgHistorique('P_stressdev ',p%P_stressdev)
      endif
!        call EnvoyerMsgHistorique('P_codebfroid ',p%P_codebfroid)
!
      if (p%P_codebfroid == 2) then
        call EnvoyerMsgHistorique(5123)
        call EnvoyerMsgHistorique('P_jvcmini ',p%P_jvcmini)
        if (p%P_codeperenne == 2) then
          call EnvoyerMsgHistorique('P_julvernal ',p%P_julvernal)
        endif
        call EnvoyerMsgHistorique('P_tfroid ',p%P_tfroid)
        call EnvoyerMsgHistorique('P_ampfroid ',p%P_ampfroid)
      endif
      if (p%P_codebfroid == 3) then
        call EnvoyerMsgHistorique(5124)
        call EnvoyerMsgHistorique('P_stdordebour ',p%P_stdordebour)
        call EnvoyerMsgHistorique('P_tdmindeb ',p%P_tdmindeb)
        call EnvoyerMsgHistorique('P_tdmaxdeb ',p%P_tdmaxdeb)
        if (p%P_codedormance == 1) then
          call EnvoyerMsgHistorique(5125)
          call EnvoyerMsgHistorique('P_ifindorm ',p%P_ifindorm)
        endif
        if (p%P_codedormance == 2) then
          call EnvoyerMsgHistorique(5126)
        endif
        if (p%P_codedormance == 3) then
          call EnvoyerMsgHistorique(5127)
          call EnvoyerMsgHistorique('P_idebdorm ',p%P_idebdorm)
          call EnvoyerMsgHistorique('P_q10',p%P_q10)
        endif
        if(p%P_codegdhdeb == 1) call EnvoyerMsgHistorique(5197,p%P_codegdhdeb)
        if(p%P_codegdhdeb == 2) call EnvoyerMsgHistorique(5198,p%P_codegdhdeb)
!      ! ml et dr - 23/03/2004
        if (p%P_codeperenne == 1) then
          call EnvoyerMsgHistorique(130)
          !stop
          call exit(9)
        endif
      endif
          call EnvoyerMsgHistorique('P_codeperenne ',p%P_codeperenne)
! emergence and starting
      if (p%P_codeperenne == 1) then
        call EnvoyerMsgHistorique(5128)
        if (p%P_codegermin == 1) then
          call EnvoyerMsgHistorique(5129)
          call EnvoyerMsgHistorique('P_tgmin ',p%P_tgmin)
          call EnvoyerMsgHistorique('P_stpltger ',p%P_stpltger)
        endif
        if (p%P_codehypo == 1) then
          call EnvoyerMsgHistorique(5130)
          call EnvoyerMsgHistorique('P_belong ',p%P_belong)
          call EnvoyerMsgHistorique('P_celong ',p%P_celong)
          call EnvoyerMsgHistorique('P_elmax ',p%P_elmax)
          call EnvoyerMsgHistorique('P_nlevlim1 ',p%P_nlevlim1)
          call EnvoyerMsgHistorique('P_nlevlim2 ',p%P_nlevlim2)
          call EnvoyerMsgHistorique('P_vigueurbat' ,p%P_vigueurbat)
        else
          call EnvoyerMsgHistorique(5131)
          call EnvoyerMsgHistorique('P_laiplantule ',p%P_laiplantule)
          call EnvoyerMsgHistorique('P_nbfeuilplant ',p%P_nbfeuilplant)
          call EnvoyerMsgHistorique('P_masecplantule ',p%P_masecplantule)
          call EnvoyerMsgHistorique('P_zracplantule ',p%P_zracplantule)
!        ! ml et dr - 23/03/04
          !!!MODIF HISAFE 1 : suppression des chaines de caractères
          !!!if (p%P_stade0 == 'lev') then
          if (p%P_stade0 == 4) then
            call EnvoyerMsgHistorique(376)
            !stop
            call exit(9)
          endif
        endif
!      ! ml et dr - 23/03/04
        !!MODIF HISAFE 1 : suppression des chaines de caractères
        !!!if (p%P_codebfroid == 1 .and. p%P_stade0 == 'dor') then
        if (p%P_codebfroid == 1 .and. p%P_stade0 == 3) then
          call EnvoyerMsgHistorique(131)
          !stop
          call exit(9)
        endif
      else
        call EnvoyerMsgHistorique(5132)
        call EnvoyerMsgHistorique('P_resperenne0',p%P_resperenne0)
      endif
! leaves
      call EnvoyerMsgHistorique('P_phyllotherme ',p%P_phyllotherme)
      call EnvoyerMsgHistorique('P_bdens ',p%P_bdens)
      call EnvoyerMsgHistorique('P_laicomp ',p%P_laicomp)
      call EnvoyerMsgHistorique('P_hautbase ',p%P_hautbase)
      call EnvoyerMsgHistorique('P_hautmax ',p%P_hautmax)
      call EnvoyerMsgHistorique('P_tcxstop ',p%P_tcxstop)
      if (p%P_codelaitr == 1) then
        call EnvoyerMsgHistorique(5133)
        call EnvoyerMsgHistorique('P_vlaimax ',p%P_vlaimax)
        call EnvoyerMsgHistorique('P_pentlaimax ',p%P_pentlaimax)
        call EnvoyerMsgHistorique('P_udlaimax ',p%P_udlaimax)
        call EnvoyerMsgHistorique('P_ratiodurvieI ',p%P_ratiodurvieI)
        call EnvoyerMsgHistorique('P_tcmin ',p%P_tcmin)
        call EnvoyerMsgHistorique('P_tcmax ',p%P_tcmax)
        call EnvoyerMsgHistorique('P_ratiosen ',p%P_ratiosen)
        call EnvoyerMsgHistorique('P_abscission ',p% P_abscission)
        call EnvoyerMsgHistorique('P_parazofmorte ',p%P_parazofmorte)
        call EnvoyerMsgHistorique('P_innturgmin ',p%P_innturgmin)
        call EnvoyerMsgHistorique('P_dlaimin ',p%P_dlaimin)
        if (p%P_codlainet == 1) then
          call EnvoyerMsgHistorique(5134)
          call EnvoyerMsgHistorique('P_dlaimax ',p%P_dlaimax)
          call EnvoyerMsgHistorique('P_tustressmin ',p%P_tustressmin)
        else
          call EnvoyerMsgHistorique(5135)
          call EnvoyerMsgHistorique('P_dlaimaxbrut ',p%P_dlaimaxbrut)
          call EnvoyerMsgHistorique('P_durviesupmax ',p%P_durviesupmax)
          call EnvoyerMsgHistorique('P_innsen ',p%P_innsen)
          call EnvoyerMsgHistorique('P_rapsenturg ',p%P_rapsenturg)
        endif
        if (p%P_codestrphot == 1) then
          call EnvoyerMsgHistorique(5136)
          call EnvoyerMsgHistorique('P_phobasesen ',p%P_phobasesen)
          call EnvoyerMsgHistorique('P_dltamsmaxsen ',p%P_dltamsmaxsen)
          call EnvoyerMsgHistorique('P_dltamsminsen ',p%P_dltamsminsen)
          call EnvoyerMsgHistorique('P_alphaphot ',p%P_alphaphot)
        endif
      else
        call EnvoyerMsgHistorique(5122)
        if (sc%P_nbplantes > 1) then
          call EnvoyerMsgHistorique(5000)
         !stop
          call exit(9)
        endif
        call EnvoyerMsgHistorique('P_tauxrecouvmax ',p%P_tauxrecouvmax)
        call EnvoyerMsgHistorique('P_tauxrecouvkmax ',p%P_tauxrecouvkmax)
        call EnvoyerMsgHistorique('P_pentrecouv ',p%P_pentrecouv)
        call EnvoyerMsgHistorique('P_infrecouv',p%P_infrecouv)
      endif
! radiation interception
      if (p%P_codetransrad == 1)then
       call EnvoyerMsgHistorique(5121)
       call EnvoyerMsgHistorique('P_extin ',p%P_extin)
      endif
      if (p%P_codetransrad == 2 .and. itk%P_codetradtec == 2) then
        call EnvoyerMsgHistorique(126)
        !stop
        call exit(9)
      endif
      call EnvoyerMsgHistorique('p%P_codetransrad', p%P_codetransrad)
      call EnvoyerMsgHistorique('p%P_codebeso', p%P_codebeso)
      if (p%P_codetransrad == 2) then
        if (p%P_codebeso == 1) then
          call EnvoyerMsgHistorique(127, p%P_extin)
        endif
        call EnvoyerMsgHistorique('P_ktrou ',p%P_ktrou)
        call EnvoyerMsgHistorique('P_adfol ',p%P_adfol)
        call EnvoyerMsgHistorique('P_dfolbas ',p%P_dfolbas)
        call EnvoyerMsgHistorique('P_dfolhaut ',p%P_dfolhaut)
        call EnvoyerMsgHistorique('P_forme ',p%P_forme)
        call EnvoyerMsgHistorique('P_rapforme ',p%P_rapforme)
      endif
        call EnvoyerMsgHistorique(125)
! shoot biomass growth
      call EnvoyerMsgHistorique('P_temin ',p%P_temin)
      call EnvoyerMsgHistorique('P_temax ',p%P_temax)
      call EnvoyerMsgHistorique('P_teopt ',p%P_teopt)
      call EnvoyerMsgHistorique('P_teoptbis ',p%P_teoptbis)
      call EnvoyerMsgHistorique('P_efcroijuv ',p%P_efcroijuv)
      call EnvoyerMsgHistorique('P_efcroiveg ',p%P_efcroiveg)
      call EnvoyerMsgHistorique('P_efcroirepro ',p%P_efcroirepro)
      if (p%P_codeindetermin == 2 .or. p%P_codeperenne == 2) then
        call EnvoyerMsgHistorique('P_remobres ',p%P_remobres)
      endif
      call EnvoyerMsgHistorique('P_coefmshaut ',p%P_coefmshaut)
! partitioning of biomass in organs
      call EnvoyerMsgHistorique('slamax ',p%P_slamax)
      call EnvoyerMsgHistorique('slamin ',p%P_slamin)
      call EnvoyerMsgHistorique('P_tigefeuil ',p%P_tigefeuil)
      call EnvoyerMsgHistorique('P_envfruit ',p%P_envfruit)
      call EnvoyerMsgHistorique('P_sea ',p%P_sea)
! yield formation
      if (p%P_codeindetermin == 1) then
        call EnvoyerMsgHistorique(5137)
        call EnvoyerMsgHistorique('P_nbjgrain ',p%P_nbjgrain)
        call EnvoyerMsgHistorique('P_cgrain ',p%P_cgrain)
        call EnvoyerMsgHistorique('P_cgrainv0 ',p%P_cgrainv0)
        call EnvoyerMsgHistorique('P_nbgrmin ',p%P_nbgrmin)
        if (p%P_codeir == 1) then
          call EnvoyerMsgHistorique(5138)
          call EnvoyerMsgHistorique('P_vitircarb ',p%P_vitircarb)
          call EnvoyerMsgHistorique('P_irmax ',p%P_irmax)
        else
           call EnvoyerMsgHistorique(5139)
          call EnvoyerMsgHistorique('P_vitircarbT ',p%P_vitircarbT)
        endif
      else
        call EnvoyerMsgHistorique(5140)
        call EnvoyerMsgHistorique('P_nboite ',p%P_nboite)
        call EnvoyerMsgHistorique('P_allocfrmax ',p%P_allocfrmax)
        call EnvoyerMsgHistorique('P_afpf ',p%P_afpf)
        call EnvoyerMsgHistorique('P_bfpf ',p%P_bfpf)
        call EnvoyerMsgHistorique('P_cfpf ',p%P_cfpf)
        call EnvoyerMsgHistorique('P_dfpf ',p%P_dfpf)
        call EnvoyerMsgHistorique('P_stdrpnou ',p%P_stdrpnou)
        call EnvoyerMsgHistorique('P_spfrmin ',p%P_spfrmin)
        call EnvoyerMsgHistorique('P_spfrmax ',p%P_spfrmax)
        call EnvoyerMsgHistorique('P_splaimin ',p%P_splaimin)
        call EnvoyerMsgHistorique('P_splaimax ',p%P_splaimax)
        if (p%P_codcalinflo == 1) then
          call EnvoyerMsgHistorique(5141)
          call EnvoyerMsgHistorique('P_nbinflo ',p%P_nbinflo)
        else
          call EnvoyerMsgHistorique(5142)
          call EnvoyerMsgHistorique('P_inflomax ',p%P_inflomax)
          call EnvoyerMsgHistorique('P_pentinflores ',p%P_pentinflores)
        endif
      endif
      call EnvoyerMsgHistorique('P_vitpropsucre ',p%P_vitpropsucre)
      call EnvoyerMsgHistorique('P_vitprophuile ',p%P_vitprophuile)
      call EnvoyerMsgHistorique('P_vitirazo ',p%P_vitirazo)
      if (p%P_codetremp == 1) then
        call EnvoyerMsgHistorique(5143)
        call EnvoyerMsgHistorique('P_tminremp ',p%P_tminremp)
        call EnvoyerMsgHistorique('P_tmaxremp ',p%P_tmaxremp)
      endif
! roots
      call EnvoyerMsgHistorique('P_sensanox ',p%P_sensanox)
      call EnvoyerMsgHistorique('P_stoprac ',p%P_stoprac)
      call EnvoyerMsgHistorique('P_sensrsec ',p%P_sensrsec)
      call EnvoyerMsgHistorique('P_contrdamax ',p%P_contrdamax)
      if (p%P_codetemprac == 1) then
        call EnvoyerMsgHistorique(128)
      else
        call EnvoyerMsgHistorique(129)
      endif
      if (p%P_coderacine == 1) then
        call EnvoyerMsgHistorique(5144)
        call EnvoyerMsgHistorique('P_zlabour ',p%P_zlabour)
        call EnvoyerMsgHistorique('P_zpente ',p%P_zpente)
        call EnvoyerMsgHistorique('P_zprlim ',p%P_zprlim)
      else
        call EnvoyerMsgHistorique(5145)
        call EnvoyerMsgHistorique('P_draclong ',p%P_draclong)
        call EnvoyerMsgHistorique('P_debsenrac ',p%P_debsenrac)
        call EnvoyerMsgHistorique('P_lvfront ',p%P_lvfront)
        call EnvoyerMsgHistorique('P_longsperac ',p%P_longsperac)
        if (p%P_codazorac == 1) then
          call EnvoyerMsgHistorique(5146)
          call EnvoyerMsgHistorique('P_minefnra ',p%P_minefnra)
          call EnvoyerMsgHistorique('P_minazorac ',p%P_minazorac)
          call EnvoyerMsgHistorique('P_maxazorac',p%P_maxazorac)
        endif
      endif
        if (p%P_codtrophrac == 1) then
          call EnvoyerMsgHistorique(5147)
          call EnvoyerMsgHistorique('P_repracpermax ',p%P_repracpermax)
          call EnvoyerMsgHistorique('P_repracpermin ',p%P_repracpermin)
          call EnvoyerMsgHistorique('P_krepracperm',p%P_krepracperm)
        endif
        if (p%P_codtrophrac == 2) then
          call EnvoyerMsgHistorique(5148)
          call EnvoyerMsgHistorique('P_repracseumax ',p%P_repracseumax)
          call EnvoyerMsgHistorique('P_repracseumin ',p%P_repracseumin)
          call EnvoyerMsgHistorique('P_krepracseu',p%P_krepracseu)
        endif
! frost
      call EnvoyerMsgHistorique('P_tletale ',p%P_tletale)
      call EnvoyerMsgHistorique('P_tdebgel ',p%P_tdebgel)
      if (p%P_codgellev == 2) then
        call EnvoyerMsgHistorique(5149)
        call EnvoyerMsgHistorique('P_nbfgellev ',p%P_nbfgellev)
        call EnvoyerMsgHistorique('P_tgellev10 ',p%P_tgellev10)
        call EnvoyerMsgHistorique('P_tgellev90 ',p%P_tgellev90)
      endif
      if (p%P_codgeljuv == 2) then
        call EnvoyerMsgHistorique(5150)
        call EnvoyerMsgHistorique('P_tgeljuv10 ',p%P_tgeljuv10)
        call EnvoyerMsgHistorique('P_tgeljuv90 ',p%P_tgeljuv90)
      endif
      if (p%P_codgelveg == 2) then
        call EnvoyerMsgHistorique(5151)
        call EnvoyerMsgHistorique('P_tgelveg10 ',p%P_tgelveg10)
        call EnvoyerMsgHistorique('P_tgelveg90 ',p%P_tgelveg90)
      endif
      if (p%P_codgelflo == 2) then
        call EnvoyerMsgHistorique(5152)
        call EnvoyerMsgHistorique('P_tgelflo10 ',p%P_tgelflo10)
        call EnvoyerMsgHistorique('P_tgelflo90 ',p%P_tgelflo90)
      endif
! water
      call EnvoyerMsgHistorique('P_psisto ',p%P_psisto)
      call EnvoyerMsgHistorique('P_psiturg ',p%P_psiturg)
      call EnvoyerMsgHistorique('P_h2ofeuilverte ',p%P_h2ofeuilverte)
      call EnvoyerMsgHistorique('P_h2ofeuiljaune ',p%P_h2ofeuiljaune)
      call EnvoyerMsgHistorique('P_h2otigestruc ',p%P_h2otigestruc)
      call EnvoyerMsgHistorique('P_h2oreserve ',p%P_h2oreserve)
      call EnvoyerMsgHistorique('P_h2ofrvert ',p%P_h2ofrvert)
      call EnvoyerMsgHistorique('P_deshydbase ',p%P_deshydbase)
      call EnvoyerMsgHistorique('P_tempdeshyd ',p%P_tempdeshyd)
      if (p%P_codebeso == 1) then
        call EnvoyerMsgHistorique(5153)
        call EnvoyerMsgHistorique('P_kmax ',p%P_kmax)
      else
        call EnvoyerMsgHistorique(5154)
        call EnvoyerMsgHistorique('P_rsmin ',p%P_rsmin)
      endif
      if (p%P_codeintercept == 1) then
        call EnvoyerMsgHistorique(5155)
        call EnvoyerMsgHistorique('P_mouillabil ',p%P_mouillabil)
        call EnvoyerMsgHistorique('P_stemflowmax ',p%P_stemflowmax)
        call EnvoyerMsgHistorique('P_kstemflow ',p%P_kstemflow)
      endif
! Nitrogen
!    ! modif Bruno
      call EnvoyerMsgHistorique('P_Vmax1 ',p% P_Vmax1)
      call EnvoyerMsgHistorique('P_Kmabs1 ',p%P_Kmabs1)
!    ! fin modif
      call EnvoyerMsgHistorique('P_Vmax2 ',p%P_Vmax2)
      call EnvoyerMsgHistorique('P_Kmabs2 ',p%P_Kmabs2)
      call EnvoyerMsgHistorique('P_adil ',p%P_adil)
      call EnvoyerMsgHistorique('P_bdil ',p%P_bdil)
      call EnvoyerMsgHistorique('P_masecNmax ',p%P_masecNmax)
      call EnvoyerMsgHistorique('P_INNmin ',p%P_Innmin)
      call EnvoyerMsgHistorique('P_INNimin ',p%P_Innimin)
      call EnvoyerMsgHistorique('P_inngrain1 ',p%P_inngrain1)
      call EnvoyerMsgHistorique('P_inngrain2 ',p%P_inngrain2)
     if (p%P_codeplisoleN == 1) then
        call EnvoyerMsgHistorique(5156)
        call EnvoyerMsgHistorique('P_adilmax ',p%P_adilmax)
        call EnvoyerMsgHistorique('P_bdilmax ',p%P_bdilmax)
     else
        call EnvoyerMsgHistorique(5157)
        call EnvoyerMsgHistorique('P_Nmeta ',p%P_Nmeta)
        call EnvoyerMsgHistorique('P_masecmeta ',p%P_masecmeta)
        call EnvoyerMsgHistorique('P_Nreserve ',p%P_Nreserve)
     endif
      if (p%P_codelegume == 2) then
        call EnvoyerMsgHistorique(132)
        if (pg%P_codesymbiose == 2) then
          call EnvoyerMsgHistorique('P_stlevdno ',p%P_stlevdno)
          call EnvoyerMsgHistorique('P_stdnofno ',p%P_stdnofno)
          call EnvoyerMsgHistorique('P_stfnofvino ',p%P_stfnofvino)
          call EnvoyerMsgHistorique('P_vitno ',p%P_vitno)
          call EnvoyerMsgHistorique('P_profnod ',p%P_profnod)
          call EnvoyerMsgHistorique('P_concNnodseuil ',p%P_concNnodseuil)
          call EnvoyerMsgHistorique('P_concNrac0 ',p%P_concNrac0)
          call EnvoyerMsgHistorique('P_concNrac100 ',p%P_concNrac100)
          call EnvoyerMsgHistorique('P_tempnod1 ',p%P_tempnod1)
          call EnvoyerMsgHistorique('P_tempnod2 ',p%P_tempnod2)
          call EnvoyerMsgHistorique('P_tempnod3 ',p%P_tempnod3)
          call EnvoyerMsgHistorique('P_tempnod4 ',p%P_tempnod4)
          if (p%P_codefixpot == 1 ) then
           call EnvoyerMsgHistorique(5158)
           call EnvoyerMsgHistorique('P_fixmax ',p%P_fixmax)
          else
           call EnvoyerMsgHistorique(5159)
           call EnvoyerMsgHistorique('P_fixmaxveg ',p%P_fixmaxveg)
           call EnvoyerMsgHistorique('P_fixmaxgr ',p%P_fixmaxgr)
          endif
        endif
      endif
! ahah
      if (p%P_codazofruit == 2) call EnvoyerMsgHistorique(232)
! BBCH
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!MODIF HISAFE ne sert pas
!!!        if(p%P_stadebbchplt .ne. '-99') call  EnvoyerMsgHistorique('stadebbchplt',p%P_stadebbchplt)
!!!        if(p%P_stadebbchger .ne. '-99') call  EnvoyerMsgHistorique('stadebbchger',p%P_stadebbchger)
!!!        if(p%P_stadebbchlev .ne. '-99') call  EnvoyerMsgHistorique('stadebbchlev',p%P_stadebbchlev)
!!!        if(p%P_stadebbchamf .ne. '-99') call  EnvoyerMsgHistorique('stadebbchamf',p%P_stadebbchamf)
!!!        if(p%P_stadebbchlax .ne. '-99') call  EnvoyerMsgHistorique('stadebbchlax',p%P_stadebbchlax)
!!!        if(p%P_stadebbchsen .ne. '-99') call  EnvoyerMsgHistorique('stadebbchsen',p%P_stadebbchsen)
!!!        if(p%P_stadebbchflo .ne. '-99') call  EnvoyerMsgHistorique('stadebbchflo',p%P_stadebbchflo)
!!!        if(p%P_stadebbchdrp .ne. '-99') call  EnvoyerMsgHistorique('stadebbchdrp',p%P_stadebbchdrp)
!!!        if(p%P_stadebbchnou .ne. '-99') call  EnvoyerMsgHistorique('stadebbchnou',p%P_stadebbchnou)
!!!        if(p%P_stadebbchdebdes .ne. '-99') call  EnvoyerMsgHistorique('stadebbchdebdes',p%P_stadebbchdebdes)
!!!        if(p%P_stadebbchmat .ne. '-99') call  EnvoyerMsgHistorique('stadebbchmat',p%P_stadebbchmat)
!!!        if(p%P_stadebbchrec .ne. '-99') call  EnvoyerMsgHistorique('stadebbchrec',p%P_stadebbchrec)
!!!        if(p%P_stadebbchfindorm .ne. '-99') call  EnvoyerMsgHistorique('stadebbchfindorm',p%P_stadebbchfindorm)

!cultivar parameters
      call EnvoyerMsgHistorique('P_codevar ',p%P_codevar(itk%P_variete))
      call EnvoyerMsgHistorique('P_stlevamf ',p%P_stlevamf(itk%P_variete))
      call EnvoyerMsgHistorique('P_stamflax ',p%P_stamflax(itk%P_variete))
      call EnvoyerMsgHistorique('P_stlevdrp ',p%P_stlevdrp(itk%P_variete))
      call EnvoyerMsgHistorique('P_stflodrp ',p%P_stflodrp(itk%P_variete))
      call EnvoyerMsgHistorique('P_stdrpdes ',p%P_stdrpdes(itk%P_variete))
      call EnvoyerMsgHistorique('P_pgrainmaxi ',p%P_pgrainmaxi(itk%P_variete))
      call EnvoyerMsgHistorique('P_adens ',p%P_adens(itk%P_variete))
      call EnvoyerMsgHistorique('P_croirac ',p%P_croirac(itk%P_variete))
      call EnvoyerMsgHistorique('P_durvieF ',p%P_durvieF(itk%P_variete))
      if (p%P_codebfroid /= 1) call EnvoyerMsgHistorique('P_jvc ',p%P_jvc(itk%P_variete))
      if (p%P_codephot == 1) then
        call EnvoyerMsgHistorique('P_sensiphot ',p%P_sensiphot(itk%P_variete))
      endif
      if (p%P_codeindetermin == 1) then
        call EnvoyerMsgHistorique('P_nbgrmax ',p%P_nbgrmax(itk%P_variete))
        call EnvoyerMsgHistorique('P_stdrpmat ',p%P_stdrpmat(itk%P_variete))
!      else
        call EnvoyerMsgHistorique('P_afruitpot ',p%P_afruitpot(itk%P_variete))
        call EnvoyerMsgHistorique('P_dureefruit ',p%P_dureefruit(itk%P_variete))
      endif
      if (p%P_codlainet == 1) then
        call EnvoyerMsgHistorique('P_stlaxsen ',p%P_stlaxsen(itk%P_variete))
        call EnvoyerMsgHistorique('P_stsenlan ',p%P_stsenlan(itk%P_variete))
      endif
!
    ! domi - 03/02/04 - juste un message en attendant que nadine regarde
      if (p%P_codlainet == 2) call EnvoyerMsgHistorique(375)

    ! domi 03/11/05
      if (p%P_abscission > 1 .or. p%P_abscission < 0) then
        call EnvoyerMsgHistorique(378)
        !stop
        call exit(9)
      endif

    ! ML et DR 12/02/08 pb d'incoherence de param
      if (p%P_codebfroid < 3) then
        p%P_codegdhdeb = 1
        call EnvoyerMsgHistorique(5120)
      endif

return
end subroutine Plante_Ecriture
 
 
