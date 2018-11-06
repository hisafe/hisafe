!*********************************************************************
!     lecture et initialisation des parametres nouvelle version
!     en attente d'etre redirigé dans leurs fichiers respectifs
!     fichier paramv6.par
!     23/11/07 ajout des parametres d'adaptation au CC de la MO
!********************************************************************
!> subroutine of general parameters v6
!> - reading of the generals parameters of the file tempoparv6.sti
SUBROUTINE Stics_Lecture_Transit(t,P_nbplantes, path, pathtempoparv6)

USE Stics
USE Messages

implicit none

    type(Stics_Transit_), intent(INOUT) :: t  
    integer , intent(IN)::P_nbplantes  !> // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0 

   ! enabling_record :le chemin pour accéder à la config
   character(len=255), intent(IN) :: path ! enabling_record
   ! enabling_record :le chemin pour accéder directement à la config
   character(len=255), intent(IN) :: pathtempoparv6 ! enabling_record


! Variables locales
      character(len=20) :: para  
      integer           :: nbparamspot  


      integer ib0                                                 ! enabling_record
      integer ib1                                                 ! enabling_record
      character(len=300) :: filepluspath                          ! enabling_record


! Bruno: ajout de 5 nouveaux parametres relatifs a limitation decomposition par N min
!  integer, intent(IN) :: P_codeNmindec   !> // PARAMETER // option to activate the available N :yes (1), no(2) // code 1/2

! DR 210508 + 3 parametres decisionsemis
      nbparamspot = 44+6    ! le nombre de paramètres du fichier à lire. Permet de vérifier en partie que les fichiers lus
                            ! correspondent à ce qu'on attend.

!DR 19/11/2013 pour Record
     ! to get the full path
      ib0 = len_trim(pathtempoparv6)                             ! enabling_record
      if (ib0 .ne. 0 ) then                                         ! enabling_record
         filepluspath =  pathtempoparv6                          ! enabling_record
      else
         ib1 = len_trim(path)                                     ! enabling_record
         if (ib1 .eq. 0 ) then                                       ! enabling_record
            filepluspath = "tempoparv6.sti"                          ! enabling_record
         else                                                        ! enabling_record
            filepluspath = path(1:ib1) // '/' // "tempoparv6.sti" ! enabling_record
         endif                                                       ! enabling_record
      endif
! fin record

    ! ouverture du fichier de paramètres en transit
    open (46,file = filepluspath,status = 'old') ! enabling_record


    ! lecture des paramètres
!specificités cultures fauchées
!***********************************
      read (46,*) para
      read (46,*,err = 250) t%P_codetempfauche
      read (46,*) para
      read (46,*,err = 250) t%P_coefracoupe(1)
      read (46,*) para
      read (46,*,err = 250) t%P_coefracoupe(2)
!specificités Quinoa
!***********************************
      read (46,*) para
      read (46,*,err = 250) t%P_codepluiepoquet
      read (46,*) para
      read (46,*,err = 250) t%P_nbjoursrrversirrig
! DR 11/04/2011 le P_dlaimin passe de paramv6 dans les fichiers plantes (0.1 prairie, 0.0 pour les autres)
!      read (46,*) para
!      read (46,*,err = 250) t%P_codedlaimin
!      read (46,*) para
!      read (46,*,err = 250) t%P_dlaimin(1)
!      read (46,*) para
!      read (46,*,err = 250) t%P_dlaimin(2)
!dynamique des talles
!***********************
      read (46,*) para
      read (46,*,err = 250) t%P_swfacmin
      read (46,*) para
      read (46,*,err = 250) t%P_codetranspitalle
      read (46,*) para
      read (46,*,err = 250) t%P_codedyntalle(1)
      read (46,*) para
      read (46,*,err = 250) t%P_SurfApex(1)
      read (46,*) para
      read (46,*,err = 250) t%P_SeuilMorTalle(1)
      read (46,*) para
      read (46,*,err = 250) t%P_SigmaDisTalle(1)
      read (46,*) para
      read (46,*,err = 250) t%P_VitReconsPeupl(1)
      read (46,*) para
      read (46,*,err = 250) t%P_SeuilReconsPeupl(1)
      read (46,*) para
      read (46,*,err = 250) t%P_MaxTalle(1)
! dr 10/06/2010 nouevaux parametres
      read (46,*) para
      read (46,*,err = 250) t%P_SeuilLAIapex(1)
      read (46,*) para
      read (46,*,err = 250) t%P_tigefeuilcoupe(1)
! DR et ML et SYL 15/06/09
! ************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
! ### SYL 19-02-2009
      read (46,*) para
      read (46,*,err = 250) t%P_codedyntalle(2)
      read (46,*) para
      read (46,*,err = 250) t%P_SurfApex(2)
      read (46,*) para
      read (46,*,err = 250) t%P_SeuilMorTalle(2)
      read (46,*) para
      read (46,*,err = 250) t%P_SigmaDisTalle(2)
      read (46,*) para
      read (46,*,err = 250) t%P_VitReconsPeupl(2)
      read (46,*) para
      read (46,*,err = 250) t%P_SeuilReconsPeupl(2)
      read (46,*) para
      read (46,*,err = 250) t%P_MaxTalle(2)
 ! dr 10/06/2010 nouevaux parametres
      read (46,*) para
      read (46,*,err = 250) t%P_SeuilLAIapex(2)
      read (46,*) para
      read (46,*,err = 250) t%P_tigefeuilcoupe(2)
! ###
! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain
! DR et ML et SYL 15/06/09
! ************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
! #### SYL 26/02/2009

!Déplafonnement des réserves pour le cycle reproducteur
!*******************************************************
! DR 10/06/2010 on indexe sur le nb de plantes
      read (46,*) para
      read (46,*,err = 250) t%P_resplmax(1)
      read (46,*) para
      read (46,*,err = 250) t%P_resplmax(2)
!calcul du stade de debut montaison pour les prairies perennes
!**************************************************************
! DR 10/06/2010 nouveaux parametres de 71
! DR et ML et SYL 16/06/09
      read (46,*) para
      read (46,*,err = 250) t%P_codemontaison(1)
      read (46,*) para
      read (46,*,err = 250) t%P_codemontaison(2)
! ####
! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain
!Prise en compte du CC sur les matieres organiques
!****************************************************
      read (46,*) para
      read (46,*,err = 250) t%P_code_adapt_MO_CC
      read (46,*) para
      read (46,*,err = 250) t%P_periode_adapt_CC
      read (46,*) para
      read (46,*,err = 250) t%P_an_debut_serie_histo
      read (46,*) para
      read (46,*,err = 250) t%P_an_fin_serie_histo
      read (46,*) para
      read (46,*,err = 250) t%P_param_tmoy_histo
      read (46,*) para
      read (46,*,err = 250) t%P_code_adaptCC_miner
      read (46,*) para
      read (46,*,err = 250) t%P_code_adaptCC_nit
      read (46,*) para
      read (46,*,err = 250) t%P_code_adaptCC_denit
      read (46,*) para
      read (46,*,err = 250) t%P_TREFdenit1
      read (46,*) para
      read (46,*,err = 250) t%P_TREFdenit2
!test humidite dans decision semis
!********************************************
      read (46,*) para
      read (46,*,err = 250) t%P_nbj_pr_apres_semis
      read (46,*) para
      read (46,*,err = 250) t%P_eau_mini_decisemis
      read (46,*) para
      read (46,*,err = 250) t%P_humirac_decisemis
!pilotage des fertilisations
!**********************************
! DR 05/04/2011 on les passe dans paramv6
      read (46,*)
      read (46,*,err = 250) t%P_codecalferti
      read (46,*)
      read (46,*,err = 250) t%P_ratiolN
      read (46,*)
      read (46,*,err = 250) t%P_dosimxN
      read (46,*)
      read (46,*,err = 250) t%P_codetesthumN      ! => Stics_Communs ?? A classer ou dupliquer

      ! Bruno : les 3 nouveaux param

      read (46,*)
      read (46,*,err = 250) t%P_codeNmindec
      read (46,*)
      read (46,*,err = 250) t%P_rapNmindec
      read (46,*)
      read (46,*,err = 250) t%P_fNmindecmin

! ML 17/10/2013 on ajoute le code de couplage avce le module pathogene
! DR 07/02/2014 j'enleve la possibilité d'appel à Mila qui fait l'objet d'une version branch Mila
! je laisse les codes de calcul de durée d'humectation qui peut servir à tous
!      read (46,*)
!      read (46,*,err = 250) t%P_codepatho
! ML 29102012 on ajoute les 2 codes de calcul de la temp de rosee et de la duree d'humectation
      read (46,*)
      read (46,*,err = 250) t%P_codetrosee
      read (46,*)
      read (46,*,err = 250) t%P_codeSWDRH
!DR 10/02/2015 j'ajoute une option pour les tours d'eau en cas d'irrigation automaitique
      read (46,*)
      read (46,*,err = 250) t%P_codedate_irrigauto
      read (46,*)
      read (46,*,err = 250) t%P_datedeb_irrigauto
      read (46,*)
      read (46,*,err = 250) t%P_datefin_irrigauto
! DR 06/05/2015 je rajoute un code pouyr tester la mortalitéé des racines
      read (46,*)
      read (46,*,err = 250) t%P_codemortalracine
! DR 05/02/2016 pour regles de semis agmipWheat
      read (46,*)
      read (46,*,err = 250) t%P_type_project
      read (46,*)
      read (46,*,err = 250) t%P_rules_sowing_AgMIP
      read (46,*)
      read (46,*,err = 250) t%P_Flag_Agmip_rap

      ! DR 23/03/2014 pour Constance et pour la preochaine version on peut faire plusieurs eclaircissages
      ! DR 23/03/2016 pour le rendre generique je le sort dans param_newform
      read (46,*)
      read (46,*,err = 250) t%P_option_thinning
! DR 30/03/2016 je met à part la possiblite d'apporter plusieurs types deengrais , c'est au dela de la notion de paturage
      read (46,*)
      read (46,*,err = 250) t%P_option_engrais_multiple
      ! DR 29/03/2016 option pour activer la gestion en paturage
      read (46,*)
      read (46,*,err = 250) t%P_option_pature
! DR 07/04/2016 les parametres de l'option pature
      read (46,*)
      read (46,*,err = 250) t%P_coderes_pature
      read (46,*)
      read (46,*,err = 250) t%P_pertes_restit_ext
      read (46,*)
      read (46,*,err = 250) t%P_Crespc_pature
      read (46,*)
      read (46,*,err = 250) t%P_Nminres_pature
      read (46,*)
      read (46,*,err = 250) t%P_eaures_pature
      read (46,*)
      read (46,*,err = 250) t%P_coef_calcul_qres
      read (46,*)
      read (46,*,err = 250) t%P_engrais_pature
      read (46,*)
      read (46,*,err = 250) t%P_coef_calcul_doseN






      call EnvoyerMsgHistorique('codeNmindec',t%P_codeNmindec)
!     call EnvoyerMsgHistorique('fredNsup',t%P_fredNsup)
      call EnvoyerMsgHistorique('rapNmindec',t%P_rapNmindec)
      call EnvoyerMsgHistorique('fNmindecmin ',t%P_fNmindecmin )
!      call EnvoyerMsgHistorique('Primingmax',t%P_Primingmax)

!dr et ml 17/10/2013 forcage de codeSWDRH si couplage avce Mila
! dr 07/02/2014 j'enleve aussi le forcage devevnu inutile
!      if (t%P_codepatho .eq.1)then
!        call EnvoyerMsgHistorique('(/)')
!        call EnvoyerMsgHistorique(408)
!        t%P_codeSWDRH=1
!      endif


    ! domi - 27/01/04 - je rajoute un test sur P_ratiol<0
      if (t%P_ratiolN < 0.0)then
        call EnvoyerMsgHistorique('(/)')
        call EnvoyerMsgHistorique(395)
        t%P_ratiolN=1.0
      endif

    ! PB - 16/03/2004
    ! en mode de simulation CAS on ne peut pas calculer automatiquement les fertilisations
      if (P_nbplantes > 1) t%P_codecalferti = 2

! dr 05/04/2011 sont passé dans paramv6
      if (t%P_codecalferti == 1) then
        call EnvoyerMsgHistorique('P_ratiolN ',t%P_ratiolN)
        call EnvoyerMsgHistorique('P_codetesthumN ',t%P_codetesthumN)
      endif


      ! DR 22/06/2015 j'ajoute un test sur les dates de tour d'eau
      if (t%P_codedate_irrigauto == 1)then
        if(t%P_datedeb_irrigauto > t%P_datefin_irrigauto) then
        call EnvoyerMsgHistorique(408)
        endif
      endif

      close(46)
      return


250   continue
      call EnvoyerMsgHistorique(5005)
      call EnvoyerMsgHistorique(405)
      !stop
      call exit(9)

return
end subroutine Stics_Lecture_Transit
 
 
