! **********************************************
! * routine pour initialiser les variables     *
! * en début de simulation. (mises à zéro,...) *
! appelé uniquement si numcult > 1 (plus d'une année de culture)
!      et si P_codeperenne /= 2 (c'est une annuelle) ou P_codeinitprec /= 2 (reinitialisation)
!
! **********************************************

subroutine initsimul(sc,pg,p,itk,t)

USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(INOUT) :: pg ! TODO : pg en IN, P_codemsfinal à dupliquer  

  type(Plante_),              intent(INOUT) :: p  

  type(ITK_),                 intent(INOUT) :: itk  

  type(Stics_Transit_),       intent(INOUT) :: t  

    ! PB - 21/01/05 - on force P_codemsfinal à 2 qd on enchaine une pérenne ! TODO : on modifie un paramètre général !!!
      if (p%P_codeperenne == 2 .and. pg%P_codeinitprec == 2 .and. sc%nbans > 1) pg%P_codemsfinal = 2

        p%caljvc       = 0.
        p%nsencour     = 0
        p%nsencourpre  = 0

        p%somelong     = 0.
        p%somcour      = 0.
        p%somcourdrp   = 0.
        p%somtemp      = 0.
        p%ndebsen      = 0

      ! DR et ML et SYL 16/06/09
      ! introduction de la montaison pour les prairies
      ! ####
      ! NB le 07/03/08
        p%somcourmont=0.
      ! ####

        p%nplt         = itk%P_iplt0 - sc%P_iwater + 1
      ! NB le 13/05/05 initialisation de la pluie pour la battance
      !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Plante.f90)
      !!!t%pluiesemis   = 0.0
        p%pluiesemis   = 0.0
      ! NB le 10/09/05 car pb enchainement années pour la vigne avec réinitialisation
      !!!MODIF HISAFE 1 : suppression des chaines de caractères
      !!!  if (p%P_stade0 /= 'dor') p%nger = 0
        if (p%P_stade0 /= 3) p%nger = 0
        p%nlev         = 0
        p%namf         = 0
        p%nlax         = 0
        p%ndrp         = 0
        p%nsen         = 0
        p%nlan         = 0
        p%nmat         = 0
        p%nrec         = 0
        p%nnou         = 0
        p%nflo         = 0
        p%nstopfeuille = 0
        p%ndebdes      = 0
        p%nrecalpfmax  = 0

        p%tursla(:)    = 1.

        p%mafeuiljaune(:) = 0.

        p%matigestruc(:)  = 0.
        p%msresjaune(:)   = 0.
        p%msjaune(:)   = 0.
        p%pdsfruittot(:)  = 0.
        p%mareserve(:)    = 0.

        p%msres(:) = itk%P_msresiduel(0)

        p%varrapforme = p%P_rapforme

        p%durvie(:,:) = 0.0
        p%dltams(:,:) = 0.0
        p%pfeuilverte(:,:) = 0.

      ! ajout de variables NB le 11/04/05
 !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Plante.f90)
 !!!       t%humectation = .FALSE.
 !!!       t%nbjhumec = 0
        p%humectation = .FALSE.
        p%nbjhumec = 0

      ! DR et ML et SYL 16/06/09
      ! introduction de la montaison pour les prairies
      ! ####
      ! SYL 11/03/08
        if(t%P_codemontaison(p%ipl) == 1)then
          p%onestan2 = 1
        endif
      ! ####

return
end subroutine initsimul
 
 
