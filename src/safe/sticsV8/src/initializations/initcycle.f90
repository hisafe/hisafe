! ******************************************
! * Routine d'initialisation de variables  *
! * plante, en début de cycle.             *
! ******************************************

subroutine initcycle(sc,pg,p,itk,t)

USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux


  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Plante_),              intent(INOUT) :: p  

  type(ITK_),                 intent(INOUT) :: itk  

  type(Stics_Transit_),       intent(INOUT) :: t  

    ! changement d'origine si numcult > 1 et enchainement sur des perennes
      if (sc%numcult > 1) then
        if (p%P_codeperenne == 2 .and. pg%P_codeinitprec == 2) then
          p%dltams(:,0) = p%dltams(:,sc%n)
          p%durvie(:,0) = p%durvie(:,sc%n)
          p%lai(:,0) = p%lai(:,sc%n)
          p%masec(:,0) = p%masec(:,sc%n)
          p%pfeuilverte(:,0) = p%pfeuilverte(:,sc%n)

        ! pour les vernalisantes, quand on enchaine la plante est déjà levée. On met nlev = 1
          if (p%P_codebfroid == 2) p%nlev = 1


        ! PB - 09/03/2005 - si dormante, on remet somelong à zéro
        ! (il faudrait vérifier s'il ne faudrait pas le faire pour les autres plantes aussi)
        ! DR 18/08/06 la Paul c'est des betises d'apres IGC ! si on est en enchainement d'année
        ! et que la dormance est faite il ne faut reinitialise les actions chaud qu'on a recupere
        !  de l'annee d'avant (voir iniclim)
        !--if (P_codebfroid == 3) somelong = 0.

        else
        ! dr 12/04/06 test pour prairie
        ! rajout d'un test sur codeinstal pour que l'enchainement redemarre au stade de debut
        ! = nlev dans notre cas sinon on repart la deuxieme annee avec nlev  = 0
          if (p%codeinstal /= 1) call initsimul(sc,pg,p,itk,t)
        endif
      endif



      p%msrac(:)    = 0.

      !!!MODIF HISAFE 4 : suppression dimension temporelle
      !!!p%upvt(:)     = 0.
      p%upvt     = 0.

      p%upobs(:)    = 0.
      p%dtj(:)      = 0.

      p%masec(:,1:sc%nbjmax) = 0.

      if (sc%P_codesimul == 1) then
           p%lai(:,1:sc%nbjmax)   = 0.
      endif

    ! 070906DR et IGC on initialise les deltai
      p%deltai(:,:) = 0.
      p%magrain(:,1:sc%nbjmax)  = 0.
      p%laisen(:,:)   = 0.
      !--durvie(:,:)   = 0.
      !--dltams(:,:)   = 0.
      p%abso(:,:)     = 0.
      p%ircarb(:,:)   = 0.

    ! DR 13/01/06 on reinitialise dans le cas de maintient apres la recolte
      p%QNplante(:,1:sc%nbjmax) = 0.

    ! DR 13/01/06 on reinitialise dans le cas de maintient apres la recolte
      p%CNgrain(:) = 0.

    ! domi et inaki 05/01/06 initialisation de stressphotveille
      p%strphotveille(:) = 1.0

    ! 070906DR et IGC on initialise les deltai
      p%dltaisen(:) = 0.

    ! DR 29/12/06 il manquait l'initialisation
      sc%cumdltaremobilN = 0.

    ! dr 05/09/2011 je rajoute laimax
      p%laimax(:) = 0.0


return
end subroutine initcycle
 
 
