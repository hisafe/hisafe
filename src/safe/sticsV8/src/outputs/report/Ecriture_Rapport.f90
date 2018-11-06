! ***************************************************** c
! * modif Bruno  20/04/99                              * c
! * écriture d'un rapport final en fin de simulation  * c
! * + éventuellement à 2 autres dates                 * c
! ***************************************************** c
!> Writing a final report at the end of simulation (file mod_rapport.sti)
!subroutine Ecriture_Rapport(sc,pg,soil,c,sta,p,itk,t) ! DR 19/07/2012 on supprime itk qui ne sert pas
subroutine Ecriture_Rapport(sc,pg,soil,p,t)

USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux
USE Sol

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Sol_),                 intent(INOUT) :: soil  

  type(Plante_),              intent(INOUT) :: p  

  type(Stics_Transit_),       intent(INOUT) :: t  

    ! variables locales pour l'optimisation
    integer,parameter :: nb_parameters_max = 300
    integer           :: nbpar  !>
    real    :: valparopt(nb_parameters_max)
    type(varying_string) :: nompar (nb_parameters_max)


! Variables locales

      integer           :: ancours  !>  
      integer           :: ifin  !>  
      integer           :: k  !>
      real              :: nstt1  !>  
      real              :: QNF  !>  
      real              :: rdt  
      ! PB - je le rajoute en tant que variable locale
      real              :: nstt2  

!      real              :: valparopt(5)

      character         :: sep  
      character(len=10) :: commentcelia  
      integer           :: kk

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!Donc on les déclare ici en local
character(len=50) :: codeversion
character(len=50) :: P_wdata1
character(len=50) :: P_usm
character(len=50) :: wlieu



return
end subroutine Ecriture_Rapport
 
 
