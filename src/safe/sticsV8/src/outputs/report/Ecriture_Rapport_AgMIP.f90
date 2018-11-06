! ***************************************************** c
! * modif Bruno  20/04/99                              * c
! * écriture d'un rapport final en fin de simulation  * c
! * + éventuellement à 2 autres dates                 * c
! ***************************************************** c
! ***************************************************** c
!> Writing a final report at the end of simulation for agmip project (file mod_rapport_AGMIP.sti)
!subroutine Ecriture_Rapport_agmip(sc,pg,soil,c,sta,p,itk,t)  !DR 19/07/2012 c, itk et t pas utile
subroutine Ecriture_Rapport_agmip(sc,pg,soil,sta,p,t)


USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux
USE Climat
USE Station
USE Sol

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Sol_),                 intent(INOUT) :: soil  

!  type(Climat_),              intent(INOUT) :: c

  type(Station_),             intent(INOUT) :: sta  

  type(Plante_),              intent(INOUT) :: p  

!  type(ITK_),                 intent(INOUT) :: itk

  type(Stics_Transit_),       intent(INOUT) :: t


      integer           :: ancours  !>  
      integer           :: ifin  !>  
      integer           :: i
      real              :: nstt1  !>  
      real              :: QNF  !>  
      real              :: rdt  
      ! PB - je le rajoute en tant que variable locale
      real              :: nstt2  

      character         :: sep  
      character(len=10) :: commentcelia  
      character(len=10) :: commentCC


     character(len=500) :: mes3000
    ! character(len=9)   :: treatment
     integer       :: j, nbligne_entete
     character*329 :: ligne
     integer       :: long_usm
     character*3   :: nom_plante
     logical :: AgMIP,Macsur
     real :: tcultmoy_cycle, tcultmax_cycle
     integer :: an_iplt,jour_iplt,nummois_iplt,an_iflo,jour_iflo,nummois_iflo,an_imat,jour_imat,nummois_imat
     integer :: an_ilev,jour_ilev,nummois_ilev
     integer :: jour
!     integer :: Flag_Agmpip_rap
     character*3 :: mois
     character*2 :: charmois_iplt,charjour_iplt
     character*2 :: charmois_ilev,charjour_ilev
     character*2 :: charmois_iflo,charjour_iflo
     character*2 :: charmois_imat,charjour_imat


     integer      ::  ii
     ! ,code_ecrit_nom_usm
     character*10 ::  scenario
     character*3  ::  co2
     character*30 ::  treatment


character(len=50) :: P_usm
character*20  :: info_level, model_name

     character*2 :: chiffre(31)
     chiffre = (/'01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19', &
                & '20','21','22','23','24','25','26','27','28','29','30','31'/)


return
end subroutine Ecriture_Rapport_agmip
 
 
