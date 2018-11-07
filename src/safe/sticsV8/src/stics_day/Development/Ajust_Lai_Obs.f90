! ***
! *- Recalcul des dates de levée et LAX en fonction de la courbe de LAI observée
! *-

subroutine recalcullevee(n,nlev,nlevobs,nlaxobs,lai,tauxcouv,P_codelaitr,estDominante, nsen,nlan)

implicit none

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nlev  
  integer, intent(OUT)   :: nlevobs  
  integer, intent(OUT)   :: nlaxobs  
  real,    intent(INOUT) :: lai(2,n+1)   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(OUT)   :: tauxcouv(n+1)   !> // OUTPUT // Cover rate // SD
  integer, intent(IN)    :: P_codelaitr  !> // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0 
  logical, intent(IN)    :: estDominante  

!!!AJOUT HISAFE
integer, intent(OUT)   :: nsen
integer, intent(OUT)   :: nlan

  integer :: AS=1  !>  
  integer ::  AO=2  

! le stade LEV est observé (dans le fichier technique) et par conséquent il est imposé dans Stics_LAI_devloppement
! cependant le LAI forcé (on est dans le cas P_codesimul='feuille') est nul; on impose donc la date de levée observée (lue dans le fichier technique)
! ML le 29/05/09
 
 !!!MODIF HISAFE 12 : Modif après détection bug
 !!!MODIF HISAFE
 !!!J'enlève cela car sinon le demande en eau reste > 0 après que la lai soit = 0
    if (nlev > 0) then
 !!!     if (lai(AS,n) <= 0. .and. lai(AS,n-1) <= 0.)then
 !!!       lai(AS,n) = 0.001
 !!!     endif

!!!MODIF HISAFE 12 : Modif après détection bug
!!!MODIF HISAFE on change le TEST qui ne marche pas sinon
!!!    if (.not.estDominante) then
    if (estDominante) then
    else
  !!!      if (lai(AO,n) <= 0. .and. lai(AO,n-1) <= 0.)then
  !!!        lai(AO,n) = 0.001
  !!!      endif
      endif

    else

! le LAI forcé est non nul à cette date, et pourtant la levée n'a pas encore eu lieu 
! (elle n'est pas observée, elle n'est pas calculée par Stics_Levee): on impose donc la date de levée correspondant au jour du 1er LAI >0
! du fichier des LAI journaliers forcés
! ML le 29/05/09
    
      if (lai(AS,n) > 0. .or. lai(AO,n) > 0.) nlevobs = n
    endif

! le LAI forcé (du fichier des LAI journaliers forcés) est maximal à cette date: on impose donc cette date comme la date du stade LAX
! ML le 29/05/09

    if (lai(AO,n) > lai(AO,n-1) .and. lai(AO,n) > lai(AO,n+1)) nlaxobs = n

    if (lai(AS,n) > lai(AS,n-1) .and. lai(AS,n) > lai(AS,n+1)) then
        nlaxobs = n
    endif

! si le LAI a été modifié, et qu'on est en taux de couverture, il faut réaffecter la valeur à la variable tauxcouv
! ML le 29/05/09

    if (P_codelaitr == 2) tauxcouv(n) = lai(AS,n)

!!!AJOUT HISAFE
  if (nlev > 0) then
      if (lai(AS,n) <= 0 .and. nlan == 0 .and. nlaxobs > 0) then
          if (nsen == 0) then
            nsen = n
          endif
          nlan = n
      endif
  endif

return
end
