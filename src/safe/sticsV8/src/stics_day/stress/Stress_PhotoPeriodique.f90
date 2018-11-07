!  ********************************
!  calcul du stress photoperiodique jouant sur la durée de vie des feuilles 
!   dans le cas de la vigne les feuilles jaunissent toutes à la meme periode
!   j'ai introduitr 3 parametres dans paramv6.par concernat ce calcul
!   P_dltamsminsen = 
!   P_dltamsmaxsen = 
!   P_phobasesen = 
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!>
!! This module calculates the photoperiodical stress acting on leaf lifespan.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine stressphot(dltams, P_dltamsminsen, P_dltamsmaxsen, P_alphaphot, & ! IN
                      strphot, strphotveille)                          ! (IN)OUT

  implicit none
  
!: Arguments

  real, intent(IN)    :: dltams   !> // OUTPUT // Growth rate of the plant  // t ha-1.j-1
  real, intent(IN)    :: P_dltamsminsen  !> // PARAMETER // threshold value of deltams from which the photoperiodic effect on senescence is maximal // t ha-1j-1 // PARAM // 1 
  real, intent(IN)    :: P_dltamsmaxsen  !> // PARAMETER // threshold value of deltams from which there is no more photoperiodic effect on senescence // t ha-1j-1 // PARPLT // 1 
  real, intent(IN)    :: P_alphaphot  !> // PARAMETER // parameter of photoperiodic effect on leaf lifespan // P_Q10 // PARPLT // 1
  real, intent(OUT)   :: strphot  
  real, intent(INOUT) :: strphotveille  


!  maintenant calcul par une exponentielle
! nB le 26/06/06
! on remplace le test sur dltams par un test sur stressphot

!  if (dltams(ipl,ens,n) <= P_dltamsminsen) then
!    strphot(ipl,ens) = 0.
!  else

      if (dltams >= P_dltamsmaxsen) then
        strphot = 1.
      else
        strphot = exp(dltams - P_dltamsmaxsen)**P_alphaphot
      endif

      if (strphot <= P_dltamsminsen) strphot = 0.

!  endif

      !: DR 17/06/05 c'est plus bon 
      !--strphot(ipl,ens) = (exp(dltams(ipl,ens,n))-exp(P_dltamsminsen))**P_alphaphot
      
      strphot = min(strphot, strphotveille)
      strphotveille = strphot

return
end subroutine stressphot 
 
