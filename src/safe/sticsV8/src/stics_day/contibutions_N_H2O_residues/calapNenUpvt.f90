! ************************************************************* c
! * subroutine pour caro de calcul des dates d'apport en upvt * c
! ************************************************************* c

subroutine calapNenUpvt(n,nplt,P_upvttapN,P_codefracappN,P_doseN,P_fracN,P_Qtot_N,nlev,upvt,jul,  &
                        anit,numeroappN,napN,somupvt,P_julapN)

  implicit none

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nplt  
  ! quelle dimension pour P_upvttapN? numeroappN ? numeroappN+1 au cas où numeroappN soit égal à zéro ? pour l'instant 300, par défaut
  integer, intent(IN)    :: P_upvttapN(50)   !>  // PARAMETER // thermal time from emergence (UPVT units) driving fertilization // degree C // PARTEC // 1
  integer, intent(IN)    :: P_codefracappN  !> // PARAMETER // option of cut modes for forage crops: yes (1), no (2) // code 1/2 // PARTEC // 0 
  real,    intent(IN)    :: P_doseN(50)      ! // PARAMETER // fertilizer amount // kgN.jour-1 // PARTEC // 1
  real,    intent(IN)    :: P_fracN(50)      ! // PARAMETER // percentage of P_Qtot_N applied // % // PARTEC // 1
  integer, intent(IN)    :: P_Qtot_N  !> // PARAMETER // amount of mineral fertilizer applications  // kg N ha-1 // PARTEC // 1 

  integer, intent(IN)    :: nlev  
  real,    intent(IN)    :: upvt      !> // OUTPUT // Daily development unit  // degree.days
  integer, intent(IN)    :: jul  

  real,    intent(INOUT) :: anit      !> // OUTPUT // Daily nitrogen provided  // kgN.ha-1 j-1
  integer, intent(INOUT) :: numeroappN  
  integer, intent(INOUT) :: napN  

  real,    intent(INOUT) :: somupvt  
  integer, intent(INOUT) :: P_julapN(50)     ! pareil que pour P_upvttapN, quelle dimension ? 300 par défaut 	  // PARAMETER // dates in julian days of fertilizer application // julian day // PARTEC // 1


! ** domi - 29/08/03 - pour caroline lecture des dates de fertil en upvtt
! *- cette affectation n'a pas lieu d'etre dans ce cas
! *- anit est à recalculer plus loin
!  pour Anne-isabelle on impose une ferti au semis si la somme de temp est < ou egale à 0

      if (nplt == n) then
        if (P_upvttapN(1) <= 0) then
        ! DR 02/07/08 je rajoute le fractionnement de l'irrigation
          if (P_codefracappN == 1) then
            anit = P_doseN(numeroappN)
          else
            anit = P_fracN(numeroappN) / 100. * P_Qtot_N
          endif
          numeroappN = numeroappN + 1
          napN = numeroappN - 1
        endif
      else
        if (nlev > 0) then
          somupvt = somupvt + upvt
          if (P_upvttapN(numeroappN) /= 0 .and. somupvt >= P_upvttapN(numeroappN)) then
            P_julapN(numeroappN) = jul
          ! DR 02/07/08 je rajoute le fractionnement de l'irrigation
            if (P_codefracappN == 1) then
              anit = P_doseN(numeroappN)
            else
              anit = P_fracN(numeroappN) / 100. * P_Qtot_N
            endif
            numeroappN = numeroappN + 1
            napN = numeroappN - 1
          endif
        endif
      endif

return
end subroutine calapNenUpvt
 
 
