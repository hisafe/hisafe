! sous programme de traitement particulier de la quinoa en poquet
! ** DR 160107 Pour la quinoa en poquet on a un traitement particulier 
! si P_codepluiepoquet = 1 on transforme les pluies en irrigations en profondeur  
! pendant P_nbjoursrrversirrig apres la levee

subroutine affectrrquinoapoquet(n,nplt,nlev,P_nbjoursrrversirrig,airg,trr)

  implicit none
  
!: Arguments

  integer, intent(IN)    :: n  
  integer, intent(IN)    :: nplt      
  integer, intent(IN)    :: nlev  
  integer, intent(IN)    :: P_nbjoursrrversirrig  !> // PARAMETER // number of days during which rainfall is replaced by irrigation in the soil after a sowing poquet // jours // PARAMV6 // 1 
  
  real,    intent(INOUT) :: airg      !> // OUTPUT // Daily irrigation // mm
  real,    intent(INOUT) :: trr      !> // OUTPUT // Rainfall  // mm.day-1
  
       
      if (nplt /= -999 .and. nplt /= 0 .and. n >= nplt) then
        if (nlev == 0 .or. n < (nlev + P_nbjoursrrversirrig)) then
          airg = airg + trr
          trr  = 0.
        endif
      endif
      
      
return
end subroutine affectrrquinoapoquet 
 
