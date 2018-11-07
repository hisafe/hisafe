!======================================================================================!
!> calculation of the date format (year, month, day) from a day in the year
subroutine dates(j,jDeb,jAn,jt,anCours,mois,jour,numMois)

  USE Divers, only: tCal1JAbs, tCal1JRel

  implicit none

!: Arguments
  integer,          intent(IN)    :: j  
  integer,          intent(IN)    :: jDeb  
  integer,          intent(IN)    :: jAn  

  integer,          intent(INOUT) :: jt         ! jour transposé  
  integer,          intent(INOUT) :: anCours  
  character(len=3), intent(INOUT) :: mois  
  integer,          intent(INOUT) :: jour  
  integer,          intent(INOUT) :: nummois  


    jt = tCal1JAbs(j,jDeb) ! jour julien depuis le 1er janvier
    anCours = jAn

    jt = tCal1JRel(jt,anCours,.true.)

    call julien(jt,anCours,mois,jour,nummois)


return
end subroutine dates
 
 
