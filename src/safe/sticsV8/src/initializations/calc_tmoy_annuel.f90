!> sous-programme de calcul de la temperature moyenne annuelle pour la prise
!! en compte du CC sur la matiere organique

subroutine calc_tmoy_annuel(tmoy_an)

USE Messages
USE Divers, only: nbjParAnnee

implicit none

!: Arguments
    real, intent(INOUT) :: tmoy_an(2,300) ! TODO : taille à mettre en dynamique ?  

!: Variables locales

    character(len=7) :: station  
    integer          :: an  !>  
    integer          :: imois  !>  
    integer          :: ijour  !>  
    integer          :: jul  !>  
    integer          :: i  !>  
    integer          :: andeb  !>  
    integer          :: k  !>  
    integer          :: l  
    real             ::tn  !>  
    real             :: tx  !>  
    real             ::  tm  


    open(142, file='climat.txt')
    read(142,*) station, andeb
    backspace(142)

! calcul de la temperature moyenne historique sur la periode lue dans le paramv6.par
!  P_an_debut_serie_histo-P_an_fin_serie_histo

    l = 0
    do i = andeb,2100
      l = l+1
      tm = 0.
      do k = 1,nbjParAnnee(i)
        read(142,*,err=250,end=80) station,an,imois,ijour,jul,tn,tx
        tm = tm + ((tn+tx) / 2.)
      enddo
      tmoy_an(1,l)= i
      tmoy_an(2,l)= tm / nbjParAnnee(i)
    enddo

80  continue
    close(142)
    return

250 call EnvoyerMsgHistorique(139)
    close(142)

return
end
 
 
