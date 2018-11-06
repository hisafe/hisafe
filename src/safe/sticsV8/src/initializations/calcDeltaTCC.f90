subroutine calcDeltaTCC(sc,t)

USE Stics

implicit none

    type(Stics_Communs_),        intent(INOUT) :: sc  
    type(Stics_Transit_),        intent(INOUT) :: t  

!: variables locales
    integer :: jj  !>  
    integer :: kk  !>  
    integer :: deb  !>  
    integer :: fin  !>  
    integer :: num_an  
    real    :: tot_glisse  


! DR 28/11/07 on va calculer les temperatures moyennes glissantes sur P_periode_adapt_CC
!   write(94,*)'Tm_histo ',Tm_histo
!   write(95,*)'numan     an     tmoyannuelle   tmoyglissante  deltat'
!   write(95,*)'Tm_histo ',Tm_histo
!    do i = 1,200
!      sc%tm_glisse(i) = 0.0
!      sc%deltaT_adaptCC(i) = 0.0
!    enddo

    sc%tm_glisse(:) = 0.
    sc%deltaT_adaptCC(:) = 0.

    do jj = 1,sc%nbans
      deb = t%P_an_fin_serie_histo - t%P_an_debut_serie_histo + 1 + jj
      fin = deb + t%P_periode_adapt_CC - 1
      tot_glisse = 0.0
      do kk = deb,fin
        num_an = kk
        tot_glisse = tot_glisse + sc%tmoy_an(2,num_an)
      enddo
      sc%tm_glisse(num_an) = tot_glisse / (fin-deb+1)
      sc%deltaT_adaptCC(num_an+1) = sc%tm_glisse(num_an) - sc%Tm_histo

!   write(95,*)jj,tmoy_an(1,jj),tmoy_an(2,jj),tm_glisse(jj),deltaT_adaptCC(jj)
!   write(94,*)num_an,tmoy_an(1,num_an),tmoy_an(2,num_an),tm_glisse(num_an),deltaT_adaptCC(num_an+1)

    enddo

return
end subroutine calcDeltaTCC
 
 
