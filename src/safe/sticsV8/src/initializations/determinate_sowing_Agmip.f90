subroutine determinate_sowing_Agmip(c,P_usm,ansemis,iwater,nplt,iplt,anit)
!c,sc%P_usm,sc%ansemis,sc%P_iwater,p(1)%P_iplt0,p(1)%nplt,sc%iplt(1)

! 26/01/2016 subroutine de calcul des regles de semis selon agmip wheat
! lecture des precinisation : de fenetre de semis , date de debut

USE Climat
  implicit none
  type(Climat_),              intent(INOUT) :: c

  character*20 , intent(IN) :: P_usm
  integer, intent(INOUT) :: ansemis

  integer, intent(INOUT) :: nplt
  integer, intent(INOUT) :: iplt
  integer, intent(INOUT) :: iwater
  real, intent(OUT) :: anit

integer i,date_ini,date_fin,jj,nbjansemis
integer nplt_deb,j_deb, m_deb,j_fin,m_fin,nplt_fin
integer nplt0,k_win,k
character*2 num_site
character*25 nom_site,pays
character*50 entete
real cum_rr,ferti_semis
integer  nbjan

open(333,file='sowing_windows.txt')
read(333,*)entete
do i=1,31
   read(333,*,end=100)num_site, nom_site, pays, j_deb, m_deb,j_fin,m_fin, ferti_semis
   write(*,*)num_site,nom_site
   jj=index(P_usm,trim(nom_site))
   if(jj.ne.0)goto 10
enddo
100 write(*,*)'site non trouve'
10 read(333,*)
close(333)
! pour le moment on ne lit que ca , je n'ai pas besin des infos annuelles ???
!on calcule le jour de deb dans le calendrier stics
  call NDATE (j_deb,m_deb,ansemis,nplt_deb)
  call NDATE (j_fin,m_fin,ansemis,nplt_fin)
  nbjansemis=nbjan(ansemis)
  if(nplt_fin.lt.nplt_deb)nplt_fin=nplt_fin+nbjansemis
! on doit faire un cumul glissant sur les 5 jours precedant le semis potentiel
do k_win=nplt_deb,nplt_fin
cum_rr=0.
do k=k_win-5,k_win-1
     cum_rr=cum_rr+c%trr(k)
     write(1502,*)P_usm,ansemis,'deb', j_deb,m_deb,'fin',j_fin,m_fin,'deb cumul',k_win,k,cum_rr
enddo
! on a trouve la date dans la windows, on seme à cette date
if(cum_rr.ge.10)then
    iplt=k_win
    goto 200
endif
enddo
! on a pas trouve de date dans la windows, on seme à la date de fin
iplt=k_win-1

200 continue

!  sc%P_iwater est a recalculer , 10 jours avant le smeis preconisé
!DR 05/02/2015 pour le moment Giacomo precnise de demarrer 10 jours avant la premiere date de semis de la fenetre
!iwater=iplt-10
nplt=iplt-iwater+1
anit=ferti_semis
write(1502,*)'semis realise',iplt,nplt,cum_rr

return
end
