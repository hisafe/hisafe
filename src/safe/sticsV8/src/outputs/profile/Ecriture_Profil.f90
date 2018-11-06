! ************************************
! *  ecriture du fichier profil.sti  *
! ************************************
!> File writing profil.sti
subroutine Ecriture_Profil(sc,profsol)

USE Stics


    implicit none

    type(Stics_Communs_), intent(INOUT) :: sc  
    integer,intent(IN)           :: profsol
    integer :: nbjan


! les VARIABLES LOCALES
    integer :: j  !>  
    integer :: k  
    integer :: jour,nummois,jjul,an1,an2
    character*3 :: mois


    ! On ouvre le fichier
    ! DR 15/02/08 le status en append est à utiliser uniquement en cas de cultures associées
 !!!   if ( sc%P_nbplantes > 1) then
!!!      open(10,file='mod_profil.sti',position='append')
 !!!   else
      open(10,file='mod_profil.sti',status='unknown')
 !!!   endif

    sc%numdateprof(sc%ipl) = sc%numdateprof(sc%ipl)-1

    !!!MODIF HISAFE 1 : suppression des chaines de caractères
    !!!write(10,*) sc%valprof

    ! sinon ça plante.
    if (sc%numdebprof(sc%ipl) == 0 .or. sc%numdateprof(sc%ipl) <= 0) return

! DR 01/04/2015 pas de poissson ...
         ! on transpose n en jour julien depuis le 1er janvier de l'année de début de simulation
          ! sc%jjul = tCal1JAbs(sc%n,sc%P_iwater)
         ! à partir de jjul, on calcule le jour julien relatif à l'année en cours de simulation
         ! TODO : vérifier que les nouvelles dates calculées correspondent aux valeurs attendues.
         !  sc%jul = tCal1JRel(sc%jjul,sc%annee(sc%P_iwater),.false.)
         !  sc%numdate = sc%jul
           do k=sc%numdebprof(sc%ipl),sc%numdateprof(sc%ipl)
              ! jjul = tCal1JAbs(sc%dateprof(sc%ipl,k),sc%P_iwater)
               jjul = sc%dateprof(sc%ipl,k)
               ! dr 02/04/2015 verifier l'annee
               an1=sc%annee(sc%P_iwater)
               an2=sc%annee(sc%P_ifwater)
               if(sc%dateprof(sc%ipl,k).gt.nbjan(an1))jjul=jjul-nbjan(an1)
               call julien(jjul,sc%annee(sc%dateprof(sc%ipl,k)),mois,jour,nummois)
!               write(10,*) sc%dateprof(sc%ipl,k),  sc%annee(sc%dateprof(sc%ipl,k)),nummois,jour
           enddo


    write(10,210)(sc%dateprof(sc%ipl,j),j=sc%numdebprof(sc%ipl),sc%numdateprof(sc%ipl))

210 format(' cm ',600(3x,i5))

!!!MODIF HISAFE 5 : suppression variable inutile
!!!On supprime un gros tableaux qui ne nous sert pas
!!!    do i=1,profsol
!!!      write(10,100)i,(sc%tabprof(sc%ipl,k,i),k=sc%numdebprof(sc%ipl),sc%numdateprof(sc%ipl))
!!!    end do

!!!100 format(i4,600f11.5)

    ! on ferme le fichier
    close(10)

return
end subroutine Ecriture_Profil
 
 
