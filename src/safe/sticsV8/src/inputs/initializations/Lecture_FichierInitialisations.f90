!> subroutine of reading initialization parameters/
!> - reading of the initializations parameters in the file ficini.txt
subroutine lecinitialisations(sc,p,soil,ficini)

USE Stics
USE Plante
USE Sol
!!!MODIF HISAFE 8 : suppression de l'objet USM
!!!USE USM
USE Messages

implicit none

    type(Stics_Communs_), intent(INOUT) :: sc  
    type(Plante_),        intent(INOUT) :: p(sc%P_nbplantes)
    type(Sol_),           intent(INOUT) :: soil  
!!!    type(USM_),                 intent(INOUT) :: usma  !> // PARAMETER // name of the P_USM // SD // USMXML // 0

! Variables locales
    integer :: nbp  !>  
    integer :: i  !>  
    integer :: k  
    character(len=50)ficini

    character(len=255) :: path
    character(len=255) :: pathinit

!19/11/2013 pour Record
    ! enabling_RECORD : to get the full path
    integer ib0                                                   ! enabling_RECORD
    integer ib1                                                   ! enabling_RECORD
    character(len=300) :: filepluspath                            ! enabling_RECORD
    ib0 = len_trim(pathinit)                                   ! enabling_record
    if (ib0 .ne. 0 ) then                                         ! enabling_record
       filepluspath =  pathinit                                ! enabling_record
    else
       ib1 = len_trim(path)                                       ! enabling_RECORD
       if (ib1 .eq. 0 ) then                                         ! enabling_RECORD
          filepluspath = ficini                                ! enabling_RECORD
       else                                                          ! enabling_RECORD
          filepluspath = path(1:ib1) // '/' // ficini       ! enabling_RECORD
       endif                                                         ! enabling_RECORD
    endif
!fin record

    ! on ouvre le fichier
!write(*,*) 'On ouvre le fichier'
     ! DR 17/07/2012 on lit le nom des fcihiers pour optimistics
     ! open(36,file = 'ficini.txt',status = 'old')
      !open(36,file = ficini,status = 'old')
      open(36,file = filepluspath,status = 'old',err = 250) ! enabling_record

!write(*,*) 'On lit le nombre de plantes'
      read (36,*,err = 250)
      read (36,*,err = 250) nbp
!write(*,*) 'Nombre de plantes lu :', nbp,' et nombre de plantes supposé :',sc%P_nbplantes

      if (nbp /= sc%P_nbplantes) then
!        if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *, 'erreur, nb de plantes ',nbp,' /= ', sc%P_nbplantes
         call EnvoyerMsgHistorique(5162, sc%P_nbplantes)
        !stop
        call exit(9)
      endif

      do i = 1, sc%P_nbplantes

        read (36,*,err = 250)
        read (36,*,err = 250) p(i)%P_stade0
        read (36,*,err = 250) p(i)%P_lai0
        read (36,*,err = 250) p(i)%P_masec0
        read (36,*,err = 250) p(i)%P_QNplante0
        read (36,*,err = 250) p(i)%P_magrain0
        read (36,*,err = 250) p(i)%P_zrac0
        read (36,*,err = 250) p(i)%P_resperenne0
        read (36,*)
        read (36,*,err = 250) (p(i)%P_densinitial(k),k = 1,5)

      end do

      if (sc%P_nbplantes == 1) then
        do k = 1,10
          read (36,*)
        enddo
      endif

      read (36,*)
      !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
      !!!read (36,*,err = 250) (sc%P_Hinitf(k),k = 1,5)
      read (36,*,err = 250) (soil%P_Hinitf(k),k = 1,5)
      read (36,*)
      read (36,*,err = 250) (soil%P_NO3initf(k),k = 1,5)
      read (36,*)
      !!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
      !!!read (36,*,err = 250) (sc%P_NH4initf(k),k = 1,5)
      read (36,*,err = 250) (soil%P_NH4initf(k),k = 1,5)

      return

250   continue
      !write(*,*) 'erreur dans la lecture du fichier initilisations'
      call EnvoyerMsgHistorique(4001)

return
end
 
 
