!*********************************************************************
!> reading of continous values of leaf area index
!> - in the case of forcing lai ( file *.lai )
!*********************************************************************
subroutine Lecture_Lai(sc,p,laifile)

    USE Stics
    USE Plante
    USE Messages, only: EnvoyerMsgHistorique

    implicit none

    type(Stics_Communs_),       intent(INOUT) :: sc  

    type(Plante_),              intent(INOUT) :: p  
    character(len=50),          intent(IN) :: laifile

    logical :: consec  
    integer :: nlai  !>  
    integer :: date(3),i,iderlai,ipremlai  
    integer :: j  !>  
    integer :: premier  
    character(len=3)  :: sufflai  

    !!!MODIF HISAFE 2 : reduction dimension temporelle
    !!!real :: tlai(731),nl(731),sbvmin
    real :: tlai(sc%nbjmax),sbvmin
    integer :: nl(sc%nbjmax)


! Initialisation.
    p%lai(:,:) = 0

! ================== debut =================================
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!    laifile = p%P_flai
!!!    open (12,file=laifile,status='old',err=999)

! ----------------------------------------------------------
! on extrait l'extension du fichier pour determiner si c'est
! un fichier .obs ou .lai
! ici on extrait les 3 dernieres lettres du nom du fichier lai

    i = LEN_TRIM(laifile)

    sufflai = laifile(i-2:i)

    if (sufflai == 'obs' .or. sufflai == 'OBS') then

  ! les lai observees   fichier (*.obs)
      premier = 0
      !!!MODIF HISAFE 2 : reduction dimension temporelle
      !!!do i = 1,731
      do i = 1,sc%nbjmax
        read(12,100,end=30) (date(j),j=1,3),nl(i),tlai(i)
100     format(i4,2i3,i4,f8.2)
        if (tlai(i) > 0.0 .and. premier == 0) then
          ipremlai = i
          premier = 1
        endif
        if (tlai(i) > 0.0 .and. premier == 1) iderlai = i

      end do
30    nlai = i-1

! lecture des LAI
! *****************

    ! test sur les valeurs manquantes
      consec = .true.
      do i = ipremlai,iderlai
        if (tlai(i) == 0) then
          consec = .false.
        endif
      end do

    ! le fichier lai a des donnees manquantes
      if (.not.consec) then
        call EnvoyerMsgHistorique(101)
        !stop
        call exit(9)
      endif

    ! changement de calendrier
      do i = sc%P_iwater,nlai
        sc%n = nl(i) - sc%P_iwater + 1
        p%lai(:,sc%n) = tlai(i)
!        write(*,*)n,lai(ipl,ens,n)
      end do

    else

    ! fichier des lai estime par ajust (*.lai)
    ! changement de format des LAI ajusté + 1ligne
      do i=1,8
        read(12,*)
      end do

      !!!MODIF HISAFE 2 : reduction dimension temporelle
      !!!do i = 1,731
      do i = 1,sc%nbjmax
        read(12,*,end=65) j,nl(j),tlai(j)
      end do

65    nlai = i - 1

    ! changement de calendrier
      do i = 1,nlai
        sc%n = nl(i) - sc%P_iwater + 1
        if (sc%n < 0) CYCLE
        p%lai(:,sc%n) = tlai(i)

      ! calcul de l'IF sénéscent
        if (p%lai(sc%AS,sc%n) < p%lai(sc%AS,sc%n-1)) then
          p%laisen(:,sc%n) = p%lai(sc%AS,sc%n-1) - p%lai(sc%AS,sc%n)
        else
          p%laisen(:,sc%n) = 0.0
        endif

        if (p%P_codeindetermin == 2) then
        ! NB - le 21/04 - recalcul d'un sbv approprié
          sbvmin = p%P_slamin / (1.0 + p%P_tigefeuil)
          !!!MODIF HISAFE 4 : suppression dimension temporelle
          !!!p%fpv(:,sc%n) = (p%lai(sc%AS,sc%n)-p%lai(sc%AS,sc%n-1)) * 1e4 / sbvmin
          !!!p%fpv(:,sc%n) = max(p%fpv(sc%AS,sc%n),0.0)
          p%fpv(:) = (p%lai(sc%AS,sc%n)-p%lai(sc%AS,sc%n-1)) * 1e4 / sbvmin
          p%fpv(:) = max(p%fpv(sc%AS),0.0)
        else
          !!!p%fpv(:,sc%n) = 0.0
          p%fpv(:) = 0.0
        endif
      end do

    endif

    close(12)

    sc%maxwth = sc%ifwater_courant - sc%P_iwater + 1

    return

!!!999 call EnvoyerMsgHistorique(4000,p%P_flai,'a50')
    !stop
!!!    call exit(9)

return
end subroutine Lecture_Lai
 
 
