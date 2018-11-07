! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
!> water entering the soil
!> - Stics book paragraphe 6.2, page 99
!>
!! The quantity of water reaching the soil is attributable to rain or irrigation, after passage through vegetation and losses by surface runoff.
!!
!! Rain which penetrates into the soil is called PRECIP
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c

subroutine eauEntrantSysteme(sc,pg,t,c,p,itk)

USE Stics
USE Plante
USE Itineraire_Technique
USE Climat
USE Parametres_Generaux
USE Divers

  implicit none

!: Arguments

  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Plante_),              intent(INOUT) :: p (sc%P_nbplantes)

  type(ITK_),                 intent(INOUT) :: itk (sc%P_nbplantes)

  type(Climat_),              intent(INOUT) :: c  

  type(Stics_Transit_),       intent(INOUT) :: t  

!: Variables locales
  real :: preciptmp(2)  
  real :: stem  
  real :: precip1(2)  
  real :: precip2(2)  

  integer :: i ! indice plante  
  integer :: ens  
  integer :: n  !>  
  integer ::  AS  !>  
  integer ::  AO  
  real    :: preciptmp_plante1

      ! pour alléger l'écriture
      n = sc%n
      AS = sc%AS
      AO = sc%AO

!     write(618,*)'jour',n,'airg',sc%airg(n), 'precip', c%trr(n)

      !: Pour la quinoa en poquet traitement particulier des pluies à la levee
      !- attention à n'utiliser que pour une culture pure
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if (p(1)%P_codeplante == 'qui' .and. t%P_codepluiepoquet == 1) then
      if (p(1)%P_codeplante == 25 .and. t%P_codepluiepoquet == 1) then
         if (p(1)%nplt /= -999 .and. n >= p(1)%nplt .and. p(1)%nplt /= 0) then
           call affectrrquinoapoquet(n,p(1)%nplt,p(1)%nlev,t%P_nbjoursrrversirrig,sc%airg(n),c%trr(n))
         endif
      endif


      preciptmp(:) = 0.
      stem         = 0.
      precip1(:)   = 0.
      precip2(:)   = 0.

      do i = 1, sc%P_nbplantes

        p(i)%stemflow = 0.0
        do ens = AS,AO
          !: Si surf = 0 alors, pas besoin de calculer...
          if (p(i)%surf(ens) > 0.0) then

          ! DR 09/10/09 si on est en calcul des irrigations avec des sommes upvt
            if (itk(i)%P_codedateappH2O == 1 .and. p(i)%nplt > 0) then
              call calculApportsIrrigationEnUpvt(sc,p(i),itk(i))
            endif

            if (i > 1) then
              preciptmp(ens) = precip2(ens) / p(i)%surf(ens)
!              write(618,*)'precip2avant irrig',i,ens,precip2(ens),p(i)%surf(ens),preciptmp(ens)
!: ML - 29/10/12 - ajout de l'argument dureehumec dans call irrig

              call irrig(n,p(i)%nplt,p(i)%nrecbutoir,itk(i)%P_codecalirrig,pg%P_irrlev,     &
                         p(i)%swfac(ens),itk(i)%P_ratiol,p(i)%cumlracz,p(i)%zrac,         &
                         sc%hucc(1:int(p(i)%zrac)),sc%hur(1:int(p(i)%zrac)),            &
                         itk(i)%P_dosimx,itk(i)%P_doseirrigmin,p(i)%exofac,                 &
                         itk(i)%P_codlocirrig,itk(i)%P_effirr,p(i)%lai(ens,n),              &
                         p(i)%P_codeintercept,p(i)%P_stemflowmax,p(i)%P_kstemflow,            &
                         p(i)%parapluie,i,sc%P_nbplantes,p(i)%surfSous,p(i)%P_mouillabil,   &
!      itk(i)%P_locirrig,p(i)%P_codeplante,pg%P_codeSIG,                    & 23/07/2012 on utilise pas plocirrig
                         p(i)%P_codeplante,pg%P_flagEcriture,                   &
                         itk(i)%nap,sc%airg(n),preciptmp(ens),stem,precip1,             &
                         p(i)%mouill(ens),p(i)%interpluie(ens),                         &
                         p(i)%irrigprof(itk(i)%P_locirrig),sc%eaunoneffic,p(i)%totpl,   &
                         t%P_codeSWDRH,c%dureehumec,t%P_codedate_irrigauto ,t%P_datedeb_irrigauto,t%P_datefin_irrigauto)
!              write(618,*)'precip2 apres irrig',i,ens,precip2(ens),p(i)%surf(ens),preciptmp(ens)
            else

              preciptmp(ens) = c%trr(n)
!              write(618,*)'trr',i,ens,preciptmp(ens)

!#if DEBUG == 1
!              if (iand(sc%irrig,2) >0) call irrig_debug_write_input(1301,sc,pg,p,itk,c,t,i,ens,preciptmp(ens),stem,precip1)
!#endif

              call irrig(n,p(i)%nplt,p(i)%nrecbutoir,itk(i)%P_codecalirrig,pg%P_irrlev,     & ! IN
                         p(i)%swfac(ens),itk(i)%P_ratiol,p(i)%cumlracz,p(i)%zrac,         &
                         sc%hucc(1:int(p(i)%zrac)),sc%hur(1:int(p(i)%zrac)),            &
                         itk(i)%P_dosimx,itk(i)%P_doseirrigmin,p(i)%exofac,                 &
                         itk(i)%P_codlocirrig,itk(i)%P_effirr,p(i)%lai(ens,n),              &
                         p(i)%P_codeintercept,p(i)%P_stemflowmax,p(i)%P_kstemflow,            &
                         p(i)%parapluie,i,sc%P_nbplantes,p(i)%surfSous,p(i)%P_mouillabil,   &
!      itk(i)%P_locirrig,p(i)%P_codeplante,pg%P_codeSIG,                    & !23/07/2012 on utilise pas plocirrig
                         p(i)%P_codeplante,pg%P_flagEcriture,                    &
                         itk(i)%nap,sc%airg(n),preciptmp(ens),stem,precip1,             & ! INOUT
                         p(i)%mouill(ens),p(i)%interpluie(ens),                         &
                         p(i)%irrigprof(itk(i)%P_locirrig),sc%eaunoneffic,p(i)%totpl,   &
                         t%P_codeSWDRH,c%dureehumec,t%P_codedate_irrigauto ,sc%n_datedeb_irrigauto,sc%n_datefin_irrigauto)
!              write(618,*)'trr',i,ens,preciptmp(ens),precip1
              preciptmp_plante1=preciptmp(ens)
!#if DEBUG == 1
!              if (iand(sc%irrig,8) >0) call irrig_debug_write_output(1303,sc,pg,p,itk,c,t,i,ens,preciptmp(ens),stem,precip1)
!#endif
!       write(618,*)'preciptmp irrig 2',preciptmp(ens)

            endif

            p(i)%stemflow = p(i)%stemflow + (stem * p(i)%surf(ens))
            if (i > 1) then
              p(i)%mouill(ens) = p(i)%mouill(ens) * p(i)%surf(ens)
            else
              precip2(1) = precip1(1)
              precip2(2) = precip1(2)
            endif
!            write(618,*) precip2(1) , precip2(2)
          endif
        end do


        p(i)%mouill(sc%aoas) = p(i)%mouill(sc%as) * p(i)%surf(sc%as) &
                             + p(i)%mouill(sc%ao) * p(i)%surf(sc%ao)

        p(i)%interpluie(sc%aoas) = p(i)%interpluie(sc%as) + p(i)%interpluie(sc%ao)

! DR 03/06/2014 je teste ici pour concerver lkes 2 plantes
!      write(618,*)'plante',i,preciptmp(AS),p(i)%surf(AS),preciptmp(AO),p(i)%surf(AO)

      end do

    ! domi - 13/10/2004 - on le sort de la boucle des plantes car airg est deja le cumul de 2 plantes
    ! PB - 06/12/2004 - on le met après l'appel à irrig en cas de calcul automatique des irrigations.
      sc%totir  = sc%totir + sc%airg(n)

    ! DR 18/09/07 pour benj on conserve les dates d'apport

    !!!MODIF HISAFE 11 : Supression code inutile
    !!!  do i = 1, sc%P_nbplantes
    !!!    if (sc%airg(n) > 0) then
    !!!       t%nbapirr(i) = t%nbapirr(i) + 1
    !!!      t%dateirr(i,t%nbapirr(i)) = sc%jjul
    !!!   endif
    !!!  enddo


    ! Reconstitution de precip et des quantités d'eau sur les feuilles
      i = sc%P_nbplantes

      sc%precip = preciptmp(AS) * p(i)%surf(AS) + preciptmp(AO) * p(i)%surf(AO)


! DR 10/07/2014 correction permettant de prendre en compte les pluies anterieures au semis lorsque qu'on est an CAS
! tests debuggage 28082014 confirme qu'il faut garder la modif


 !!!MODIF HISAFE sinon ça plante
    if (sc%P_nbplantes > 1) then
      if((p(2)%surf(AS)+p(2)%surf(AO)).eq.0)sc%precip=preciptmp_plante1
    endif


!	write(618,*)'precip dans eauentrant1', sc%precip,preciptmp(AS)
      do i = sc%P_nbplantes-1,1,-1
        sc%precip = sc%precip + p(i)%stemflow
!      write(618,*)'precip dans eauentrant2',i, sc%precip,p(i)%stemflow
      end do

return
end subroutine eauEntrantSysteme
 
 
