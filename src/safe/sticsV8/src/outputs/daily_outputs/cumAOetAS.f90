! sous-programme qui cumule les variables d'état ombre et soleil
! pour ne faire qu'une sortie pour la culture
! TODO: faire des cumuls au fur et à mesure des calculs et non pas tout à la fois en fin de boucle journalière.
!> subroutine that combines the state variables shade and sun
!! to make only outlet for culture
subroutine cumAOetAS(n,P_codesimul,p,itk)

USE Plante
USE Itineraire_Technique

  implicit none

!: Arguments
  integer,            intent(IN)    :: n  
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=12),  intent(IN)    :: P_codesimul  !> // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0
  integer,  intent(IN)    :: P_codesimul  !> // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0

  type(Plante_),      intent(INOUT) :: p  

  type(ITK_),         intent(IN)    :: itk  



!: Variables locales
  integer :: iz  !>  
  integer ::  AO  !>  
  integer ::  AS  
  real    :: rdtote  


      AS = 1  ! au soleil
      AO = 2  ! à l'ombre

! ** variables à pondérer
! *- ici les variables "xxxT" sont des variables "temporaires" de cumul
! *- utilisés normalement uniquement dans les fonctions de sorties
! *- (et notamment sortie.for et lecsorti.for)
! *- et les variables "xxx(0)" sont les memes variables mais à portées
! *- plus large que sortie.for et lecsorti.for
! *- typiquement, on les retrouve dans rapport.for et lecrap.for,
! *- ou bilan.for et bilcoupe.for, et parfois ailleurs mais très rarement.

      p%mouill(0) = p%mouill(AS) * p%surf(AS) + p%mouill(AO) * p%surf(AO)

      p%deltai(0,n) = p%deltai(AS,n) * p%surf(AS) + p%deltai(AO,n) * p%surf(AO)

      p%dltaisen(0) = p%dltaisen(AS) * p%surf(AS) + p%dltaisen(AO) * p%surf(AO)

      p%dltams(0,n) = p%dltams(AS,n) * p%surf(AS) + p%dltams(AO,n) * p%surf(AO)

      p%dltamsen(0) = p%dltamsen(AS) * p%surf(AS) + p%dltamsen(AO) * p%surf(AO)

      p%dltamstombe(0) = p%dltamstombe(AS) * p%surf(AS) + p%dltamstombe(AO) * p%surf(AO)

      p%dltags(0) = p%dltags(AS) * p%surf(AS) + p%dltags(AO) * p%surf(AO)

      p%dltaremobil(0) = p%dltaremobil(AS) * p%surf(AS) + p%dltaremobil(AO) * p%surf(AO)

    ! dr 151107 pour sorties climator
      p%photnet(0) = p%photnet(AS) * p%surf(AS) + p%photnet(AO) * p%surf(AO)

      p%eai(0) = p%eai(AS) * p%surf(AS) + p%eai(AO) * p%surf(AO)

      p%fapar(0) = p%fapar(AS) * p%surf(AS) + p%fapar(AO) * p%surf(AO)

      p%fpft(0) = p%fpft(AS) * p%surf(AS)+ p%fpft(AO) * p%surf(AO)

      p%fpv(0) = p%fpv(AS) * p%surf(AS)+ p%fpv(AO) * p%surf(AO)

      p%nfruitnou(0) = p%nfruitnou(AO) * p%surf(AO) + p%nfruitnou(AS) * p%surf(AS)

      p%pousfruit(0) = p%pousfruit(AO) * p%surf(AO) + p%pousfruit(AS) * p%surf(AS)

      p%pdsfruitfrais(0) = p%pdsfruitfrais(AO) * p%surf(AO) + p%pdsfruitfrais(AS) * p%surf(AS)

      p%raint(0)  = p%raint(AO) * p%surf(AO)+ p%raint(AS) * p%surf(AS)

      p%sourcepuits(0) = p%sourcepuits(AO) * p%surf(AO) + p%sourcepuits(AS) * p%surf(AS)

      p%splai(0) = p%splai(AO) * p%surf(AO) + p%splai(AS) * p%surf(AS)

      p%spfruit(0) = p%spfruit(AO) * p%surf(AO) + p%spfruit(AS) * p%surf(AS)

      p%sla(0) = p%sla(AO) * p%surf(AO) + p%sla(AS) * p%surf(AS)

      p%vitmoy(0) = p%vitmoy(AO) * p%surf(AO) + p%vitmoy(AS) * p%surf(AS)

      p%allocfruit(0) = p%allocfruit(AO) * p%surf(AO) + p%allocfruit(AS) * p%surf(AS)

      p%huile(0) = p%huile(AO) * p%surf(AO) + p%huile(AS) * p%surf(AS)

      p%sucre(0) = p%sucre(AO) * p%surf(AO) + p%sucre(AS) * p%surf(AS)

      p%h2orec(0) = p%h2orec(AO) * p%surf(AO) + p%h2orec(AS) * p%surf(AS)

      p%resperenne(0) = p%resperenne(AO) * p%surf(AO) + p%resperenne(AS) * p%surf(AS)

      p%remobilj(0) = p%remobilj(AO) * p%surf(AO) + p%remobilj(AS) * p%surf(AS)

      p%mabois(0) = p%mabois(AO) * p%surf(AO) + p%mabois(AS) * p%surf(AS)

      p%maenfruit(0) = p%maenfruit(AO) * p%surf(AO) + p%maenfruit(AS) * p%surf(AS)

      p%mafrais(0) = p%mafrais(AO) * p%surf(AO) + p%mafrais(AS) * p%surf(AS)

      p%mafeuil(0) = p%mafeuil(AO) * p%surf(AO) + p%mafeuil(AS) * p%surf(AS)

      p%mafeuiljaune(0) = p%mafeuiljaune(AO) * p%surf(AO) + p%mafeuiljaune(AS) * p%surf(AS)

      p%mafeuilverte(0) = p%mafeuilverte(AO) * p%surf(AO) + p%mafeuilverte(AS) * p%surf(AS)

      p%mafeuiltombe(0) = p%mafeuiltombe(AO) * p%surf(AO) + p%mafeuiltombe(AS) * p%surf(AS)

      p%masecveg(0) = p%masecveg(AO) * p%surf(AO) + p%masecveg(AS) * p%surf(AS)

      p%matigestruc(0) = p%matigestruc(AO) * p%surf(AO) + p%matigestruc(AS) * p%surf(AS)

      p%offrenod(0) = p%offrenod(AO) * p%surf(AO) + p%offrenod(AS) * p%surf(AS)

      p%fixreel(0) = p%fixreel(AO) * p%surf(AO) + p%fixreel(AS) * p%surf(AS)

      p%fixpot(0) = p%fixpot(AO) * p%surf(AO) + p%fixpot(AS) * p%surf(AS)

      p%fixmaxvar(0) = p%fixmaxvar(AO) * p%surf(AO) + p%fixmaxvar(AS) * p%surf(AS)

      p%qfix(0) = p%Qfix(AO) * p%surf(AO) + p%Qfix(AS) * p%surf(AS)

      p%durvie(0,n) = p%durvie(AO,n) * p%surf(AO) + p%durvie(AS,n) * p%surf(AS)

!      do iz = 1,nbrecolte-1
      do iz = 1, p%P_nboite
        p%pdsfruit(0,iz) = p%pdsfruit(AO,iz) * p%surf(AO) + p%pdsfruit(AS,iz) * p%surf(AS)
!        p%nfruit(0,iz) = p%nfruit(0,iz)                                     &
!                       + (p%nfruit(AO,iz) - p%nfruitv(AO,iz)) * p%surf(AO)  &
!                       + (p%nfruit(AS,iz) - p%nfruitv(AS,iz)) * p%surf(AS)
        p%nfruitv(AO,iz) = p%nfruit(AO,iz)
        p%nfruitv(AS,iz) = p%nfruit(AS,iz)
      end do


! *** // variables à cumuler (une seule fois !) //

!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!      if (lge(P_codesimul,'feuille') .eqv. .FALSE.) then
      if (P_codesimul == 1) then

        if (p%P_codelaitr == 1) then
          p%lai(0,n) = p%lai(AS,n) * p%surf(AS) + p%lai(AO,n) * p%surf(AO)
        endif

        p%laisen(0,n) = p%laisen(0,n-1)                                   &
                      + (p%laisen(AO,n) - p%laisen(AO,n-1)) * p%surf(AO)  &
                      + (p%laisen(AS,n) - p%laisen(AS,n-1)) * p%surf(AS)

      else
        p%lai(0,n) = p%lai(AS,n)
        p%laisen(0,n) = p%laisen(AS,n)
        p%deltai(0,n) = max(p%lai(AS,n) - p%lai(AS,n-1),0.0)
      endif

      p%magrain(0,n) = p%magrain(0,n-1)                                   &
                     + (p%magrain(AO,n) - p%magrain(AO,n-1)) * p%surf(AO) &
                     + (p%magrain(AS,n) - p%magrain(AS,n-1)) * p%surf(AS)

!: Cumul des rendements
      if (n == p%nrec .and. p%nbrecolte > 1) then
        p%rdtint(0,p%nbrecolte-1) = p%rdtint(AS,p%nbrecolte-1) * p%surf(AS) + p%rdtint(AO,p%nbrecolte-1) * p%surf(AO)
      endif

    ! Cumul de masec AO/AS
    !- sauf s'il a déjà été calculé (par ex. dans inicoupe)
      if (p%masec(0,n) <= 0.) then
        p%masec(0,n) = p%masec(0,n-1) + p%dltams(0,n) - p%dltamstombe(0)
      endif
!        write(*,*)'dans cumaoas',p%masec(0,n),p%masec(0,n-1) , p%dltams(0,n) , p%dltamstombe(0)

    ! Si récolte/cueillette, on enleve la partie récoltée/cueillée du cumul de masec
      if (itk%P_codcueille == 2) then
        if (n == p%nrec .and. p%nrec > 0) then
          if (itk%P_nbcueille == 1) rdtote = p%magrain(0,n-1)/100.
          if (itk%P_nbcueille == 2) then
            rdtote = p%rdtint(0,p%nbrecolte-1)/100.
          endif
! *- n = nrec
! DR 05/12/2014 attention on a deja enleve une fois la rdt recolté
!          p%masec(0,n) = p%masec(0,n) - rdtote
        endif
      endif

      if (p%masec(0,n) > 0.) then
        p%ptigestruc(0)    = p%matigestruc(0)  / p%masec(0,n)
        p%penfruit(0)      = p%maenfruit(0)    / p%masec(0,n)
        p%preserve(0)      = p%resperenne(0)   / p%masec(0,n)
        p%pfeuil(0)         = p%mafeuil(0)      / p%masec(0,n)
        p%pfeuiljaune(0)   = p%mafeuiljaune(0) / p%masec(0,n)
        p%pfeuilverte(0,n) = p%mafeuilverte(0) / p%masec(0,n)
      else
        p%ptigestruc(0)    = 0.
        p%penfruit(0)      = 0.
        p%preserve(0)      = 0.
        p%pfeuil(0)      = 0.
        p%pfeuiljaune(0)   = 0.
        p%pfeuilverte(0,n) = 0.
      endif


      if (n == p%nlev) then
        if (p%P_codelaitr == 1) then
! --            lai(0,n) = lai(0,n-1)+P_lai0
          if (p%P_lai0 > p%lai(0,n)) then
            p%lai(0,n) = p%P_lai0
          endif
        endif
! *!* que faire de cela ? --> msres0 obsolete
! *!* -- masec(0,n) = masec(0,n-1)+msres0
      endif

      p%hauteur(0) = p%hauteur(AO) * p%surf(AO) + p%hauteur(AS) * p%surf(AS)

!DR 15/04/2016 on fait des cumuls etranges ceci etant deja fait dans croissance
!      p%masecneo(0) = p%masecneo(0)                                         &
!                    + (p%masecneo(AO) - p%masecneov(AO)) * p%surf(AO)       &
!                    + (p%masecneo(AS) - p%masecneov(AS)) * p%surf(AS)
!      p%msneojaune(0) = p%msneojaune(0)                                     &
!                      + (p%msneojaune(AO) - p%msneojaunev(AO)) * p%surf(AO) &
!                      + (p%msneojaune(AS) - p%msneojaunev(AS)) * p%surf(AS)
!      p%msresjaune(0) = p%msresjaune(0)                                     &
!                      + (p%msresjaune(AO) - p%msresjaunev(AO)) * p%surf(AO) &
!                      + (p%msresjaune(AS) - p%msresjaunev(AS)) * p%surf(AS)
!      p%msres(0) = p%msres(0)                                               &
!                 + (p%msres(AO) - p%msresv(AO)) * p%surf(AO)                &
!                 + (p%msres(AS) - p%msresv(AS)) * p%surf(AS)

! Cumul déjà effectué après l'appel à la routine "grain"
!      p%nbgrains(0) = p%nbgrains(0)                                         &
!                    + (p%nbgrains(AO) - p%nbgrainsv(AO)) * p%surf(AO)       &
!                    + (p%nbgrains(AS) - p%nbgrainsv(AS)) * p%surf(AS)

    ! PB : 24/07/2009 - Ces variables ao/as ne possédaient pas de variables de cumuls dans la 6.4
    ! mais dans la version modularisée, toutes les variables ao/as ont aussi un indice aoas de cumul
    ! JE rajoute donc les cumuls manquants
      p%mafraisfeuille(0) = p%mafraisfeuille(AS) * p%surf(AS) &
                          + p%mafraisfeuille(AO) * p%surf(AO)
      p%mafraisrec(0)     = p%mafraisrec(AS)     * p%surf(AS) &
                          + p%mafraisrec(AO)     * p%surf(AO)
      p%mafraisres(0)     = p%mafraisres(AS)     * p%surf(AS) &
                          + p%mafraisres(AO)     * p%surf(AO)
      p%mafraistige(0)    = p%mafraistige(AS)    * p%surf(AS) &
                          + p%mafraistige(AO)    * p%surf(AO)

    ! TODO: QNplanteres



      if (p%lai(AS,n) <= 0.0 .and. p%lai(AO,n) <= 0.0) p%lai(0,n) = 0.0

      if (p%masec(AO,n) <= 0.0 .and. p%masec(AS,n) <= 0.0) then
        p%magrain(0,n) = 0.0
        p%masec(0,n) = 0.0
      ! probleme dans bilcoupe si masecneo(0) = 0.0 alors masectot = 0.0
      !--  masecneo(0) = 0.0
      !--  ms(0,n) = 0.0
        p%laisen(0,n) = 0.0
      !--  msresjaune(0) = 0.0
        p%msres(0) = 0.0
      ! on saute ces mises à zéros, valables que pour la plante associée
      !!!MODIF HISAFE on change la façon de faire le test
      !!!if (.not. p%estDominante) then
        if (p%estDominante) then
        else
          p%msneojaune(0) = 0.0
!DR 27/09/2012 etrange de mettre nbgrains à 0 pour la plante dominée ?? j'enleve pour nbgrains et nbfruit
! DR 27/09/2012 voir si on eleve aussi le
!          p%nbgrains(0) = 0.0
!          p%nfruit(0,1:p%P_nboite) = 0.0
        endif
      endif


    ! on fait une sauvegarde des variables pour pouvoir faire le "delta" au jour suivant.
!      p%masecneov(AO)   = p%masecneo(AO)
!      p%msneojaunev(AO) = p%msneojaune(AO)
!      p%msresjaunev(AO) = p%msresjaune(AO)
!      p%msresv(AO)      = p%msres(AO)
!      p%nbgrainsv(AO)   = p%nbgrains(AO)
!      p%masecneov(AS)   = p%masecneo(AS)
!      p%msneojaunev(AS) = p%msneojaune(AS)
!      p%msresjaunev(AS) = p%msresjaune(AS)
!      p%msresv(AS)      = p%msres(AS)
!      p%nbgrainsv(AS)   = p%nbgrains(AS)
!      p%masecneov(0)   = p%masecneo(0)
!      p%msneojaunev(0) = p%msneojaune(0)
!      p%msresjaunev(0) = p%msresjaune(0)
!      p%msresv(0)      = p%msres(0)
!      p%nbgrainsv(0)   = p%nbgrains(0)




! ** PB - 06/02/2004
! *- en realité, lai(0) n'est plus la somme des deltai donc, on a plus besoin de
! *- faire la difference entre le "cumul des deltai" et le "cumul des delta lai senescent"
! *- pour retrouver le lai "réel" du jour courant.
! -- <obsolete>
! -- on transfere ça là ou il y en a besoin (lecsorti et sortie)
! --      lai(0,n) = max(lai(0,n)-laisen(0),0.0)
! -- </obsolete>


    ! variables cumulées

      p%interpluie(0) = p%interpluie(AO) + p%interpluie(AS)

      p%abso(0,n) = p%abso(AO,n) + p%abso(AS,n)
! --      demande(0) = demande(AO)+demande(AS)
! --      eop(0) = eop(AO)+eop(AS)
! --      ep(0)2 = ep(AO)+ep(AS)
! --      flusol(0) = flusol(AO)+flusol(AS)
! --      flurac(0) = flurac(AO)+flurac(AS)
! --      QNplante(0) = QNplante(AO)+QNplante(AS)
! --      QNgrain(0) = QNgrain(AO)+QNgrain(AS)
!

      p%demande(0) = p%demande(AO) * p%surf(AO) + p%demande(AS) * p%surf(AS)

      p%eop(0) = p%eop(AO) * p%surf(AO) + p%eop(AS) * p%surf(AS)

      p%ep(0) = p%ep(AO) * p%surf(AO) + p%ep(AS) * p%surf(AS)

      p%QNplante(0,n) = p%QNplante(AO,n) * p%surf(AO) + p%QNplante(AS,n) * p%surf(AS)

      p%QNgrain(0) = p%QNgrain(AO) * p%surf(AO) + p%QNgrain(AS) * p%surf(AS)


!: Variables recalculées
!!!MODIF HISAFE on change la façon de faire le test
!!!    if (.not. p%estDominante) then

    if (p%estDominante) then

        p%CNgrain(0)    = p%CNgrain(AS)
        p%CNplante(0)   = p%CNplante(AS)
        p%swfac(0)      = p%swfac(AS)
        p%inn(0)        = p%inn(AS)
        p%inns(0)       = p%inns(AS)
        p%innsenes(0)   = p%innsenes(AS)
        p%innlai(0)     = p%innlai(AS)
        p%turfac(0)     = p%turfac(AS)
      ! DR 060906 pour Aisabelle qui veut une variable qui n'existait pas
        p%senfac(0)     = p%senfac(AS)
        p%epsib(0)      = p%epsib(AS)
        p%irazo(0,n)    = p%irazo(AS,n)
        p%ircarb(0,n)   = p%ircarb(AS,n)
        p%pgrain(0)     = p%pgrain(AS)
        p%teaugrain(0)  = p%teaugrain(AS)


    else

        if (p%magrain(0,n) > 0.0) then
          ! bug corrigé le 13/06/05 NB car magrain est en g/m2
          p%CNgrain(0) = p%QNgrain(0) / p%magrain(0,n) * 10
        else
          p%CNgrain(0) = 0.0
        endif

        if (p%masec(0,n) > 0.0) then
          p%CNplante(0) = p%QNplante(0,n) / (p%masec(0,n) * 10)
!   write(ficdbg,'(i3,2f16.12)')n,QNplante(0),masec(0,n)
        else
          p%CNplante(0) = 0.0
        endif

        p%swfac(0)    = min(p%swfac(AO), p%swfac(AS))
        p%inn(0)      = min(p%inn(AO), p%inn(AS))
        p%inns(0)     = min(p%inns(AO), p%inns(AS))
        p%innsenes(0) = min(p%innsenes(AO), p%innsenes(AS))
        p%innlai(0)   = min(p%innlai(AO), p%innlai(AS))
        p%turfac(0)   = min(p%turfac(AO), p%turfac(AS))
      ! DR 060906 pour Aisabelle  qui veut une variable qui n'existait pas
        p%senfac(0)   = min(p%senfac(AO), p%senfac(AS))
        p%epsib(0)    = (p%epsib(AO) + p%epsib(AS)) / 2
        p%irazo(0,n)  = (p%irazo(AO,n) + p%irazo(AS,n)) / 2
        p%ircarb(0,n) = (p%ircarb(AO,n) + p%ircarb(AS,n)) / 2
        p%pgrain(0)   = (p%pgrain(AO) + p%pgrain(AS)) / 2
        p%teaugrain(0) = (p%teaugrain(AO) + p%teaugrain(AS)) / 2

    end if




return
end subroutine cumAOetAS
 
 
