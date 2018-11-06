!> Module climat
!> - Description of the struture Climat_
!> - reading of meteorologicals datas
module Climat

implicit none

! Les codes symboliques du module Plante
integer, parameter :: CLIMAT_METHOD_STICS_V6 = 6            !< method inherited from Stics V6
integer, parameter :: CLIMAT_METHOD_XML_V7 = 7              !< Method of reading XML / Javastics
integer, parameter :: CLIMAT_LECTURE_OK = 1                 !< Code OK  
integer, parameter :: CLIMAT_LECTURE_ERREUR_NO_METHOD = -1  !< Error: The method chosen does not correspond to any known method.
!DR 17/09/2012 je remplace le 731 par nb_days_max
!!!MODIF HISAFE 2 : reduction dimension temporelle
!!!integer, parameter :: nb_days_max = 731  ! maximum size of the table climate
integer, parameter :: nb_days_max = 366  ! maximum size of the table climate


TYPE , BIND(C) :: Climat_


! paramètres climat
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!  character(len=2) ::  nometp
  integer ::  nometp   !! remplacement pe/pc/sw/pt => 1/2/3/4

  
!: tableaux de transition  
!!!MODIF HISAFE 5 : suppression variable inutile
!!!  real, dimension(nb_days_max) :: ttrr    ! (nb_days_max)
!!!  real, dimension(nb_days_max) :: ttmin   ! (nb_days_max)
!!!  real, dimension(nb_days_max) :: ttmax   ! (nb_days_max)
!!!  real, dimension(nb_days_max) :: ttrg    ! (nb_days_max)
!!!  real, dimension(nb_days_max) :: ttetp   ! (nb_days_max)
!!!  real, dimension(nb_days_max) :: ttpm    ! (nb_days_max)
!!!  real, dimension(nb_days_max) :: ttvent  ! (nb_days_max)
!!!  real, dimension(nb_days_max) :: ttco2   ! (nb_days_max)
  
!: tableaux finaux
  real, dimension(nb_days_max) :: tetp    !< (nb_days_max) (possible 0:731, car climabri a besoin de t(n-1). Qd n = 1, pas de tetp(0).)  	  // OUTPUT // Efficient potential evapotranspiration (entered or calculated) // mm day-1
  real, dimension(nb_days_max) :: trr     !< (nb_days_max)     // OUTPUT // Rainfall  // mm.day-1
  real, dimension(0:nb_days_max) :: tmoy    !< (0:nb_days_max)     // OUTPUT // Mean active temperature of air // degree C
  real, dimension(nb_days_max) :: tmin    !< (nb_days_max)     // OUTPUT // Minimum active temperature of air // degree C
  real, dimension(nb_days_max) :: tmax    !< (nb_days_max)  	  // OUTPUT // Maximum active temperature of air // degree C
  real, dimension(nb_days_max) :: trg     !< (nb_days_max)  	  // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
  real, dimension(nb_days_max) :: tpm     !< (nb_days_max)  	  // OUTPUT // Vapour pressure in air // mbars
  real, dimension(nb_days_max) :: tvent   !< (nb_days_max)  	  // OUTPUT // Mean speed of B2vent // m.s-1
!: DR - 23/09/2014 on le declare en reel comme les autres sinon pbs dans le calcu de fco2s
  real, dimension(nb_days_max) :: co2     ! !< (nb_days_max)      // OUTPUT // CO2 concentration // ppm


!: pour les cultures sous abri
  real, dimension(nb_days_max) :: trrext  !< (nb_days_max)
  real, dimension(0:nb_days_max) :: tmoyext !< (0:nb_days_max)  	  // OUTPUT // Mean temperature of external air // degree C
  real, dimension(nb_days_max) :: tminext !< (nb_days_max)  	  // OUTPUT // Minimum temperature of external air // degree C
  real, dimension(nb_days_max) :: tmaxext !< (nb_days_max)  	  // OUTPUT // Maximum temperature of external air // degree C
  real, dimension(nb_days_max) :: trgext  !< (nb_days_max)  	  // OUTPUT // Exterior radiation // MJ.m-2
  real, dimension(nb_days_max) :: tpmext  !< (nb_days_max)
  
!!!MODIF HISAFE 7 : Déplacement de variables (de Climat.f90 dans Stics.f90)
!!!  integer :: c%nitetcult(0:nb_days_max)    !< // OUTPUT // Number of iterations to calculate TCULT // SD
  
  real :: tutilrnet  
  real :: difftcult  
  real :: daylen  
  real :: humimoy  
  real :: humair    !< // OUTPUT // Air moisture // 0-1
  real :: phoi          !< // OUTPUT // Photoperiod // hours
  real :: etpp(0:nb_days_max)    !< // OUTPUT // Potential evapotranspiration as given by Penman formula // mm day-1

!: ML - 29/10/12 - calcul de la durée d'humectation
  real :: dureehumec !> // OUTPUT // wetness duration    // hour
  real :: dureeRH1
  real :: dureeRH2
  real :: dureeRH !> // OUTPUT //duration of night relative humidity higher than a given threshold   // hour
!  integer :: compteurhumheure
!: ML fin

!: DR - 09/01/06 - rajout de variables pour rapport
  real :: Ctculttout    !< // OUTPUT // Crop temperature (TCULT) integrated over the simulation period //  degree C
  real :: Ctairtout  
  real :: somdifftculttair    !< // OUTPUT // Cumulated temperature differences (TCULT-TAIR) over the simulation period //  degree C
      !dr 14/09/2012 inutiles
!  real :: somtroseecult    !< // OUTPUT // Cumulated dew point temperatures (from TCULT) over the simulation period //  degree C
!  real :: somtroseeair    !< // OUTPUT // Cumulated dew point temperatures (from TAIR) over the simulation period //  degree C
  real :: Ctetptout    !< // OUTPUT // Potential evapotranspiration (PET) integrated over the simulation period // mm
  real :: Cetmtout    !< // OUTPUT // Maximum evapotranspiration integrated over the simulation period // mm
  real :: Crgtout    !< // OUTPUT // Global radiation integrated over the simulation period // Mj/m2

!: DR - 08/09/06 
  real :: tncultmat    !< // OUTPUT // Average of minimum crop temperatures (TCULTMIN) between LAX and REC // degree C
  real :: amptcultmat     !< // OUTPUT // Mean range of tcult between lax and rec // degree C
  
  integer :: dureelaxrec  
  integer :: nbjechaudage     !< // OUTPUT // Number of shrivelling days between LAX and REC // jours


!: PB - 23/07/2010 - Je déplace ici julzero et julfin qui sont
!  les jours juliens de début et de fin lues dans le fichier climat.
!  Ils ont + leur place ici que dans la structure Stics_.
  integer :: julfin  
  integer :: julzero  

! Je crée aussi des variables pour stocker les jours, mois et années de début et fin de fichier
  integer :: jourzero  
  integer :: jourfin  
  integer :: moiszero  
  integer :: moisfin  
  integer :: anneezero  
  integer :: anneefin  

  real :: humair_percent    !< // OUTPUT // Air moisture // %


end type Climat_

interface Climat_Lecture
module procedure Climat_Lire_fichier, Climat_Lire_fp
end interface

contains


!> Routine de lecture du fichier climatique.
!! @param c  structure of Climat_
!! @param nomFicClimat name of weather file
!<
subroutine Climat_Lire_fichier(c, nomFicClimat, method, P_culturean,  codeRetour, flag_record)

  type(Climat_),     intent(INOUT)         :: c  
!  character(len=60), intent(IN)            :: nomFicClimat
  character(len=255), intent(IN)            :: nomFicClimat
   integer,           intent(IN), optional  :: method
  integer,           intent(IN)            :: P_culturean  !> // PARAMETER // crop status 1 = over 1 calendar year ,other than 1  = on two calendar years (winter crop in northern hemisphere) // code 0/1 // P_USM/USMXML // 0 
  integer,           intent(OUT)           :: codeRetour  
 logical,       intent(IN)    :: flag_record
  
  integer, parameter :: ficClimat = 153 ! numéro unique dans Stics pour le fichier climat.  
  
  ! on ouvre le fichier en lecture

  open(unit=ficClimat, file=nomFicClimat, status='old', action='read', pad='no', iostat=codeRetour )
  if (codeRetour < 0) then ! Erreur à l'ouverture du fichier

    return
  endif
  
  if (present(method)) then
    call Climat_Lecture(c, ficClimat, method, P_culturean, codeRetour, flag_record)
  else
    ! on appelle la lecture du fichier avec le méthode Stics V6
    call Climat_Lecture(c, ficClimat, CLIMAT_METHOD_STICS_V6, P_culturean, codeRetour, flag_record)
  endif

  if (codeRetour /= CLIMAT_LECTURE_OK) then

    return
  endif
  
end subroutine Climat_Lire_fichier


!> Routine de lecture du fichier climatique.
!! @param c  structure of Climat_
!! @param nomFicClimat name of weather file
subroutine Climat_Lire_fp(c, file_pointer, method, P_culturean, codeRetour, flag_record)

  type(Climat_), intent(INOUT) :: c  
  integer,       intent(IN)    :: file_pointer  
  integer,       intent(IN)    :: method  
  integer,       intent(IN)    :: P_culturean  !> // PARAMETER // crop status 1 = over 1 calendar year ,other than 1  = on two calendar years (winter crop in northern hemisphere) // code 0/1 // P_USM/USMXML // 0 
  integer,       intent(OUT)   :: codeRetour  
  logical,       intent(IN)    :: flag_record

  select case(method)

    case(CLIMAT_METHOD_XML_V7)
      !call Climat_Lecture_XML_V7(c, file_pointer)

    case(CLIMAT_METHOD_STICS_V6)
      call Climat_Lecture_V6(c, file_pointer, P_culturean, codeRetour, flag_record)

    case DEFAULT !: la méthode choisie ne correspond à aucune méthode définie => ERREUR
      codeRetour = CLIMAT_LECTURE_ERREUR_NO_METHOD
      return

  end select

end subroutine Climat_Lire_fp


! *******************************************************************************
!> Subroutine CLIMAT_LECTURE
!! reading of climatic data in the temporary file climat.txt
!********************************************************************************
subroutine Climat_Lecture_V6(c,fp,P_culturean,codeRetour,flag_record)

USE Messages
USE Divers, only: nbjParAnnee

implicit none

  !: Arguments
    type(Climat_), intent(INOUT) :: c  
    integer,       intent(IN)    :: fp  
    integer,       intent(IN)    :: P_culturean  !> // PARAMETER // crop status 1 = over 1 calendar year ,other than 1  = on two calendar years (winter crop in northern hemisphere) // code 0/1 // P_USM/USMXML // 0 
    integer,       intent(OUT)   :: codeRetour  
    logical,       intent(IN)    :: flag_record

  !: Variables locales
    integer          :: retour  !>  
    integer          :: i  !>  
    integer          :: k  !>  
    integer          :: jcal  !>  
    integer          :: jul  !>  
    integer          ::  annee  
    integer          :: imois  !>  
    integer          :: ijour  !>  
    integer          ::  nbJAn1  !>  
    integer          :: nbJAn2  
!    character(len=7) :: station  dr 10/08/2012 test sur les nom de station agmip, je rallonge le nombre de caracteres
    character(len=30) :: station
    character(len=150) :: mes999


return
end subroutine Climat_Lecture_V6


end module Climat
 
 
