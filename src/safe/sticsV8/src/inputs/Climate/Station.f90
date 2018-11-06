!> Module station
!> - Description of the struture Station_
!> - reading of station parameters
module Station

implicit none


TYPE , BIND(C) :: Station_


  integer :: P_codecaltemp  !< // PARAMETER // option of use of crop temperature for phasic development calculation : yes (2), no (1)  // code 1/2 // STATION // 0
  integer :: P_codernet  !< // PARAMETER // option of calculation of net radiation // code 1/2/3 // STATION // 0
  integer :: P_codeclichange  !< // PARAMETER // option for climatel change : yes (2), no (1)  // code 1/2 // STATION // 0
  real :: P_zr  !< // PARAMETER // Reference height of meteorological data measurement // m // STATION // 0
  real :: P_ra  !< // PARAMETER // Aerodynamic resistance (used in volatilization  module when we use ETP approach) // s m-1 // STATION // 1 	!zarbi a voir avce marie  // OUTPUT // Aerodynamic resistance between the cover and the reference level P_zr // s.m-1
  real :: P_NH3ref  !< // PARAMETER // NH3 concentration in the atmosphere // ug.m-3 // STATION // 1
  real :: P_aangst  !< // PARAMETER // coefficient of the Angstrom's relationship for extraterrestrial radiation // SD // STATION // 1
  real :: P_bangst  !< // PARAMETER // coefficient of the Angstrom's relationship for extraterrestrial radiation // SD // STATION // 1
  real :: P_coefdevil  !< // PARAMETER // multiplier coefficient of the exterior radiation to compute PET inside of a greenhouse // SD // STATION // 1
  real :: P_albveg  !< // PARAMETER // P_albedo of the vegetation // SD // STATION // 1
  real :: P_altistation  !< // PARAMETER // altitude of the input metorological station  // m // STATION // 0
  real :: P_altisimul  !< // PARAMETER // altitude of simulation // m // STATION // 0
  real :: P_gradtn  !< // PARAMETER // thermal gradient in altitude for minimal temperatures  // degree C m-1 // STATION // 1
  real :: P_gradtx  !< // PARAMETER // thermal gradient in altitude for maximal temperatures  // degree C m-1 // STATION // 1
  real :: P_altinversion  !< // PARAMETER // altitude of inversion of the thermal gradiant // m // STATION // 1
  real :: P_gradtninv  !< // PARAMETER // thermal gradient in altitude for minimal temperatures under the inversion level // degree C m-1 // STATION // 1
  real :: P_cielclair  !< // PARAMETER // threshold for the proportion of sunny hours allowing the inversion of thermal gradiant with altitude // SD // STATION // 1
  real :: P_ombragetx  !< // PARAMETER // shadow effect to calculate the thermal modification in the northern parts of montains  // degree C // STATION // 1
  real :: P_latitude  !< // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0

! déplacé depuis la structure climat
  real :: P_aks  !< // PARAMETER // parameter of calculation of the energetic lost between the inside and the outside of a greenhouse  // Wm-2K-1 // STATION // 1
  real :: P_bks  !< // PARAMETER // parameter of calculation of the energetic lost between the inside and the outside of a greenhouse  // Wm-2K-1 // STATION // 1
  real :: P_cvent  !< // PARAMETER // parameter of the climate calculation under the shelter // SD // STATION // 1
  real :: P_phiv0  !< // PARAMETER // parameter allowing the calculation of the under shelter climate // * // STATION // 1
  real :: P_coefrnet  !< // PARAMETER // coefficient of calculation of the net radiation under greenhouse // * // STATION // 1
  real :: P_patm  !< // PARAMETER // atmospheric pressure // mbars // STATION // 0
  real :: P_corecTrosee  !< // PARAMETER // temperature to substract to Tmin to estimate dew point temperature (in case of missing air humidity data) // degree C // STATION // 1
  integer :: P_codeetp !: DR - 31/10/07 	  // PARAMETER // code of calculation mode of ETP [pe/pc/sw/pt] // code 1/2/3/4 // STATION // 0
  real :: P_alphapt  !< // PARAMETER // Parameter of Priestley-Taylor  // SD // STATION // 1
  integer :: P_codaltitude  !< // PARAMETER // option of calculation of the climate in altitude // code 1/2 // STATION // 0
  integer :: P_codadret  !< // PARAMETER // option of calculation of climate in montain accounting for the orientation (1 : south, 2 : north) // code 1/2 // STATION // 0
  real :: P_aclim  !< // PARAMETER // climatic component of A // mm // STATION // 1
  real :: ra_recal  !< // OUTPUT // Aerodynamic resistance (used in volatilization  module when we use ETP approach) // s m-1

end type Station_


contains

! TODO: les variables du fichier Station appartenant
!       à la structure Climat doivent-elles être
!       transférées dans la structure Station ?
! 09/01/2012 oui , fait
subroutine Station_Lecture(ficsta,sta,path,pathstation)

USE Messages

implicit none

    type(Station_), intent(INOUT) :: sta  
    character(len=50)ficsta

   ! enabling_record :le chemin pour accéder à la config
   character(len=255), intent(IN) :: path ! enabling_record
   ! enabling_record :le chemin pour accéder directement à la config
   character(len=255), intent(IN) :: pathstation ! enabling_record

! Variables locales

! DR 17/07/2012 on lit le nom du fichier station dans new_travail.usm pour optimistics
!      open (36,file = 'station.txt', status = 'unknown')

! DR 19/11/2013 pour Record
   ! to get the full path
    integer ib0                                               ! enabling_record
    integer ib1                                               ! enabling_record
    character(len=300) :: filepluspath                        ! enabling_record
!ec attention utiliser pathstation et pas pathclimat
    ib0 = len_trim(pathstation)                             ! enabling_record
    if (ib0 .ne. 0 ) then                                     ! enabling_record
       filepluspath =  pathstation                          ! enabling_record
    else
       ib1 = len_trim(path)                                   ! enabling_record
       if (ib1 .eq. 0 ) then                                     ! enabling_record
          filepluspath = ficsta                                  ! enabling_record
       else                                                      ! enabling_record
          filepluspath = path(1:ib1) // '/' // ficsta         ! enabling_record
       endif                                                     ! enabling_record
    endif
! fin record

!      open (36,file = ficsta, status = 'unknown')
    open (36,file = filepluspath ,status = 'unknown')         ! enabling_record


! weather Station
!******************
      read (36,*)
      read (36,*,err = 250) sta%P_zr
      read (36,*)
      read (36,*,err = 250) sta%P_NH3ref
! 09/01/2012 passe dans param.par
!      read (36,*)
!      read (36,*,err = 250) sta%P_parsurrg
      read (36,*)
      read (36,*,err = 250) sta%P_latitude
      read (36,*)
      read (36,*,err = 250) sta%P_patm
      read (36,*)
      read (36,*,err = 250) sta%P_aclim

! climate
!******************
      read (36,*)
      read (36,*,err = 250) sta%P_codeetp
      read (36,*)
      read (36,*,err = 250) sta%P_alphapt
      read (36,*)
      read (36,*,err = 250) sta%P_codeclichange
      read (36,*)
      read (36,*,err = 250) sta%P_codaltitude
      read (36,*)
      read (36,*,err = 250) sta%P_altistation
      read (36,*)
      read (36,*,err = 250) sta%P_altisimul
      read (36,*)
      read (36,*,err = 250) sta%P_gradtn
      read (36,*)
      read (36,*,err = 250) sta%P_gradtx
      read (36,*)
      read (36,*,err = 250) sta%P_altinversion
      read (36,*)
      read (36,*,err = 250) sta%P_gradtninv
      read (36,*)
      read (36,*,err = 250) sta%P_cielclair
      read (36,*)
      read (36,*,err = 250) sta%P_codadret
      read (36,*)
      read (36,*,err = 250) sta%P_ombragetx
! microclimate
!******************
      read (36,*)
      read (36,*,err = 250) sta%P_ra
      read (36,*)
      read (36,*,err = 250) sta%P_albveg
      read (36,*)
      read (36,*,err = 250) sta%P_aangst
      read (36,*)
      read (36,*,err = 250) sta%P_bangst
      read (36,*)
      read (36,*,err = 250) sta%P_corecTrosee
      read (36,*)
      read (36,*,err = 250) sta%P_codecaltemp
      read (36,*)
      read (36,*,err = 250) sta%P_codernet
! climate under a shelter
!******************
      read (36,*)
      read (36,*,err = 250) sta%P_coefdevil
      read (36,*)
      read (36,*,err = 250) sta%P_aks
      read (36,*)
      read (36,*,err = 250) sta%P_bks
      read (36,*)
      read (36,*,err = 250) sta%P_cvent
      read (36,*)
      read (36,*,err = 250) sta%P_phiv0
      read (36,*)
      read (36,*,err = 250) sta%P_coefrnet


      close(36)

      return

250   continue
      call EnvoyerMsgHistorique(219)

return
end subroutine Station_Lecture


subroutine Station_Ecriture(sta, P_codabri)

USE Messages

    type(Station_), intent(IN) :: sta  
    integer,        intent(IN) :: P_codabri  !> // PARAMETER // option of calculation of the climate under a shelter // code 1/2 // PARTEC // 0 
    character*40    :: method_etp


    call EnvoyerMsgHistorique('   ')
    call EnvoyerMsgHistorique(261)
    call EnvoyerMsgHistorique('*********************************************')

      call EnvoyerMsgHistorique('P_zr', sta%P_zr)
      call EnvoyerMsgHistorique('P_NH3ref', sta%P_NH3ref)
      call EnvoyerMsgHistorique('P_ra', sta%P_ra)
      call EnvoyerMsgHistorique('P_aangst', sta%P_aangst)
      call EnvoyerMsgHistorique('P_bangst', sta%P_bangst)
      call EnvoyerMsgHistorique('P_albveg', sta%P_albveg)

!      call EnvoyerMsgHistorique('P_parsurrg', sta%P_parsurrg)
      call EnvoyerMsgHistorique('P_latitude', sta%P_latitude)

      call EnvoyerMsgHistorique('P_patm', sta%P_patm)
      call EnvoyerMsgHistorique('P_corecTrosee', sta%P_corecTrosee)
      call EnvoyerMsgHistorique('P_codeclichange', sta%P_codeclichange)

! DR 30/04/2013 je rajoute la methode de calcul de l'etp choisie dans le fichier station
      if(sta%P_codeetp.eq.1)method_etp=' Penman reading'
      if(sta%P_codeetp.eq.2)method_etp=' Penman calculated'
      if(sta%P_codeetp.eq.3)method_etp=' calcul of Shuttleworth-and-Wallace'
      if(sta%P_codeetp.eq.4)method_etp=' calcul of Priestley-Taylor'


      call EnvoyerMsgHistorique('P_codeetp', sta%P_codeetp)
      call EnvoyerMsgHistorique('P_codeetp', method_etp)
      call EnvoyerMsgHistorique('P_alphapt', sta%P_alphapt)

      call EnvoyerMsgHistorique('P_codernet', sta%P_codernet)
      call EnvoyerMsgHistorique('P_codecaltemp', sta%P_codecaltemp)
      call EnvoyerMsgHistorique('P_codaltitude', sta%P_codaltitude)


    ! écritures pour l'altitude - NB - le 05/04
      if (sta%P_codaltitude == 2) then
        call EnvoyerMsgHistorique('P_altistation', sta%P_altistation)
        call EnvoyerMsgHistorique('P_altisimul', sta%P_altisimul)
        call EnvoyerMsgHistorique('P_codadret', sta%P_codadret)
        call EnvoyerMsgHistorique('P_gradtn', sta%P_gradtn)
        call EnvoyerMsgHistorique('P_gradtx', sta%P_gradtx)
        call EnvoyerMsgHistorique('P_altinversion', sta%P_altinversion)
        call EnvoyerMsgHistorique('P_gradtninv', sta%P_gradtninv)
        call EnvoyerMsgHistorique('P_cielclair', sta%P_cielclair)
        call EnvoyerMsgHistorique('P_ombragetx', sta%P_ombragetx)
      endif

      if (P_codabri == 2) then
        call EnvoyerMsgHistorique('P_coefdevil', sta%P_coefdevil)
        call EnvoyerMsgHistorique('P_aks', sta%P_aks)
        call EnvoyerMsgHistorique('P_bks', sta%P_bks)
        call EnvoyerMsgHistorique('P_cvent', sta%P_cvent)
        call EnvoyerMsgHistorique('P_phiv0', sta%P_phiv0)
        call EnvoyerMsgHistorique('P_coefrnet', sta%P_coefrnet)
      endif

return
end subroutine Station_Ecriture


end module Station
 
 
