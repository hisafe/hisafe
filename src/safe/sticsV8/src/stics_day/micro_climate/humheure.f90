! *----------------------------------------------* c
! * Reconstitution des humidités horaires        * c
! * (inspirée de Magret : Choisnel & Lagouarde)  * c
! *----------------------------------------------* c
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 6.6.3.b, page 121
!>
!! To enable coupling with plant disease models, an hourly reconstitution of microclimate state variables (crop temperature and air moisture) is made according
!! to the following principle:
!>  - The maximum crop temperature is assumed to occur at 14h00 TU and the minimum at sunrise. Between these two dates, linear interpolations make it possible
!!   to reconstitute hourly temperatures.
!>  - The dew point temperature is calculated from tcult and humidite by reversing the tvar function (see the module tvar.f90). An hourly reconstitution similar
!! to that used for the crop temperature is made by applying recurrent hypotheses to the minimum value of the dewpoint temperature, until there is convergence
!! at the level of average daily moisture levels.
! ------------------------------------------------------------------------------------------------------------------------------------------------------------* c

!: ML - 29/10/12 calcul durée humectation (ajout des variables tmin_jour, dureehumec et compteurhumheure dans la liste des arguments)

!subroutine humheure(n,tcultmin,tcultmax,tmaxveille,humidite,daylen,tmin_demain,tcult,trosemax,humimoy,tmin_jour,dureehumec,   &
!                    dureeRH,dureeRH1,dureeRH2,compteurhumheure,trr)
subroutine humheure(n,tcultmin,tcultmax,tmaxveille,humidite,daylen,tmin_demain,tcult,trosemax,humimoy,   &
                   tmin_jour,codetrosee,tculth,troseh,humh)

USE Divers, only: TVAR

  implicit none

!: Arguments

  integer, intent(IN)    :: n  
  real,    intent(IN)    :: tcultmin  
  real,    intent(IN)    :: tcultmax   !> // OUTPUT // Crop surface temperature (daily maximum) // degree C
  real,    intent(IN)    :: tmaxveille  
  real,    intent(IN)    :: humidite   !> // OUTPUT // Moisture in the canopy // %
  real,    intent(IN)    :: daylen  
  real,    intent(IN)    :: tmin_jour
  real,    intent(IN)    :: tmin_demain  
  integer, intent(IN)    :: codetrosee
!  integer, intent(IN)    :: compteurhumheure

  real,    intent(INOUT) :: tcult   !> // OUTPUT // Crop surface temperature (daily average) // degree C
  real,    intent(INOUT) :: trosemax(0:n)  
  real,    intent(INOUT) :: humimoy  
!  real,    intent(INOUT) :: dureehumec
!  real,    intent(INOUT) :: dureeRH1
!  real,    intent(INOUT) :: dureeRH2
!  real,    intent(INOUT) :: dureeRH
!  real,    intent (OUT)  :: trr

  real,    intent(OUT)    :: troseh(24)
  real,    intent(OUT)    :: tculth(24)
  real,    intent(OUT)    :: humh(24)



!: Variables locales
  integer :: i  !>  
  integer :: in  
  real    :: heuremin  !>  
  real    :: diffrosemin  
!  real    :: troseh(24)
!  real    :: tculth(24),humh(24)
  real    :: trosee  !>  
  real    :: h  !>  
  real    :: hx  

!: ML - 29/10/12 - calcul de la temperature de rosee horaire selon STICS ou selon Debele Bekele (voir Debele et al., 2007 et Bregaglio et al., 2010)
  real    :: trosej
  real    :: pi
!  integer :: code_trosee
!: ML fin


! fonction(s)
  real    :: F_temprosee

      tcult = (tcultmin + tcultmax) / 2

      trosee = F_temprosee(humidite * TVAR(tcult))
      trosemax(n) = 2 * trosee - tcultmin
      trosemax(0) = trosemax(1)

      heuremin = 12.0 - (daylen / 2.0)
      humimoy = 0.0
      in = 0
      hx = 14.0
      diffrosemin = 0.0

!: ML - 29/10/12 - calcul de la temperature de rosee horaire selon STICS ou selon Debele Bekele (voir Debele et al., 2007 et Bregaglio et al., 2010)
      pi = 4 * atan(1.0)
! si code_trosee = 1 la temperature de rosee horaire est calculee selon STICS (calcul initial du code),
! si code_trosee = 2 elle est calculée selon Debele Bekele
!      code_trosee = 2
!: ML fin

B1:   do while(.TRUE.)

        in = in + 1

        do  i = 1,24

          ! calcul de tcultheure
          h = float(i)
          if (h < heuremin) then
            tculth(i) = tmaxveille - (-hx - h) / (-hx - heuremin) * (tmaxveille - tcultmin)
!            write(70,*) n,i,tculth(i),tmaxveille,hx,h,heuremin,tcultmin
          endif

          if (h >= heuremin .and. h < hx) then
            tculth(i) = tcultmin + (h - heuremin) / (hx - heuremin) * (tcultmax - tcultmin)
!            write(70,*) n,i,tculth(i),tcultmax,hx,h,heuremin,tcultmin
          endif

          if (h >= hx) then
            tculth(i) = tcultmax + (hx - h) / (24 - heuremin) * (tcultmax - tmin_demain)
!            write(70,*) n,i,tculth(i),tcultmax,hx,h,heuremin,tmin(3) ! 3 = n+1
          endif

          ! calcul de troseheure
          !: ML - 29/10/12 - calcul de la temperature de rosee horaire selon STICS
          if (codetrosee == 1) then

            if (h < heuremin) then
                troseh(i) = trosemax(n-1) - (-hx - h) / (-hx - heuremin) * (trosemax(n-1) - (tcultmin - diffrosemin))
            endif

            if (h >= heuremin .and. h < hx) then
                troseh(i) = tcultmin + (h - heuremin) / (hx - heuremin) * (trosemax(n) - tcultmin)
            endif

            if (h >= hx) then
                troseh(i) = trosemax(n) + (hx - h) / (24 - heuremin) * (trosemax(n) - (tmin_demain - diffrosemin))
            endif
          endif
          !: ML - 29/10/12 - calcul de la temperature de rosee horaire selon Debele Bekele (voir Debele et al., 2007 et Bregaglio et al., 2010)
          if (codetrosee == 2) then
            trosej = 0.9153 * tmin_jour + 0.2021
            troseh(i) = trosej + i/24*(tmin_jour - tmin_demain) + 0.5*sin ((i+1)* pi/6 - 3*pi/4)
          endif
          !: ML fin

          humh(i) = TVAR(troseh(i)) / TVAR(tculth(i))
!          write(70,*) n,i,humh(i),troseh(i),tculth(i)
          humh(i) = min(humh(i), 1.0)

          !: Calcul d'une moyenne vraie
          humimoy = humimoy + humh(i)
!          write(70,*) n,i,humimoy, humh(i)

        end do

        humimoy = humimoy / 24.
!        write(70,*) n,humimoy


        if (humimoy - humidite > 0.05 .and. in < 10) then
          diffrosemin = diffrosemin + 1.0
        else
          EXIT B1
        endif

      end do B1

!: ML - 29/10/12 - calcul de la durée d'humectation + JC Rajout condition pluie
! do i=1,24
!  if (compteurhumheure == 1) then
!    if (trr .eq. 0) then
!          if (tculth(i) < troseh(i)) then
!              dureehumec = dureehumec + 1
!          else
!              dureehumec = dureehumec
!          end if
!    end if
!    if (trr .gt. 0) then
!          if (humh(i) .ge. 0.95) then
!             dureehumec = dureehumec + 1
!          else
!             dureehumec = dureehumec
!          end if
!    end if
!  end if
!end do
!: ML fin

! ML et JC - 29/10/12: calcul de la durée de RH nocturne supérieure à un seuil (utilisé dans le cas de la sporulation)
! if (compteurhumheure == 1) then
! dureeRH1 = 0
! dureeRH = 0
!   do i = 1,7
!    if (humh(i) .ge. 0.90)then
!      dureeRH1 = dureeRH1 + 1
!    end if
!   end do
!   dureeRH = dureeRH1 + dureeRH2
!   dureeRH2 = 0
!   do i = 19,24
!    if (humh(i) > 0.90) then
!      dureeRH2 = dureeRH2 + 1
!    end if
!   end do
!end if
! JC fin

!      jul = n + P_iwater - 1

return
end subroutine humheure
 
 
