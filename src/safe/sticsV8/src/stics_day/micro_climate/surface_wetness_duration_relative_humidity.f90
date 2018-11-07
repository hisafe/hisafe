

subroutine surface_wetness_duration_relative_humidity(tculth,troseh,humh, &
           dureehumec,dureeRH,dureeRH1,dureeRH2,trr)

USE Divers, only: TVAR

  implicit none

!: Arguments

!  integer, intent(IN) :: n
  real,    intent(IN) :: trr
  real,    intent(IN) :: tculth(24)
  real,    intent(IN) :: troseh(24)
  real,    intent(IN) :: humh(24)
  real,    intent(OUT) :: dureehumec !> // OUTPUT // wetness duration    // hour
  real,    intent(OUT) :: dureeRH1
  real,    intent(OUT) :: dureeRH2
  real,    intent(OUT) :: dureeRH !> // OUTPUT //duration of night relative humidity higher than a given threshold   // hour




!: Variables locales
  integer :: i  !>

! ML et DR le 29/10/12 Initialisation de dureehumec à 0 au début de la journée
  dureehumec = 0.0


 do i=1,24
!  if (compteurhumheure == 1) then
    if (trr .eq. 0) then
          if (tculth(i) < troseh(i)) then
              dureehumec = dureehumec + 1
          else
              dureehumec = dureehumec
          end if
    end if
    if (trr .gt. 0) then
          if (humh(i) .ge. 0.95) then
             dureehumec = dureehumec + 1
          else
             dureehumec = dureehumec
          end if
    end if
!  end if
end do
!: ML fin

! ML et JC - 29/10/12: calcul de la durée de RH nocturne supérieure à un seuil (utilisé dans le cas de la sporulation)
! if (compteurhumheure == 1) then
 dureeRH1 = 0
 dureeRH = 0
   do i = 1,7
    if (humh(i) .ge. 0.90)then
      dureeRH1 = dureeRH1 + 1
    end if
   end do
   dureeRH = dureeRH1 + dureeRH2
   dureeRH2 = 0
   do i = 19,24
    if (humh(i) > 0.90) then
      dureeRH2 = dureeRH2 + 1
    end if
   end do
! end if

return
end subroutine surface_wetness_duration_relative_humidity

