subroutine ecriture_start(sc,soil)
USE Stics
!USE Plante
!USE Itineraire_Technique
USE Sol
!USE Climat
!USE Station
!USE Parametres_Generaux
!USE Divers
USE Lixivation, only:  calculRsurRU, calculRU

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc

!  type(Parametres_Generaux_), intent(IN)    :: pg

!  type(Plante_),              intent(INOUT) :: p (sc%P_nbplantes)

!  type(ITK_),                 intent(INOUT) :: itk (sc%P_nbplantes)

  type(Sol_),                 intent(INOUT) :: soil

!  type(Climat_),              intent(INOUT) :: c

!  type(Station_),             intent(INOUT) :: sta

!  type(Stics_Transit_),       intent(INOUT) :: t
! DR je calcule les varaibles pour le jour start
        ! calcul de la réserve jusqu'à profcalc
      sc%resmes = SUM(sc%hur(1:int(soil%profcalc))+sc%sat(1:int(soil%profcalc)))
      sc%azomes = SUM(soil%nit(1:int(soil%profcalc)))
      sc%ammomes = SUM(soil%amm(1:int(soil%profcalc)))

    ! calcul de RsurRU
      sc%RU = calculRU(soil%profsol, sc%hucc, sc%humin)
      sc%RsurRU = calculRsurRU(sc%RU, soil%profsol, sc%hur, sc%sat, sc%humin)




end subroutine ecriture_start
