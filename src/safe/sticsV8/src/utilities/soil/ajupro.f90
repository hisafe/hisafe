! -------------------------------------------------------------------- c
! *************** SUBROUTINE AJUPRO(NH,P_epc,QN,NBCE,Q,QEXC) ************** c
! -------------------------------------------------------------------- c
! * Calcul du profil d'azote par couche élémentaire de 1 cm à partir * c
! * d'un profil mesuré par horizon (de grande épaisseur)             * c
! * Méthode : ajustement par spline cubique des quantités cumulées   * c
! * NH      nombre d'horizons mesurés                                * c
! * P_epc(i)   épaisseur non cumulée de chacun des horizons (cm)       * c
! * QN(i)   quantité d'azote minéral dans chaque horizon (kgN/ha)    * c
! * NBCE       nombre de couches élémentaires                           * c
! * Q(i)    quantité d'azote minéral dans chaque couche (kgN/ha)     * c
! * QEXC    quantités d'azote ajustées en excès par rapport aux      * c
! *   quantités mesurées (lorsqu'on met à 0 les valeurs négatives)   * c
! -------------------------------------------------------------------- c
!> Calculation of the profile of nitrogen by elementary layer of 1 cm from Profile measured by horizon (very thick)
!> Method: cubic spline fit of cumulative quantities
!> - NH number of horizons measured
!> - P_epc (i) non-cumulative thickness of each horizon (cm)
!> - QN (i) quantity of mineral nitrogen in each horizon (kg N / ha)
!> - NBCE number of elementary layers
!> - Q (i) quantity of mineral nitrogen in each layer (kg N / ha)
!> - Amounts of nitrogen QEXC adjusted excess relative to the
!> - Measured quantities (when setting negative values ​​to 0)
subroutine AJUPRO(NH,P_epc,QN,NBCE,Q,QEXC)

implicit none

! Argument(s)
    integer, intent(IN) :: NH  
    real,    intent(IN) :: P_epc(NH)     !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
    real,    intent(IN) :: QN(NH)
    integer, intent(IN) :: NBCE  

    real,    intent(INOUT) :: Q(NBCE)  
    real,    intent(INOUT) :: QEXC  


    real    :: ZP(20),QP(20),C1(20),C2(20),C3(20),C4(20)  
    integer :: i  !>  
    integer :: j  !>  
    integer :: NP  
    real    :: DZ  !>  
    real    :: S1  !>  
    real    :: SN  !>  
    real    :: Y1  !>  
    real    :: Y2  

        NP = NH+1
        ZP(1) = 0.
        QP(1) = 0.
        do i = 1,NH
          ZP(i+1) = ZP(i)+P_epc(i)
          QP(i+1) = QP(i)+QN(i)
        end do

        S1 = 0.
        SN = 0.

        call SPLINE(ZP,QP,NP,C1,C2,C3,C4)

        QEXC = 0.
        Y1 = 0.
        do i = 1,NBCE
          do j = 1,NP-1
            if (FLOAT(I) <= ZP(j+1)) EXIT
          end do
          DZ = FLOAT(i)-ZP(j)
          Y2 = C1(j)+DZ*(C2(j)+DZ*(C3(j)+DZ*C4(j)))
          Q(i) = Y2-Y1
          Y1 = Y2
          if (Q(i) >= 0.) CYCLE
          QEXC = QEXC-Q(i)
          Q(i) = 0.
        end do

return
end subroutine ajupro
 
 
