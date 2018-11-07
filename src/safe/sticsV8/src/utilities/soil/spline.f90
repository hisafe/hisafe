! ------------------------------------------------------------------------ c
! ************* SUBROUTINE SPLINE(XP,YP,NP,C1,C2,C3,C4) ****************** c
! ------------------------------------------------------------------------ c
! * Calcul des coefficients des polynomes de spline cubique              * c
! * (XP,YP)i        points a relier   i = 1,NP                           * c
! * (C1,C2,C3,C4)i  coefficients du polynome sur l'intervalle i = 1,NP-1 * c
! ------------------------------------------------------------------------ c
!> Calculation of the coefficients of cubic spline polynomials
!> -(XP, YP) connecting points a i i = 1, NP
!>- (C1, C2, C3, C4) of the polynomial coefficients i in the interval i = 1, NP-1
subroutine SPLINE(XP,YP,NP,C1,C2,C3,C4)

! ** les dimensionnement des tableaux de la ligne suivante sont passes de 1 à 20
! *- le 18-12-96 pour essai probleme quand P_iniprofil = 1

implicit none

! Argument(s)
    real,    intent(IN)    :: XP(20),YP(20)  
    integer, intent(IN)    :: NP  

    real,    intent(INOUT) :: C1(20),C2(20),C3(20),C4(20)  

! Variable(s) locale(s)
    real    :: A(20),B(20),C(20),D(20),S(20),D1(20)  
    integer :: k  !>  
    integer :: N1  
    real    :: DXI  !>  
    real    :: DXI1  !>  
    real    :: S1  !>  
    real    :: SN  


! ** S1  derivee seconde au point XP(1)  : a fixer (valeur nominale 0)
! ** SN  derivee seconde au point XP(NP) : a fixer (valeur nominale 0)
      S1 = 0.
      SN = 0.
      do k = 1,NP-2
        DXI = XP(k+1)-XP(k)
        DXI1 =  XP(k+2)-XP(k+1)
        A(k+1) = DXI
        B(k+1) = 2.*(DXI+DXI1)
        C(k+1) = DXI1
        D(k+1) = 6.*((YP(k+2)-YP(k+1))/DXI1-(YP(k+1)-YP(k))/DXI)
      end do
      C1(2) = C(2)/B(2)
      D1(2) = D(2)/B(2)
      N1 = NP-1
      do k = 2,NP-2
        C1(k+1) =  C(k+1)/(B(k+1)-A(k+1)*C1(k))
        D1(k+1) = (D(k+1)-A(k+1)*D1(k))/(B(k+1)-A(k+1)*C1(k))
      end do
      D1(N1) = (D(N1)-A(N1)*D1(N1-1))/(B(N1)-A(N1)*C1(N1-1))
      S(N1) = D1(N1)
      do k = N1-1,2,-1
        S(k) = D1(k)-C1(k)*S(k+1)
      end do
      S(1) =  S1
      S(NP) = SN
      do k = 1,N1
        DXI   = XP(k+1)-XP(k)
        C4(k) = (S(k+1)-S(k))/(6.*DXI)
        C3(k) = S(k)/2.
        C2(k) = (YP(k+1)-YP(k))/DXI - DXI*(2.*S(k)+S(k+1))/6.
        C1(k) = YP(k)
      end do


return
end subroutine spline
 
 
