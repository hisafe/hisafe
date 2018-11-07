! *-----------------------------------------------------------------* c
! *   fonction de simulation de la dynamique des talles d'un couvert* c
! *   prairial par une approche stochastique bas�e sur le           * c
! *   fonctionnement hydrique du couvert et en particulier          * c
! *   des apex fonctionnels � l'int�rieur des gaines                * c
! *   auteurs :
! *   Fran�ois Leli�vre, Sylvain Satger, Fran�oise Ruget & Nadine Brisson
! *   le 08/03/2007
! *   param�tres � indicer sur les plantes
!     P_SurfApex : surface �quivalente des apex transpirants
!     P_SeuilMorTalle : seuil de transpiration relative en de�a duquel meurent les talles
!     cteloigamma : Changement de signification: param�tre 1 servant � d�finir les param�tres de la loi gamma
!                   Si paramgamma=1, cteloigamma=CV
!                   Si paramgamma=2,
!                   Si paramgamma=3, cteloigamma=sigma�
!                  (avant: CV de la loi gamma de la distribution des talles en fonction de leur transpiration relative)
!     cteloigamma2
!     P_VitReconsPeupl: vitesse thermique de reconstitution du peuplement de talles
!     P_SeuilReconsPeupl: seuil de densit� en dessous duquel le peuplement ne pourra pas se reconstituer en totlit�
!     P_MaxTalle : peuplement maximal en nombre de talles/m2
!     variables d'entr�e
!     densite : densit� de talles courante
!     densitemax : densit� potentielle de reconstitution du peuplement � affecter d�s la
!     premi�re mortalit� de talles
!     transpi(g�n�rique) c'est-�-dire soit epC2 : transpiration(mm) soit et : �vapotranspiration de l'ensemble du couvert (sol +plantes)
!     transpipot(g�n�rique) c'est-�-dire soit eopC : transpiration maximale(mm) soit etm : �vapotranspiration potentielle du couvert
!     mortalle : nombre de talles disparues ce jour
!     LAIC(ipl,n)
!     tempeff : temp�rature efficace journali�re
! *-----------------------------------------------------------------* c
!
! DR introduction des modifs de Sylvain le 23/05/08
! *************************************************
! 15/06/09
! Introduction des modifs de Sylvain Satger
! on a recupere dynamictalle en entier
! *************************************************

subroutine dynamictalle(P_SurfApex,P_SeuilMorTalle,cteloigamma,P_VitReconsPeupl,  &
                        P_SeuilReconsPeupl,P_MaxTalle,densite,transpi,          &
                        transpipot,LAIapex,P_SeuilLAIapex,tempeff,mortalle,   &
                        deltainet,reserve,mortrestalle,deltai,lain,         &
                        densitemax,masec,mortmasec,drlsenmortalle)

  implicit none
  
!: Arguments

  real :: P_SurfApex  !> // PARAMETER // equivalent surface of a transpiring apex // m� // PARAMV6/PLT // 1 
  real :: P_SeuilMorTalle  !> // PARAMETER // relative transpiring threshold to calculate tiller mortality // mm // PARAMV6/PLT // 1 
  real :: cteloigamma  
  real :: P_VitReconsPeupl  !> // PARAMETER // thermal time for the regeneration of the tiller population // nb tillers/degree C/m� // PARAMV6 // 1
  real :: P_SeuilReconsPeupl  !> // PARAMETER // tiller density threshold below which the entire population won't be regenerated // nb tillers/m� // PARAMV6/PLT // 1 
  real :: P_MaxTalle  !> // PARAMETER // maximal density of tillers/m� // Nb tillers/ // PARAMV6/PLT // 1 
  real :: densite      !> // OUTPUT // Actual sowing density // plants.m-2
  real :: transpi  
  real :: transpipot  
  real :: LAIapex  
  real :: P_SeuilLAIapex  !> // PARAMETER // Maximal value of LAI+LAIapex when LAIapex isn't nil // m�/m� // PARAMV6/PLT // 1 
  real :: tempeff      !> // OUTPUT // Efficient temperature for growth // degree C
  real :: mortalle      !> // OUTPUT // number of dead tillers per day // tillers.j-1
  real :: deltainet  
  real :: reserve  
  real :: mortrestalle  
  real :: deltai      !> // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
  real :: lain  
  real :: densitemax  
  real :: masec      !> // OUTPUT // Aboveground dry matter  // t.ha-1
  real :: mortmasec      !> // OUTPUT // Dead tiller biomass  // t.ha-1
  real :: drlsenmortalle      !> // OUTPUT // Root biomass corresponding to dead tillers // t ha-1.j-1

!: Variables locales
  real :: mu  
  real :: seuilsurscale  
  real :: shape_  
  real :: paramgamma  !>  
  real ::  cteloigamma2  


!: Fonction(s)
  real :: gammp  

      
    ! d�finition des param�tres de la loi gamma(shape, seuilsurscale)
      paramgamma = 1
      cteloigamma2 = -999
      if (transpipot > 0.) then
        mu = transpi/transpipot
      else
        return
      endif

    ! mortalit� des talles conditionnelle � la croissance
    ! en LAI relative � l'�tat du peuplement
      if (deltai <= 0.03*(densite/P_Maxtalle) .and. densite > 0.) then

    ! calcul des talles qui meurent en fonction de la loi gamma

      ! FR et SYL 091208 introduction de la possibilit� de choisir un
      ! ET constant ou variant dans le sens de la moyenne ou inversement
        if (paramgamma == 1) then
          shape_ = 1 / (cteloigamma**2)
          seuilsurscale = P_SeuilMorTalle / ((cteloigamma**2) * mu)
        else
          if (paramgamma == 2) then
            shape_ = (mu**(cteloigamma+2)) / cteloigamma2
            seuilsurscale = (P_SeuilMorTalle * (mu**(cteloigamma+1))) / cteloigamma2
          else
            shape_ = (mu**mu) / cteloigamma
            seuilsurscale = (P_SeuilMorTalle * mu) / cteloigamma
          endif
        endif

      ! SYL 060907 prise en compte de la diminution de la biomasse (mortmasec) et de la densit� racinaire
      ! (drlsenmortalle) quand il y a mortalit� d talles
        mortalle = gammp(shape_,seuilsurscale) * densite
        mortrestalle = mortalle * reserve / densite
        mortmasec = mortalle * masec / densite
        drlsenmortalle = mortalle / densite
        densite = densite - mortalle

      else

      ! reconstitution de la population des talles
      ! calcul d'un maximum fonction de la densit� courante
        mortalle = 0.0

      ! SYL 040907
        mortmasec = 0.0
        mortrestalle = 0.0
        drlsenmortalle = 0.0

        if (densite < P_SeuilReconsPeupl) then
          densitemax = min(densitemax, P_MaxTalle*(-8e-7*densite**2+1.9e-3*densite))
        endif

      ! reconstitution
        if (deltainet > 0.03*(densite/P_Maxtalle)) then
          densite = min(densite+P_VitReconsPeupl*densite*tempeff,densitemax)
        endif

      endif
      
    ! calcul du LAI apex
    ! NB et Sylvain 300807
      if (lain > P_SeuilLAIapex) then
        LAIapex = 0
      else
        LAIapex = min((P_SurfApex*densite),(P_SeuilLAIapex-lain))
      endif

return
end subroutine dynamictalle
      

real function gammp(a,x)
      
  implicit none

!: Arguments
  real, intent(IN) :: a  
  real, intent(IN) :: x  

!: Function(s)
  real :: gammcf  !>  
  real :: gamser  !>  
  real :: gln  
      
      ! TODO : fichier historique ? if (x < 0. .or. a <= 0.) pause 'bad arguments in gammp'
      
      if (x < a+1.) then
        call gser(gamser,a,x,gln)
        gammp = gamser
      else
        call gcf(gammcf,a,x,gln)
        gammp = 1.-gammcf
      endif
      
return
end function gammp
!  (C) Copr. 1986-92 Numerical Recipes Software &H1216.


      
      
subroutine gser(gamser,a,x,gln)

  implicit none

!: Arguments
  real, intent(OUT) :: gamser  
  real, intent(IN)  :: a  
  real, intent(IN)  :: x  
  real, intent(OUT) :: gln  

!: Variables locales
  integer :: ITMAX  
  real    :: EPS  

  PARAMETER (ITMAX = 100,EPS = 3.e-7)

  integer :: n  
  real    :: ap  !>  
  real    :: del  !>  
  real    :: somme  

!: Fonction(s)  
  real ::gammln  
  
      gln = gammln(a)
      if (x <= 0.) then
        ! TODO : fichier historique ? : -- if (x < 0.) pause 'x < 0 in gser'
        gamser = 0.
        return
      endif
      
      ap = a
      somme = 1. / a
      del = somme
      do n = 1,ITMAX
        ap = ap + 1.
        del = del * x / ap
        somme = somme + del
        if (abs(del) < abs(somme)*EPS) EXIT
      end do
      
      ! TODO : fichier historique ? : -- if (n >= ITMAX)pause 'a too large, ITMAX too small in gser'
      
      gamser = somme * exp(-x + (a * log(x)) - gln)

return
end subroutine gser
!  (C) Copr. 1986-92 Numerical Recipes Software &H1216.


subroutine gcf(gammcf,a,x,gln)

  implicit none

!: Arguments
  real, intent(OUT) :: gammcf  
  real, intent(IN)  :: a  
  real, intent(IN)  :: x  
  real, intent(OUT) :: gln  
  
!: Variables locales
  integer :: ITMAX  
  real    :: EPS  !>  
  real    :: FPMIN  
  PARAMETER (ITMAX = 100,EPS = 3.e-7,FPMIN = 1.e-30)
  integer :: i  
  real    :: an  !>  
  real    :: b  !>  
  real    :: c  !>  
  real    :: d  !>  
  real    :: del  !>  
  real    :: h  
  
!: Function(s)
  real :: gammln  

      gln = gammln(a)
      b = x + 1. - a
      c = 1. / FPMIN
      d = 1. / b
      h = d
      do i = 1,ITMAX
        an = -i * (i - a)
        b = b + 2.
        d = an * d + b
        if (abs(d) < FPMIN) d = FPMIN
        c = b + an / c
        if (abs(c) < FPMIN) c = FPMIN
        d = 1. / d
        del = d * c
        h = h * del
        if (abs(del - 1.) < EPS) EXIT
      end do
      
      ! TODO : fichier historique ? : -- if (i >= ITMAX) pause 'a too large, ITMAX too small in gcf'
      
      gammcf = exp(-x + (a * log(x)) - gln) * h

return
end subroutine gcf
!  (C) Copr. 1986-92 Numerical Recipes Software &H1216.

      
real function gammln(xx)

  implicit none

!: Arguments
  real, intent(IN) :: xx  
  
!: Variables locales

  integer :: j  
  double precision :: ser  !>  
  double precision :: stp  !>  
  double precision :: tmp  !>  
  double precision :: x  !>  
  double precision :: y  !>  
  double precision :: cof(6)  
  SAVE cof,stp
  DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,24.01409824083091d0,-1.231739572450155d0, &
               0.1208650973866179d-2,-0.5395239384953d-5,2.5066282746310005d0/

      x = xx
      y = x
      tmp = x + 5.5d0
      tmp = (x + 0.5d0) * log(tmp) - tmp
      ser = 1.000000000190015d0
      do j = 1,6
        y = y + 1.d0
        ser = ser + cof(j) / y
      end do
      gammln = real(tmp + log(stp * ser / x))

return
end function gammln
!  (C) Copr. 1986-92 Numerical Recipes Software &H1216.

 
 
