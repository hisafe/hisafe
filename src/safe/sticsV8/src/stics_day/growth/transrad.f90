! **************************************************************** c
! *  version 5.0                                                 * c
! *  derniere modif 14/05/01                                     * c
! **************************************************************** c
! * sous-programme de transfert radiatif                         * c
! * un modèle de transfert radiatif prenant en compte            * c
! * la géométrie du couvert (recommandé pour arbres et           * c
! * cultures principales dans les associations)                  * c
! **************************************************************** c
!> This subroutine calculates the radiation interception, with prediction of light interception dependent not only on LAI, but also on plant height and width,
!> row spacing, plant spacing and direct and diffuse light absorption.
!> - Stics book paragraphe 3.2.2, page 50-51
!>
!! A calculation of radiation transfer enables an estimate of the radiation intercepted by a crop in rows, taking account of its geometry in a simple fashion.
!! The objective is to estimate, on a daily time step, the fraction of radiation intercepted by the crop and fraction part transmitted to the layer below,
!! which can be either the soil or another crop (case of intercropping). To calculate those two components, the soil surface is split into a shaded part and
!! a sunlit part and by convention the shaded part corresponds to the vertical projection of the crop foliage onto the soil surface. The available daily variables
!! are the Leaf Area Index (LAI), calculated independently and the global radiation (trg)
!!
!! The simplest method of calculating the radiation received at a given point X (located on the soil in the inter-row) is to calculate angles H1 and H2
!! corresponding to the critical angles below which point X receives the total radiation directly.  At angles below H1 and above H2, point X receives an amount
!! of radiation below the total radiation value, due to absorption by the crop. Within those angle windows, Beer’s law is used to estimate the fraction of
!! transmitted radiation. It is assumed that a canopy can be represented by a simple geometric shape (rectangle or triangle) and that it is isotropically infinite.
!! We can therefore describe the daily radiation received at point X as the sum of the radiation not intercepted by the crop (rdroit)(sun at an angle between H1 and H2)
!! and the radiation transmitted (rtransmis).  The "infinite canopy" hypothesis allows us to assume that when the sun is at an angle below H1 and H2,
!! all the radiation passes through the crop. Each part of the radiation received at X includes a direct component and a diffuse component.
!! Let us assume that, for the transmitted part, the same extinction coefficient (ktrou) applies to both components (which is generally accepted to be the case
!! when the general Beer law is used with a daily time scale).
!!
!! In contrast, for rdroit, direct and diffuse components should be separated because of the directional character of the direct component, which requires
!! the calculation of separate proportions of radiation reaching the soil (kgdiffus and kgdirect are the proportions of diffuse radiation, rdiffus, and direct
!! radiation, rdirect, respectively, reaching the soil).
!---------------------------------------------------------------
subroutine transrad(P_adfol,lairognecum,laieffcum,P_dfolbas,P_dfolhaut,dfol,rdif,parapluie,raint,P_parsurrg,P_forme,lai,  &
                    laisen,eai, P_interrang,nlax,nsen,P_codlainet,P_hautbase,P_codepalissage,P_hautmaxtec,P_largtec,      &
                    originehaut,hauteur,deltahauteur, P_hautmax,varrapforme,largeur,jul,trg,P_latitude,rombre,rsoleil,    &
               !     P_orientrang,P_ktrou,surfAO,surfAS,ipl,P_nbplantes)
                   P_orientrang,P_ktrou,surfAO,surfAS,ipl)

  implicit none

  real,    intent(IN)    :: P_adfol  !> // PARAMETER // parameter determining the leaf density evolution within the chosen shape // m-1 // PARPLT // 1 
  real,    intent(IN)    :: lairognecum  
  real,    intent(IN)    :: laieffcum  
  real,    intent(IN)    :: P_dfolbas  !> // PARAMETER // minimal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1 
  real,    intent(IN)    :: P_dfolhaut  !> // PARAMETER // maximal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1 
  real,    intent(OUT)   :: dfol   !> // OUTPUT //  "Within the shape  leaf density" // m2 m-3
  real,    intent(OUT)   :: rdif   !> // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1
  integer, intent(OUT)   :: parapluie  
  real,    intent(OUT)   :: raint   !> // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
  real,    intent(IN)    :: P_parsurrg  !> // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // STATION // 1 

  integer, intent(IN)    :: P_forme  !> // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: laisen   !> // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
  real,    intent(IN)    :: eai  
  real,    intent(IN)    :: P_interrang  !> // PARAMETER // Width of the P_interrang // m // PARTEC // 1 
  real,    intent(IN)    :: nlax  
  real,    intent(IN)    :: nsen  
  real,    intent(IN)    :: P_codlainet  !> // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: P_hautbase  !> // PARAMETER // Base height of crop // m // PARPLT // 1 
  integer, intent(IN)    :: P_codepalissage  !> // PARAMETER // option: no (1),  yes2) // code 1/2 // PARTEC // 0 
  real,    intent(IN)    :: P_hautmaxtec  !> // PARAMETER // maximal height of the plant allowed by the management // m // PARTEC // 1 
  real,    intent(IN)    :: P_largtec  !> // PARAMETER // technical width // m // PARTEC // 1 
  real,    intent(IN)    :: originehaut  

  real,    intent(INOUT) :: hauteur   !> // OUTPUT // Height of canopy // m
  real,    intent(INOUT) :: deltahauteur  
  real,    intent(INOUT) :: P_hautmax  !> // PARAMETER // Maximum height of crop // m // PARPLT // 1 
  real,    intent(INOUT) :: varrapforme  

  real,    intent(OUT)   :: largeur   !> // OUTPUT // Width of the plant shape  // m

  integer, intent(IN)    :: jul  
  real,    intent(IN)    :: trg   !> // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
  real,    intent(IN)    :: P_latitude  !> // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0 

  real,    intent(OUT)   :: rombre   !> // OUTPUT // Radiation fraction in the shade // 0-1
  real,    intent(OUT)   :: rsoleil   !> // OUTPUT // Radiation fraction in the full sun // 0-1
  real,    intent(IN)    :: P_orientrang  !> // PARAMETER // Direction of ranks // rd (0=NS) // PARTEC // 1 
  real,    intent(IN)    :: P_ktrou  !> // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1 

  real,    intent(INOUT)   :: surfAO
  real,    intent(INOUT)   :: surfAS

  integer, intent(IN)    :: ipl  
!  integer, intent(IN)    :: P_nbplantes  !> // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0



  real :: calcul_RDif  

!: les VARIABLES LOCALES

  !integer :: jul
  integer :: formetrans  
  real    :: hauteurzero  
  real    :: largtrans  
  real    :: raptrans  
  !real    :: surfAO
  !real    :: surfAS



!: Transferts radiatifs

      ! ** NB le 28/03/02 ajout de EAI
      ! *- NB le 14/05 la densité foliaire varie entre deux bornes
      ! *- NB le 10/10/05 la densité foliaire peut être décroissante
      ! si P_adfol <0
      if (P_adfol > 0.) then
        dfol = P_adfol * (lai + eai + lairognecum + laieffcum + laisen)
      else
        dfol = P_adfol * (lai + eai + lairognecum + laieffcum + laisen) + P_dfolbas + P_dfolhaut
      endif
      if (dfol < P_dfolbas) dfol = P_dfolbas
      if (dfol > P_dfolhaut) dfol = P_dfolhaut

      call formplante(P_forme,lai,laisen,eai,P_interrang,nlax,nsen,P_codlainet,P_hautbase,P_codepalissage,P_hautmaxtec,  &
                      P_largtec,originehaut, &
                      hauteur,deltahauteur,P_hautmax,varrapforme,dfol,largeur,formetrans,raptrans,largtrans,hauteurzero)

      !: Calcul de rdif : diffus/global
      rdif =  calcul_RDif(trg,P_latitude,jul)

      !: Calcul du rayonnement transmis
      call rtrans(rombre,rsoleil,largtrans,P_latitude,jul,raptrans,P_interrang,formetrans, &
                  hauteurzero,P_orientrang,rdif,P_ktrou,lai,eai)

      !: Calcul des surfaces à l'ombre et au soleil
      if (ipl == 1) then
        surfAO = largeur / P_interrang
        surfAO = min(surfAO,1.0)
        surfAS = 1.0 - surfAO
        if (surfAS <= 0.0) then
           surfAS = 0.001
           surfAO = 1.0 - surfAS
        endif

        !: Il y a un effet parapluie pour interception de la pluie
        parapluie = 1

      else
        parapluie = 0
      endif

      !: Calcul du rayonnement intercepté
      raint = P_parsurrg * trg * (1 - (rombre * surfAO) - (rsoleil * surfAS))

      !- NB le 19/02/03
      if (raint < 0.0) raint = 0.0

return
end subroutine transrad


!----------------------------------------------------------------
!  function permettant de calculer le rayonnement transmis
!  sous la culture dominante en un point quelconque entre
!  deux rangs
!  ir = P_interrang (m)
!  l = largeur du houppier
!  e = epaisseur du houppier
!  rap = rapport e/l
!  x = position du point par rapport au rang le plus proche (m)
!  haut = hauteur de la base du houppier (m)
!  P_forme = 1 pour rectangle et 2 pour triangle
!  si rap<0, triangle à l'envers
!  alpha = angle des rangs avec le nord (radian)
!  rdif = rapport diffus/total
!  dfol = densité folaire (m3/m2)
!

subroutine rtrans(rombre,rsoleil,l,P_latitude,j,rap,ir,P_forme,haut,alpha,rdif,P_ktrou,lai,eai)

  implicit none

!: Arguments
  real,    intent(OUT)   :: rombre   !> // OUTPUT // Radiation fraction in the shade // 0-1
  real,    intent(OUT)   :: rsoleil   !> // OUTPUT // Radiation fraction in the full sun // 0-1
  real,    intent(INOUT) :: l  
  real,    intent(IN)    :: P_latitude  !> // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0 
  integer, intent(IN)    :: j  
  real,    intent(IN)    :: rap  
  real,    intent(IN)    :: ir  
  integer, intent(INOUT) :: P_forme  !> // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: haut  
  real,    intent(IN)    :: alpha  
  real,    intent(IN)    :: rdif   !> // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1
  real,    intent(IN)    :: P_ktrou  !> // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1 
  real,    intent(IN)    :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: eai  


!: Variables locales
  real :: rtransmis(60)  
  real :: lat  
  real :: x  
  real :: rg  
  real :: rdirect  
  real :: kgdiffus  
  real :: kgdirect  
  real :: rdroit  
  real :: pi  
  real :: htab(23)  
  real :: aztab(23)  
  real :: SOCtab(23)  
  real :: interval  

  real :: xprec  
  real :: teta1  
  real :: teta2  
  integer :: ilim  
  integer :: i  

  !: Tableau d'Hervé Sinoquet donnant pour 23 directions hauteur, azimuth
  !- la proportion de diffus selon le standard SOC
  data htab/5*9.23,2*10.81,3*26.57,5*31.08,3*47.41,2*52.62,3*69.16/
  data aztab/12.23,59.77,84.23,131.77,156.23,36,108,0,72,144,23.27,48.73,95.27,120.73,167.27,0,72,144,36,108,0,72,144/
  data SOCtab/5*0.0043,2*0.0055,3*0.0140,5*0.0197,3*0.0336,2*0.0399,3*0.0495/

   ! 5 format(i3,3f7.2,i3,3f7.2,i3,5f7.2)

      !: Calcul de la discretisation de l'P_interrang
      if (ir < 1.0) then
        interval = 200.
      else
        interval = 20.
      endif

      !: Nb - le 19/04 - Bug quand l > ir/2
      if (l > ir/2.) l = ir/2.

      rg = 1.0
      rdirect = rg - rdif
      pi = 4 * atan(1.0)
      !: Changements d'unités
      lat = P_latitude / 180 * pi

      !: Boucle sur la position dans l'P_interrang
      xprec = 0.0
      ilim = 0
      !: domi - 04/01/2005 - y'a un soucis avec le depassement de tableau. On enlève le +1
      ! --      do 20 i = 1,nint(ir/2*interval)+1
      do i = 1,nint(ir / 2 * interval)
        x = (i-1) / interval
        if (xprec <= l/2 .and. x >= l/2) ilim = i
        xprec = x

        !: diffus
        call kdif(kgdiffus,htab,aztab,SOCtab,x,haut,rap,l,ir)

        !: direct
        !- TODO: teta1 et teta2 sont des variables de sorties de kgeom qui ne sont utilisées nulle part.
        !-       Faut-il les garder en tant que sortie de kgeom ? Ou bien les transformer en variable locales de kgeom ?
        call kgeom(kgdirect,lat,j,l,rap,x,ir,P_forme,haut,alpha,teta1,teta2)

        rdroit = (kgdiffus * rdif) + (kgdirect * rdirect)
        rtransmis(i) = (1.0 - rdroit) * (exp(-P_ktrou * (lai + eai)))
        rtransmis(i) = rtransmis(i) + rdroit

        !: Pour Denis
        !--     call photpd(P_latitude,j,daylen,phoi)
        !--     xxx = exp(-P_ktrou*lai)
        !--     write(15,12)j,x,rtransmis(i),12-teta1/pi*daylen,12+teta2/pi*daylen
        !--    s,xxx
        !--  12 format(i3,5f8.3)
        !--     write(3,21) lai,x,kgdiffus,kgdirect,rtransmis(i),rdroit
        !--  21 format(6f7.2)

      end do

      !: Moyennes à l'ombre et au soleil
      rombre = 0.0
      rsoleil = 0.0

      do i = 1,nint(ir / 2 * interval)
        if (i <= ilim) then
          rombre = rombre + rtransmis(i)
        else
          rsoleil = rsoleil + rtransmis(i)
        endif
      end do

      if (ilim == 0.) then
        rombre = 0.0
      else
        rombre = rombre / ilim
      endif
      if (ilim < nint(ir / 2 * interval) + 1) then
          rsoleil = rsoleil / (nint(ir / 2.0 * interval) + 1.0 - ilim)
      endif

return
end subroutine rtrans




!---------------------------------------------------------------------------
!  Function Kgeom
!  proportion de rayonnement direct reçu sous la
!  culture dominante
subroutine kgeom(kg,lat,j,l,rap,x,ir,P_forme,haut,alpha,teta1,teta2)

  implicit none

  !: Arguments
  real,    intent(OUT)   :: kg  
  real,    intent(IN)    :: lat  
  integer, intent(IN)    :: j  
  real,    intent(IN)    :: l  
  real,    intent(IN)    :: rap  
  real,    intent(INOUT) :: x  
  real,    intent(IN)    :: ir  
  integer, intent(INOUT) :: P_forme  !> // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: haut  
  real,    intent(IN)    :: alpha  
  real,    intent(OUT)   :: teta1  
  real,    intent(OUT)   :: teta2  

  !: Fonction externe
  real :: tetacrit  

  !: Variables locales
  real :: tgh  
  real :: pi  
  real :: limite  
  real :: limite2  
  real :: e  

      pi = 4 * atan(1.0)
      x = min(x,ir/2)
      ! x = 0 - ca pose pb
      e = abs(l * rap)
      limite = l / 2
      if (e > 0.0) then
        limite2 = l / 2 * (haut / e+1)
      else
        P_forme = 1
      endif

      !: Cas du rectangle(P_forme = 1)
      if (P_forme == 1) then
        tgh = (haut + e) / (ir - x - limite)
        teta1 = tetacrit(lat,j,tgh,alpha)
        if (x > limite) then
          tgh = (haut + e) / (x - limite)
          teta2 = tetacrit(lat,j,tgh,alpha)
        endif
        if (x < limite) then
          tgh = haut / ( -x + limite)
          teta2 = -tetacrit(lat,j,tgh,alpha)
        endif
        if (x == limite) teta2 = 0
      endif

      !: Cas du triangle à l'envers
      if (P_forme == 2 .and. rap < 0.) then
        tgh = (haut + e) / (ir - x - limite)
        teta1 = tetacrit(lat,j,tgh,alpha)
        if (x > limite) then
          tgh = (haut + e) / (x - limite)
          teta2 = tetacrit(lat,j,tgh,alpha)
        endif
        if (x < limite) then
          tgh = (haut + e) / ( x - limite)
          teta2 = -tetacrit(lat,j,tgh,alpha)
        endif
        if (x == limite) teta2 = 0
      endif

      ! cas du triangle à l'endroit
      if (P_forme == 2 .and. rap > 0.) then
        tgh = (haut + e) / (ir - x - limite)
        teta1 = tetacrit(lat,j,tgh,alpha)
        if (x < limite2) then
          if (x > limite) then
            tgh = haut / ( x - limite)
            teta2 = tetacrit(lat,j,tgh,alpha)
          endif
          if (x < limite) then
            tgh = haut / (limite - x)
            teta2 = -tetacrit(lat,j,tgh,alpha)
          endif
          if (x == limite) teta2 = 0.0
        endif
        if (x >= limite2) then
          tgh = (haut + e) / x
          teta2 = tetacrit(lat,j,tgh,alpha)
        endif
      endif

      ! Quel interet puisqu'on réaffecte kg 2 lignes en dessous ?
      if (e > 2.1e-2) then
        kg = cos(teta1)
        kg = cos(teta2)
      endif

      kg = 0.5 * (cos(pi/2 + teta1) + cos(pi/2 + teta2))
      kg = max(kg,0.0)

return
end subroutine kgeom



!---------------------------------------------------------------------------
!  calcul du cos de l'angle teta correspondant à la hauteur
! apparente du soleil h (tangente de l'angle en radian)
real function tetacrit(lat,j,tgh,alpha)

  USE Divers, only: decangle
  implicit none

  !: Arguments
  real,    intent(IN) :: lat  
  integer, intent(IN) :: j  
  real,    intent(IN) :: tgh  
  real,    intent(IN) :: alpha  


  !: Variables locales
  real :: teta(180)  
  real :: sinh  
  real :: h  
  real :: hcrit  
  real :: a  
  real :: b  
  real :: acrit  
  real :: bcrit  
  real :: dec  
  real :: pi  
  integer :: n  
  integer :: i  
  real :: hprec  
  real :: cosazim  
  real :: azim  
  real :: hcritprec  


!   initialisations
      acrit = 0.0
      bcrit = 0.0
      a = 0.0
      b = 0.0
      tetacrit = 0.0
      n  = 3
      ! 12/04/2012 je mets l'appel en minuscule pour que ce soit pareil que la declaration
      dec = decangle(j)
      hprec = 0.0
      pi = 4 * atan(1.0)

      do i = 1, 18 * n
        teta(i) = 10. / n * (i - 1)
        teta(i) = (teta(i) - 90) / 180 * pi
        !: position du soleil (h,azim)
        sinh = sin(lat) * sin(dec) + cos(lat) * cos(dec) * cos(teta(i))
        h = asin(sinh)
        cosazim = (-cos(lat) * sin(dec) + sin(lat) * cos(dec) * cos(teta(i))) / cos(h)
        !: PB - 20/12/2004 - pour éviter les plantages sur acos(cosazim)
        cosazim = min(1.0,cosazim)
        if (teta(i) /= 0.0) then
          azim = acos(cosazim) * teta(i) / abs(teta(i))
        else
          azim = 0.0
        endif
        if (sinh < 0.0) h = 0.0

        ! hauteur critique
        hcrit = atan(tgh * abs(sin(azim + alpha + 0.00001)))
        ! test pour que h = hcrit
        if (hcritprec >= hprec .and. hcrit <= h .and. i > 1) then
          ! interpolation linéaire
          acrit = (hcrit - hcritprec) / (teta(i) - teta(i-1))
          bcrit = hcrit - acrit * teta(i)
          a = (h - hprec) / (teta(i) - teta(i-1))
          b = h - a * teta(i)
          ! ** attention division par zéro à corriger - NB - le 05/03/02
          if (a /= acrit) tetacrit = (b - bcrit) / (acrit - a)
          return
        endif

        hcritprec = hcrit
        hprec = h
      end do

return
end function tetacrit




!---------------------------------------------------------------------------
!  Function Kdif
!  proportion de rayonnement diffus reçu sous la
!  culture dominante
subroutine kdif (kgdiffus,htab,aztab,SOCtab,x,haut,rap,l,ir)

  implicit none

!: Arguments
  real, intent(IN)    :: rap  
  real, intent(INOUT) :: x ! a priori, on pourrait passer en INTENT(IN) mais il faudrait alors passer par une variable locale intermédiaire
  real, intent(IN)    :: haut  
  real, intent(IN)    :: ir  
  real, intent(IN)    :: l  

  real, intent(OUT)   :: kgdiffus  
  real, intent(IN)    :: htab(23)  
  real, intent(IN)    :: aztab(23)  
  real, intent(IN)    :: SOCtab(23)  


!: Variables locales
  real    :: limite  
  integer :: i  
  real    :: pi  
  real    :: e  
  real    :: G  
  real    :: hcrit  

      pi = 4 * atan(1.0)
      x = min(x,ir/2)
! correction bug e = l*rap
      e = abs(l*rap)
      limite = l/2.
      kgdiffus = 0.

! pour le rang de droite

      G = (haut+e)/(ir-x-l/2.)
      do i = 1,23
        hcrit = atan(G*sin(aztab(i)/180*pi))/pi*180
        if (hcrit < htab(i)) kgdiffus = kgdiffus+SOCtab(i)
      end do

! pour le rang de gauche uniquement
! si le point ne se trouve pas sous le houppier

      if (x > l/2) then
        G = (haut+e)/(x-l/2)
        do i = 1,23
          hcrit = atan(G*sin(aztab(i)/180*pi))/pi*180
          if (hcrit < htab(i)) kgdiffus = kgdiffus+SOCtab(i)
        end do
      endif

return
end subroutine kdif




! ------------------------------------------------- c
! * calcul de rdif : diffus/global                * c
! * rapport global/global extra-terrestre         * c
! ------------------------------------------------- c
real function calcul_RDif(rg,P_latitude,jul)

  USE Divers, only: RGEX

  implicit none

!: Arguments
  integer, intent(IN) :: jul  
  real,    intent(IN) :: rg  
  real,    intent(IN) :: P_latitude  !> // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0 

!: Variable locale
  real :: RsRso  !>  
  real ::  rdif   !> // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1



      RsRso = rg / RGEX(P_latitude / 180 * 3.14156, jul)

      !: A priori cette ligne est inutile
      !-- if (RsRso > 0.76) RsRso = 0.76

      ! rapport diffus/global  (Spitters et al 1986 AFM 38 : 217-229)
      if (RsRso < 0.07)  rdif = 1.0
      if (RsRso >= 0.07) rdif = 1.0 - (2.3 * (RsRso - 0.07)**2)
      if (RsRso > 0.35)  rdif = 1.33 - (1.46 * RsRso)
      if (RsRso > 0.75)  rdif = 0.23

      calcul_RDif = rdif

return
end function calcul_RDif



! ---------------------------------------------------------

! ---------------------------------------------------------
subroutine formplante(P_forme,lai,laisen,eai,P_interrang,nlax,nsen,P_codlainet,P_hautbase,P_codepalissage,P_hautmaxtec,P_largtec, &
                      originehaut, hauteur,deltahauteur,P_hautmax,varrapforme,dfol,largeur,formetrans,raptrans,largtrans,         &
                      hauteurzero)

  implicit none

!: Arguments
  integer, intent(IN)    :: P_forme  !> // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  real,    intent(IN)    :: laisen   !> // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
  real,    intent(IN)    :: eai  
  real,    intent(IN)    :: P_interrang  !> // PARAMETER // Width of the P_interrang // m // PARTEC // 1 
  real,    intent(IN)    :: nlax  
  real,    intent(IN)    :: nsen  
  real,    intent(IN)    :: P_codlainet  !> // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
  real,    intent(IN)    :: P_hautbase  !> // PARAMETER // Base height of crop // m // PARPLT // 1 
  integer, intent(IN)    :: P_codepalissage  !> // PARAMETER // option: no (1),  yes2) // code 1/2 // PARTEC // 0 
  real,    intent(IN)    :: P_hautmaxtec  !> // PARAMETER // maximal height of the plant allowed by the management // m // PARTEC // 1 
  real,    intent(IN)    :: P_largtec  !> // PARAMETER // technical width // m // PARTEC // 1 
  real,    intent(IN)    :: originehaut  

  real,    intent(INOUT) :: hauteur   !> // OUTPUT // Height of canopy // m
  real,    intent(INOUT) :: deltahauteur  
  real,    intent(INOUT) :: P_hautmax  !> // PARAMETER // Maximum height of crop // m // PARPLT // 1 
  real,    intent(INOUT) :: varrapforme  
  real,    intent(INOUT) :: dfol   !> // OUTPUT //  "Within the shape  leaf density" // m2 m-3

  real,    intent(OUT)   :: largeur   !> // OUTPUT // Width of the plant shape  // m
  integer, intent(OUT)   :: formetrans  
  real,    intent(OUT)   :: raptrans  
  real,    intent(OUT)   :: largtrans  
  real,    intent(OUT)   :: hauteurzero  

!: Variables locales
  real :: enouv  
  real :: tmp1  
  real :: tmp2  
  real :: hauteurjour  


      !: Calcul de la largeur en fonction de lai
      !- de la P_forme et de la densité foliaire
      if (P_forme == 1) then
        tmp1 = lai + laisen + eai
        tmp2 = dfol * varrapforme
        largeur = sqrt(tmp1*P_interrang/tmp2)
      else
        tmp1 = lai + laisen + eai
        tmp2 = dfol * abs(varrapforme)
        largeur = sqrt(2*tmp1*P_interrang/tmp2)
      endif

! *- NB - le 23/11
      if (nlax > 0 .and. nsen == 0) then
        if (P_codlainet == 1) hauteur = hauteur + deltahauteur
      else
        if (nsen == 0) then
          hauteurjour = P_hautbase + largeur * abs(varrapforme)
          deltahauteur = hauteurjour - hauteur
          hauteur = hauteurjour
        endif
      endif


! NB le 01/07/05 introduction d'une hauteur et largeur techniques maximales
! ** ML le 29/05/07 les largeur et hauteur techniques maximales ne sont
! *- prises en compte que dans le cas où il y a palissage (P_codepalissage = 2)
      if (P_codepalissage == 2) then
        P_hautmax = min(P_hautmax,P_hautmaxtec)
      endif

      !: ML le 29/05/07
      !: Limitation de la largeur par P_largtec sans limitation de hauteur
      if (hauteur < P_hautmax) then
        if (P_codepalissage /= 2 .or. (P_codepalissage == 2 .and. largeur < P_largtec)) then
          varrapforme = sign(1.0,varrapforme) * (hauteur - P_hautbase) / largeur
        endif
        if (P_codepalissage == 2 .and. largeur >= P_largtec) then
          largeur = P_largtec
          varrapforme = sign(1.0,varrapforme) * (hauteur - P_hautbase) / largeur
        endif
      endif

      !: Limitation de la hauteur par P_hautmax sans limitation de largeur
      if (hauteur >= P_hautmax) then
        hauteur = P_hautmax
        if (P_codepalissage /= 2 .or. (P_codepalissage == 2 .and. largeur < P_largtec)) then
          varrapforme = sign(1.0,varrapforme) * (hauteur - P_hautbase) / largeur
        endif

        !: Limitation de la hauteur par P_hautmax et de la largeur par P_largtec
        !
        !- dans ce cas il faut non seulement calculer varrapforme, mais également
        !- recalculer dfol afin qu'il soit cohérent avec P_hautmaxtec et P_largtec en sortie
        !- sachant que dans les 2 cas précédents (soit la hauteur, soit la largeur est limitée)
        !- la densité foliaire dfol avait toujours la possibilité de compenser
        !- un manque de place en hauteur par un remplissage en largeur ou inversement
        if (P_codepalissage == 2 .and. largeur >= P_largtec) then
          largeur = P_largtec
          varrapforme = sign(1.0,varrapforme) * (hauteur - P_hautbase) / largeur
          if (P_forme == 1) then
            dfol = tmp1 * P_interrang / (largeur**2 * varrapforme)
          else
            dfol = 2 * tmp1 * P_interrang / (largeur**2 * varrapforme)
          endif
        endif
      endif

      hauteurzero = P_hautbase-originehaut


      formetrans = P_forme
      if (hauteurzero < 0.0) then

        !: Cas des cultures en dont les feuillages se mélangent
        enouv = largeur * abs(varrapforme) + hauteurzero

        !: Cas du rectangle
        if (P_forme == 1) then
          raptrans = enouv / largeur
          largtrans = largeur
        endif

        !: Cas du triangle à l'endroit
        if (P_forme == 2 .and. varrapforme > 0.0) then
          raptrans = varrapforme
          largtrans = enouv / raptrans
        endif

        !: Cas du triangle à l'envers qui "devient" rectangle au
        !- dessus de la culture associée
        if (P_forme == 2 .and. varrapforme < 0.0) then
          largtrans = largeur
          raptrans = enouv / largtrans
          formetrans = 1
        endif

        hauteurzero = 0.001

      else
        raptrans = varrapforme
        largtrans = largeur
      endif

return
end subroutine formplante
 
 
