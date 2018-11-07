!***************************************************************
! calcul de l'absorption d'azote par le principe offre/demande
! demande = courbe de dilution "maximale"
! offre = cumul sur le profil de sol de l'offre élémentaire
!         au niveau racinaire estimée comme le minimum entre
!         l'offre physique (migration du nitrate du sol à la
!         racine) et la capacité d'absorption (double MM)
!
! fluxrac : flux d'absorption maximal possible par couche  de sol
! fluxsol : flux de transfert sol --> racine   par couche  de sol
! flurac  : flux cumulé d'absorption sur l'ensemble du profil
! flusol  : flux cumulé sol --> racine sur l'ensemble du profil
! offrN     = minimum(fluxsol, fluxrac)
! cumoffrN  = cumul de l'offre sur l'ensemble du profil
! masecdil  = matière sèche déterminant les besoins (t/ha)
! P_masecNmax = matière sèche à laquelle la dilution d'azote commence
! QNplante  = quantité d'azote dans la plante (kg/ha)
! NC        = concentration critique en azote
! CNplante  = concentration d'azote dans la plante
! inn       = indice de satisfaction des besoins azotés
! inns      = inn borné à 1
!****************************************************************
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!! This module calculates Nitrogen deficiency.
!> - Stics book paragraphe 3.4.2, page 60
!>
!! The nitrogen status of a crop can be characterized using the concept of 'dilution curves' which relate the N concentration in plant shoots to
!! the dry matter accumulated in them (Lemaire and Salette, 1984; Greenwood et al., 1991). For a given species, a 'critical dilution curve' can be defined,
!! which can be used to make a diagnosis of nitrogen nutrition (Justes et al., 1994; Lemaire et Gastal., 1997): plants below this curve are or have been
!! N deficient, whereas plants above the curve have an optimal growth, i.e. are not limited by nitrogen. The critical dilution curve is the basis for defining
!! a nitrogen nutrition index (inn) which is the ratio of the actual nitrogen concentration (CNplante, in % of dry matter) to the critical concentration (NC)
!! corresponding to the same biomass (masecabso, in t ha-1).
!!
!! However we are aware of an important limitation in the inn dynamics, as for example in the case of the nitrogen reserve available in perennial organs
!! (e.g. grapevine). Consequently we propose an alternative stress variable corresponding to the nitrogen input flux relative to the critical one as
!! proposed by Devienne-Barret et al. (2000). It is a kind of instantaneous inn named inni relying on the daily accumulations of nitrogen (VabsN) and
!! nitrogen dependent biomass (deltabso).
!!
!! All nitrogen stress indices accept INNmin or INNimin as the floor value for respectively the “INN” and the “INNI” options.
!! By definition the inns index corresponds to the inn between INNmin and 1.  The innlai and innsenes indices are defined by point [1,1] and by
!! points [INNmin, innturgmin] and [INNmin, innsen], respectively.
!! Such a parameterisation allows the effect of nitrogen deficiency on photosynthesis to be differentiated from that on leaf expansion.
!! In practice it seems that these two functions react very similarly and innturgmin is similar to INNmin, while innsen is greater, indicating that the
!! plants accelerate their senescence later than their growth decrease, just as for water stress. A commonly accepted value for INNmin is 0.3 and
!! INNimin is 0.0.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!

subroutine stressN(masecdil,abso,offrenod,QNplante_prec,dltaremobilN,P_codelegume,P_masec0,P_adilmax, & ! IN
                   P_masecNmax,P_bdilmax,masecpartiel,magrain,absodrp,P_codeplisoleN,P_masecmeta,adilI, &
                   bdilI,P_adil,P_bdil,adilmaxI,bdilmaxI,dNdWcrit,deltabso,P_codeINN,P_INNmin,P_INNimin,  &
                   P_innturgmin,P_innsen,P_QNpltminINN,                                               &
                   QNplante,Qfix,QNplanteres,CNplante,inn,inni,inns,innlai,innsenes,hisafeInfluence)              ! INOUT

  implicit none

!: Arguments

  real,    intent(IN)    :: masecdil  
  real,    intent(IN)    :: abso      !> // OUTPUT // Nitrogen absorption rate by plant  // kg N ha-1
  real,    intent(IN)    :: offrenod      !> // OUTPUT // Amount of fixed nitrogen by nodules // kg.ha-1.j-1
  real,    intent(IN)    :: QNplante_prec  
  real,    intent(IN)    :: dltaremobilN  
  integer, intent(IN)    :: P_codelegume  !> // PARAMETER // 1 when the plant  id a legume crop, or 2 // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: P_masec0  !> // PARAMETER // initial biomass // t ha-1 // INIT // 1 
  real,    intent(IN)    :: P_adilmax  !> // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // N% MS // PARPLT // 1 
  real,    intent(IN)    :: P_masecNmax  !> // PARAMETER // Aerial biomass  on and after there is nitrogen dilution (critical and maximal curves) // t ha-1 // PARPLT // 1 
  real,    intent(IN)    :: P_bdilmax  !> // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // SD // PARPLT // 1 
  real,    intent(IN)    :: masecpartiel  
  real,    intent(IN)    :: magrain  
  real,    intent(IN)    :: absodrp  
  integer, intent(IN)    :: P_codeplisoleN  !> // PARAMETER // code for N requirement calculations at the beginning of the cycle: dense plant population (1), isolated plants (2, new formalisation) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: P_masecmeta  !> // PARAMETER // biomass of the plantlet supposed to be composed of metabolic nitrogen // t ha-1 // PARPLT // 1
  real,    intent(IN)    :: adilI  
  real,    intent(IN)    :: bdilI  
  real,    intent(IN)    :: P_adil  !> // PARAMETER // Parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // N% MS // PARPLT // 1 
  real,    intent(IN)    :: P_bdil  !> // PARAMETER // parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // SD // PARPLT // 1 
  real,    intent(IN)    :: adilmaxI  
  real,    intent(IN)    :: bdilmaxI  
  real,    intent(IN)    :: dNdWcrit  
  real,    intent(IN)    :: deltabso  
  integer, intent(IN)    :: P_codeINN  !> // PARAMETER // option to compute INN: cumulated (1), instantaneous (2)  // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: P_INNmin  !> // PARAMETER // Minimum value of INN authorised for the crop // SD // PARPLT // 1 
  real,    intent(IN)    :: P_INNimin  !> // PARAMETER // INNI (instantaneous INN) corresponding to P_INNmin // SD // PARPLT // 1 
  real,    intent(IN)    :: P_innturgmin  !> // PARAMETER // parameter of the nitrogen stress function active on leaf expansion (INNLAI), that is a bilinear function passing by the point of coordinate (P_innmin, P_innturgmin) // SD // PARPLT // 1 
  real,    intent(IN)    :: P_innsen  !> // PARAMETER // parameter of the nitrogen stress function active on senescence (innnsenes), bilinear function of the INN using the point (P_innmin, P_innsen) // SD // PARPLT // 1 
  real,    intent(IN)    :: P_QNpltminINN  !> // PARAMETER // minimal amount of nitrogen in the plant allowing INN computing // kg ha-1 // PARAM // 1 

  !!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE (extaction eau, azote et stress)
  integer, intent(IN)    :: hisafeInfluence      !>  //INPUT // Hisafe influence on water and nitrogen (1=yes 0=no)

  real,    intent(INOUT) :: QNplante      !> // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1
  real,    intent(INOUT) :: Qfix      !> // OUTPUT // Quantity of nitrogen fixed by nodosities // kg.ha-1
  real,    intent(INOUT) :: QNplanteres  
  real,    intent(INOUT) :: CNplante      !> // OUTPUT // Nitrogen concentration of entire plant  // %
  real,    intent(INOUT) :: inn      !> // OUTPUT // Nitrogen nutrition index (satisfaction of nitrogen needs ) // 0-1
  real,    intent(INOUT) :: inni  
  real,    intent(INOUT) :: inns      !> // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
  real,    intent(INOUT) :: innlai      !> // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin to 1
  real,    intent(INOUT) :: innsenes      !> // OUTPUT // Index of nitrogen stress active on leaf death // P_innmin to 1


!: Variables locales
  real :: masecabso  !>  
  real :: NC  !>  
  real :: NCmax  !>  
  real :: VabsN  !>  
  real :: dNdWc  !>  
  real :: dQNc  


      if (masecdil <= 0.0) return

!  write(245,*)'stressN',masecdil,abso,offrenod,QNplante_prec,dltaremobilN,P_codelegume,P_masec0,P_adilmax, & ! IN
!                   P_masecNmax,P_bdilmax,masecpartiel,magrain,absodrp,P_codeplisoleN,P_masecmeta,adilI, &
!                   bdilI,P_adil,P_bdil,adilmaxI,bdilmaxI,dNdWcrit,deltabso,P_codeINN,P_INNmin,P_INNimin,  &
!                   P_innturgmin,P_innsen,P_QNpltminINN,                                               &
!                   QNplante,Qfix,QNplanteres,CNplante,inn,inni,inns,innlai,innsenes




      ! *-------------------------------------------------------------------* c
      ! * Calcul des teneurs en azote et des indices de nutrition azotée * c
      ! *-------------------------------------------------------------------* c
      ! variable VabsN nécessaire pour INNinstantané NB le 07/04/05
      ! domi 16/09/05 affrenod,Qfix passés en AO/AS
      vabsN = abso + offrenod


      QNplante = QNplante_prec + VabsN


      ! ajout des remobilisations azotées venant des réserves
      QNplante = QNplante + dltaremobilN


      if (P_codelegume == 2) then
        Qfix = Qfix + offrenod
      endif

      ! ** quantité minimale d'azote = azote contenu dans les grains
      ! --     CNsemence = 2.0
      ! --     QNplantemin = CNsemence*P_pgrainmaxi*densite*10000/1000/100
      ! --     QNplante = max(QNplante,QNplantemin)

      ! ** pour les cultures plantées (P_masec0>0) il faut ajouter QNplanteres   NB le 23/4/98
      QNplanteres = P_masec0 * P_adilmax * 10. * P_masecNmax**(-P_bdilmax)

      CNplante = QNplante / (masecpartiel * 10.)

      ! ** bornage de CNplante pour ecriture
      if (CNplante > P_adilmax) then
        CNplante = P_adilmax
      endif

      ! ** Bruno - aout2003
      ! --    if (CNplante > dNdWmax) then
      ! --      CNplante = dNdWmax
      ! --      QNplante = CNplante * masecpartiel*10.
      ! --    endif

      ! ** concentration critique en azote (NC)
      masecabso = masecdil - (magrain / 100.)  + (absodrp * magrain / 100.)
      if (masecabso <= P_masecNmax) then

        if (P_codeplisoleN == 2) then
          if (masecabso <= P_masecmeta) masecabso = P_masecmeta
          NC = adilI * masecabso**(-bdilI)
          NCmax = adilmaxI * masecabso**(-bdilmaxI)
          dNdWc = 10. * adilI * (1. - bdilI) * masecabso**(-bdilI)
        else
          NC = dNdWcrit
          NCmax = P_adilmax * P_masecNmax**(-P_bdil)
          dNdWc = 10. * P_adil * (1. - P_bdil) * P_masecNmax**(-P_bdil)
        endif
      else
        NC = P_adil * masecabso**(-P_bdil)
        NCmax = P_adilmax * masecabso**(-P_bdil)
        dNdWc = 10. * P_adil* (1. - P_bdil) * masecabso**(-P_bdil)
      endif

      ! ** indice de nutrition azotée et indice de stress azoté cumulé : INN
     !!! if (hisafeInfluence == 0) then
          if (NC > 0.) then
            inn = CNplante / NC
            inn = min(inn, NCmax / NC)
          else
            inn = 1.
          endif
     !!! end if



      ! ** indice de stress azoté instantané INNi le 07/04/05
      ! calcul de la vitesse d'absorption critique pour l'INNi
      dQNc =  deltabso * dNdWc
      if (dQNc > 0.) then
        inni = VabsN / dQNc
      else
        inni = 1.0
      endif

      ! ** choix de l'inn actif et calcul de inns qui agit sur RUE   (E. Justes)
      if (P_codeINN == 1) then
        inns = min(1., inn)
      else
        inns = min(1., inni)
        inns = (1. - P_INNmin) / (1. - P_INNimin) * inns + ((P_INNimin - P_INNmin) / (P_INNimin - 1.))

      endif
      inns = max(P_INNmin, inns)

      !: Calcul de innlai qui agit sur croissance foliaire
      innlai = (P_innturgmin - 1.) / (P_INNmin - 1.) * inns + (1. - (P_innturgmin - 1.) / (P_INNmin - 1.))


      innlai = min(1., innlai)
      innlai = max(P_INNmin, innlai)


!  write(245,*)'***',innlai,P_innturgmin,P_INNmin,inns

      !: Calcul de innsenes qui agit sur senescence foliaire
      innsenes = (P_innsen - 1.) / (P_INNmin - 1.) * inns + (1. - (P_innsen - 1.) / (P_INNmin - 1.))
      innsenes = min(1., innsenes)
      innsenes = max(P_INNmin, innsenes)

      !: Pour éviter l'instabilité de l'inn au départ
      if (QNplante < P_QNpltminINN) then
        inns     = 1.
        innlai   = 1.
        innsenes = 1.
      endif

return
end subroutine stressN
 
 
