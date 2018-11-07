! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> This module calculates the crop N demand.
!> - Stics book paragraphe 8.6.1, page 157-159
!>
!! If NMAX is the maximal crop nitrogen content and written as a function of plant biomass (W that can be slightly different from masec), the daily N demand
!! (demande, in kg N ha-1 day-1) is the product of the crop growth rate (dltams, in t ha-1 day-1) and the derivative of NMAX relative to W.
!! In STICS the expression of NMAX varies as a function of 2 criteria: the density of the canopy and the presence of storage organs; the first one defining
!! the parameters of the NMAX=f(W) curves (according to Lemaire and Gastal, 1997, or Justes et al., 1997) and the second one defining W.
!> - The dilution curves
!!
!!   Two curves define the critical response function: one for low biomass corresponding to isolated plants and one for high biomass with dense canopies.
!!   Similarly, two curves can be derived to characterize the N demand of these two populations. These 4 curves can be described by similar power functions.
!!   In addition to the prescribed parameters adil and bdil, the other parameters are obtained using the following assumptions:
!>   -   There is a value of metabolic-N concentration (nmeta) corresponding to the plantlet nitrogen content that is composed of functional organs only.
!!       This value is a function of species metabolism: 6.47% for C3 crops (e.g. wheat) and 4.8% for C4 crops (e.g. maize) (Justes et al., 1997; Lemaire and Gastal, 1997).
!>   -   It is possible to define an arbitrary biomass for this plantlet status (masecmeta = 0.04 t ha-1; Justes et al., 1997).
!>   -   It is possible to define experimentally the biomass value at the intersection of the two curves that depends on the form of the canopy (masecdil) and
!!       at this point the reserve N content is Nres.
!!   The curvature of the maximal curve is the same than that of the critical curve for dense canopy: bdilmax=bdil
!> - The presence of storage organs
!!
!!   The N demand due to vegetative organs is assumed to follow the maximal dilution curve, whereas the demand associated with the "fruit"
!!   (either grains or storage organs) depends on the nitrogen status of the crop through the variable absodrp. The biomass (W) used to calculate the N demand
!!   from the maximal dilution curve can be reduced using the parameters inngrain1 and inngrain2.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine bNpl(masec,nbrecolte,rdtint,surf,dltams,P_codeplisoleN,P_masecNmax,P_masecmeta,  & ! IN
                adilmaxI,bdilmaxI,adilI,bdilI,P_adilmax,P_bdilmax,P_bdil,P_adil,dltags,       &
                dltamsN,P_inngrain1,dltaremobilN,P_codelegume,P_codesymbiose,fixreel,       &
                masecdil,masecpartiel,inns,innlai,demande,abso,dNdWcrit,deltabso,     & ! INOUT
                absodrp,P_inngrain2,offrenod,demandebrute)

  implicit none

!: Arguments

  real,    intent(IN)    :: masec                     ! (n)       // OUTPUT // Aboveground dry matter  // t.ha-1
  integer, intent(IN)    :: nbrecolte  
  real,    intent(IN)    :: rdtint(nbrecolte-1)       ! 1 to nbrecolte-1
  real,    intent(IN)    :: surf      !> // OUTPUT // Fraction of surface in the shade // 0-1
  real,    intent(IN)    :: dltams                    ! (n)       // OUTPUT // Growth rate of the plant  // t ha-1.j-1
  integer, intent(IN)    :: P_codeplisoleN  !> // PARAMETER // code for N requirement calculations at the beginning of the cycle: dense plant population (1), isolated plants (2, new formalisation) // code 1/2 // PARPLT // 0 
  real,    intent(IN)    :: P_masecNmax  !> // PARAMETER // Aerial biomass  on and after there is nitrogen dilution (critical and maximal curves) // t ha-1 // PARPLT // 1 
  real,    intent(IN)    :: P_masecmeta  !> // PARAMETER // biomass of the plantlet supposed to be composed of metabolic nitrogen // t ha-1 // PARPLT // 1
  real,    intent(IN)    :: adilmaxI  
  real,    intent(IN)    :: bdilmaxI  
  real,    intent(IN)    :: adilI  
  real,    intent(IN)    :: bdilI  
  real,    intent(IN)    :: P_adilmax  !> // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // N% MS // PARPLT // 1 
  real,    intent(IN)    :: P_bdilmax  !> // PARAMETER // Parameter of the maximum curve of nitrogen needs [Nplante]=P_adilmax MS^(-P_bdilmax) // SD // PARPLT // 1 
  real,    intent(IN)    :: P_bdil  !> // PARAMETER // parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // SD // PARPLT // 1 
  real,    intent(IN)    :: P_adil  !> // PARAMETER // Parameter of the critical curve of nitrogen needs [Nplante]=P_adil MS^(-P_bdil) // N% MS // PARPLT // 1 
  real,    intent(IN)    :: dltags      !> // OUTPUT // Growth rate of the grains  // t ha-1.j-1
  real,    intent(IN)    :: dltamsN  
  real,    intent(IN)    :: P_inngrain1  !> // PARAMETER // INN minimal for net absorption of nitrogen during grain filling  // SD // PARPLT // 1 
  real,    intent(IN)    :: dltaremobilN  
  integer, intent(IN)    :: P_codelegume  !> // PARAMETER // 1 when the plant  id a legume crop, or 2 // code 1/2 // PARPLT // 0 
  integer, intent(IN)    :: P_codesymbiose  !> // PARAMETER // option of calculation of symbiotic fixation // code 1/2 // PARAM // 0 
  real,    intent(IN)    :: fixreel      !> // OUTPUT // Actual rate of symbiotic uptake // kg ha-1 j-1


  real,    intent(INOUT) :: masecdil  
  real,    intent(INOUT) :: masecpartiel  
  real,    intent(INOUT) :: inns      !> // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
  real,    intent(INOUT) :: innlai      !> // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin to 1
  real,    intent(INOUT) :: demande      !> // OUTPUT // Daily nitrogen need of the plant   // kgN.ha-1.j-1
  real,    intent(INOUT) :: abso                      ! (n)       // OUTPUT // Nitrogen absorption rate by plant  // kg N ha-1
  real,    intent(INOUT) :: dNdWcrit  
  real,    intent(INOUT) :: deltabso  
  real,    intent(INOUT) :: absodrp  
  real,    intent(INOUT) :: P_inngrain2  !> // PARAMETER // INN minimal for null net absorption of nitrogen during grain filling  // SD // PARPLT // 1 
  real,    intent(INOUT) :: offrenod      !> // OUTPUT // Amount of fixed nitrogen by nodules // kg.ha-1.j-1
  real,    intent(INOUT) :: demandebrute  

!: Variables locales
  integer :: irecolte  
  real    :: dNdW  


! ** TODO : voir si on peut définitivement supprimer ces commentaires.
! ** masecdil = masec pour culture annuelle et
! *-          = masecneo  pour les cultures perennes
! le 4/11/2004 c'est la totalité de la matière sèche qui est soumise à
! la courbe de dilution
!      if (P_codefauche == 2)then
!        if(naer == 0) then
!          masecdil=ms
!        else
!          masecdil=masec
!        endif
!
! ** arret d'absorption à la récolte - NB - le 15/05/02
! --         if(masec <= 0.and.nmat > 0) masecdil=0.
!        if(masec <= 0.and.nmat > 0) return
!      else
!        masecdil=masecneo
!      endif


      masecdil = masec

      ! ** en cas de ceuillette
      ! *- NB - le 08/05/02
      do irecolte = 1, nbrecolte-1
        masecdil = masecdil + (rdtint(irecolte) / 100.)
      end do


      ! ** NB le 22/04
      ! *- suppression des remobilisations des pérennes dans le calcul
      ! *- du stress azoté
      !-- masecdil = masecdil - min(cumdltares, P_resperenne0)


      ! ** NB - le 31/3/98 - pour CAS
      if(surf < 1.0) then
        masecpartiel = masecpartiel + dltams
      else
        masecpartiel = masecdil
      endif



      if (masecdil <=  0.0) then
        inns = 1.0
        innlai = 1.0
        demande = 0.0
        abso = 0.0
        return
      endif



      !--------------------------------------------------------------------!
      ! 1. demande en azote de la plante  (courbe de dilution maximale)    !
      !--------------------------------------------------------------------!
      !                                                                    !
      !  demande = dNmax/dt = dNmax/dW * dW/dt                             !
      !                                                                    !
      !  courbe de dilution : N% = a W**(-b)            si W > P_masecNmax   !
      !                       N% = a P_masecNmax**(-b)    si W < P_masecNmax   !
      !                                                                    !
      !  besoin cumulé en N :  N = a W**(1-b)           si W > P_masecNmax   !
      !                        N = a W P_masecNmax**(-b)  si W < P_masecNmax   !
      !                                                                    !
      !  besoin instantané  : dN/dt = dN/dW * dW/dt                        !
      !                                                                    !
      !          avec         dN/dW = a(1-b) W**(-b)    si W > P_masecNmax   !
      !                       dN/dW = a P_masecNmax**(-b) si W < P_masecNmax   !
      !                                                                    !
      !   NB introduction d'une fonction puissance pour la                 !
      !   première partie des courbes d'azote                              !
      !   W correspond à masecdil est en t/ha et demande en kgN/ha/jour    !
      !--------------------------------------------------------------------!

      if (P_codeplisoleN == 2) then

        ! NB - le 10/2/2005 corrigé le 11/09/05 pour introduire P_Nreserve
        ! tous les paramètres sont calculés dans initnonsol
        if (masecdil <= P_masecNmax) then
          if(masecdil <= P_masecmeta) masecdil = P_masecmeta
          dNdW = adilmaxI * (1-bdilmaxI) * masecdil**(-bdilmaxI)
          dNdWcrit = adilI * (1-bdilI) * masecdil**(-bdilI)
        else
          dNdW = P_adilmax * (1-P_bdil) * masecdil**(-P_bdil)
          dNdWcrit = P_adil * (1-P_bdil) * masecdil**(-P_bdil)
        endif

      else

! *- ancienne version P_codeplisoleN=1
! --     dNdWmax  = P_adilmax * P_masecNmax**(-P_bdilmax)
! --     dNdWcrit = P_adil    * P_masecNmax**(-P_bdil)

        ! masecdil est en t/ha et demande en kgN/ha/jour
        if (masecdil <= P_masecNmax) then
          dNdW = P_adilmax * P_masecNmax**(-P_bdilmax)
          dNdWcrit = P_adil * P_masecNmax**(-P_bdil)
        else
          dNdW = P_adilmax * (1-P_bdilmax) * masecdil**(-P_bdilmax)
          dNdWcrit = P_adil * (1-P_bdil) * masecdil**(-P_bdil)
        endif

      endif

      !: l'absortion d'azote diminue apres drp
      !- on applique la courbe à  MSveg + absodrp*MSgrain
      if (dltags <= 0.) then
        deltabso = dltamsN
        absodrp = 1.
      else
        ! absodrp dépend du statut azoté de la culture
        if(P_inngrain1 == P_inngrain2) then
          P_inngrain2 = P_inngrain1 + 0.01
        endif

        if(inns <= P_inngrain1) absodrp = 1.

        if(inns > P_inngrain1 .and. inns <= P_inngrain2) then
          absodrp = (inns - P_inngrain2) / (P_inngrain1 - P_inngrain2)
        endif

        if(inns > P_inngrain2) absodrp=0.

        if(dltams >= dltags) then
          deltabso = (dltamsN - dltags) + (absodrp * dltags)
        else
          deltabso = absodrp * dltamsN
        endif
      endif

!      demande = dNdW * deltabso * 10.* surf
      demande = dNdW * deltabso * 10.

      ! cas des pérennnes avec remobilisation des réserves azotées
      ! NB le 16/06/06
      demande = demande - dltaremobilN
      demande = max(demande, 0.01)


      ! cas des légumineuses : courbe critique assurée par les nodules
      if (P_codelegume == 2) then

        ! modif NB - le 11/2/2005
        if(P_codesymbiose == 1) then
          dNdW = dNdWcrit
! --          if (masecdil <= P_masecNmax) then
! --            dNdW = dNdWcrit
! --          else
! --            dNdW=P_adil*(1-P_bdil)*masecdil**(-P_bdil)
! --          endif
          ! la demande est par rapport à la surface considérée (NB le 27/3/98)
          !-- offrenod=dNdW*dltams*10.*surf
          offrenod = dNdW * dltams * 10.
        endif

        ! offrenod issu du sspg offrnodu
        if (P_codesymbiose == 2) then
          offrenod = fixreel
          offrenod = min(demande, offrenod)
          demandebrute = demande
        endif
        demande = demande - offrenod
      endif

return
end subroutine bNpl
 
 
