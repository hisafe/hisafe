!! ******************************************************************************************
!! STICS V8.5 VERSION SPECIFIQUE HI-SAFE
!! PROGRAMME PRINCIPAL CONTENANT LES FONCTIONS APPELLEES DEPUIS HISAFE
!! Auteur : Isabelle LECOMTE
!! Dernière modification : le 18 janvier 2017  nit(
!! ******************************************************************************************
!!
!! Liste de fonctions appellées depuis HISAFE via interface JNA
!! verifParam  : Vérification des fichiers de paramètres lus dans Hi-sAFe et ecriture dans fichier log
!! verifPlante : Vérification des fichiers de paramètres plantes lus dans Hi-sAFe et ecriture dans fichier log
!! initClimat  : Initialisation données climatiques
!! initBoucleAnnuelle : Initialisation de la boucle annuelle et ouverture du fichier BILAN
!! boucleJour1 : boucle journalière partie 1 (jusqu'à la demande en eau et azote de la plante)
!! boucleJour2 : boucle journalière partie 2 (On injecte d'extraction en eau et azote de la plante calculé dans HISAFE)
!! finBoucleAnnuelle : Terminaison de la boucle annuelle et ecriture du BILAN

!! ******************************************************************************************
!!
!! Liste des adaptations effectuées pour HISAFE et identifiées dans le code par les commentaires suivants :
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!MODIF HISAFE 2 : réduction dimension temporelle
!!!MODIF HISAFE 3 : réduction dimension interventions
!!!MODIF HISAFE 4 : suppression dimension temporelle
!!!MODIF HISAFE 5 : suppression variable inutile
!!!MODIF HISAFE 6 : remplacement des tableaux de booléen
!!!MODIF HISAFE 7 : déplacement de variables
!!!MODIF HISAFE 8 : suppression objet USM
!!!MODIF HISAFE 9 : Ajout influence de l'arbre HISAFE
!!!MODIF HISAFE 10 : Supression calcul déjà fait dans HISAFE
!!!MODIF HISAFE 11 : Supression code inutile
!!!MODIF HISAFE 12 : Modif après détection bug
!!
!! ******************************************************************************************

program Stics_Programme


USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux

   implicit none

   integer, parameter :: nb_plant_max = 2

    type(Stics_Communs_)            :: sc

    type(Plante_),dimension(nb_plant_max) :: p

    type(Parametres_Generaux_)      :: pg  

    type(ITK_),dimension(nb_plant_max)    :: itk

    type(Climat_)                   :: c  

    type(Station_)                  :: sta  

    type(Sol_)                      :: soil  

    type(Stics_Transit_)            :: t  



end program Stics_Programme


!! ******************************************************************************************
!! Vérification des fichiers de paramètres lus dans Hi-sAFe et ecriture dans fichier log
!! Paramètres généraux
!! Paramètres de transition
!! Paramètres sol
!! ******************************************************************************************

FUNCTION verifParam (pg, t, soil, sc, lg, repname) BIND(C, name='verifParam')

    USE Stics
    USE Parametres_Generaux
    USE Sol
    USE Messages

    TYPE(Stics_Communs_)            :: sc
    TYPE(Parametres_Generaux_)      :: pg
    type(Stics_Transit_)            :: t
    TYPE(Sol_)                      :: soil
    INTEGER,VALUE                   :: lg
    CHARACTER                       :: repname

    character(len=lg) :: repbilname
    character(len=255) :: filename
    character(len=2) :: langue = 'EN'


    !Remplissage messages en anglais
    call remplirMessages(langue)


    repbilname = ''
    filename= ''

     do i = 1, lg-1
       repbilname(i:i+1) = repname(i:i+1)
       if (repbilname(i:i+1) == "\") repbilname(i:i+1) = "/"
     end do
     filename= repbilname//'/initialisation.sti'


    open (88,file=filename, status='unknown')
    call setFichierHistorique(88)
    call setFlagEcriture(.true.)
    call EnvoyerMsgHistorique(5)

    call Ecriture_Parametres_Generaux(pg)

    call Ecriture_Transit(t)

    ! RAZ de tous les objets
    !!!call Stics_Zero(sc)

    call Sol_Ecriture(soil,pg)

    call Sol_Ecriture_Hisafe(soil)


    ! Fermeture des fichiers de sorties
    call EnvoyerMsgHistorique(5)
    close(88)



    return

END FUNCTION

!! ******************************************************************************************
!! Initialisation données climatiques
!! ******************************************************************************************

FUNCTION initClimat (pg, t, sta, c) BIND(C, name='initClimat')

    USE Stics
    USE Parametres_Generaux
    USE Station
    USE Climat


    TYPE(Parametres_Generaux_)  :: pg
    TYPE(Stics_Transit_)        :: t
    TYPE(Station_)              :: sta
    TYPE(Climat_)               :: c

    call iniclim1(c,sta)

    return

END FUNCTION

!! ******************************************************************************************
!! Vérification des fichiers de paramètres lus dans Hi-sAFe et ecriture dans fichier log
!! Paramètres plantes
!! Paramètres d'itinénaire technique
!! ******************************************************************************************

 FUNCTION verifPlante (pg, t, sc, itk, p, main, lg, repname) BIND(C, name='verifPlante')

    USE Stics
    USE Parametres_Generaux
    USE Itineraire_Technique
    USE Plante
    USE Messages
    USE Bilans

    TYPE(Stics_Communs_)            :: sc
    TYPE(Parametres_Generaux_)      :: pg
    TYPE(Stics_Transit_)            :: t
    type(Plante_),dimension(1)      :: p
    type(ITK_),dimension(1)         :: itk
    LOGICAL,VALUE                   :: main
    INTEGER,VALUE                   :: lg
    CHARACTER                       :: repname



    character(len=lg) :: repbilname
    character(len=255) :: filename
    integer codeRetour

    repbilname = ''
    filename= ''

     do i = 1, lg-1
       repbilname(i:i+1) = repname(i:i+1)
       if (repbilname(i:i+1) == "\") repbilname(i:i+1) = "/"
     end do


    if (main) then
        filename= repbilname//'/mainplante.sti'
    else
        filename= repbilname//'/secondplante.sti'
    end if

    open (90,file=filename,status='unknown')
    call setFichierHistorique(90)
    call setFlagEcriture(.true.)


    p(1)%ipl = 1
    itk(1)%ipl = 1


    call ITK_Ecriture_Tests(itk(1),pg,sc) !TODO: Séparer tests et écritures ?

    call Plante_Tests(p(1),itk(1)%P_variete)
    call Plante_Ecriture(p(1),sc,itk(1),pg)

    ! Fermeture des fichiers de sorties

    call EnvoyerMsgHistorique(5)
    close(90)


    return

END FUNCTION

!! ******************************************************************************************
!! Initialisation de la boucle annuelle et ouverture du fichier BILAN
!! ******************************************************************************************

FUNCTION initBoucleAnnuelle (pg, t, sta, c, sc, soil, p, itk, cellId, lg, repname) BIND(C, name='initBoucleAnnuelle')


    USE Stics
    USE Parametres_Generaux
    USE Station
    USE Sol
    USE Itineraire_Technique
    USE Plante
    USE Climat
    USE Bilans

    TYPE(Stics_Communs_)            :: sc
    TYPE(Parametres_Generaux_)      :: pg
    TYPE(Station_)                  :: sta
    TYPE(Stics_Transit_)            :: t
    TYPE(Sol_)                      :: soil
    TYPE(Climat_)                   :: c
    type(Plante_),dimension(1)      :: p
    type(ITK_),dimension(1)         :: itk

    INTEGER,VALUE                   :: cellId

    INTEGER,VALUE                   :: lg
    CHARACTER                       :: repname

    character(len=lg) :: repbilname
    character(len=255) :: ficbilname


    repbilname = ''
    ficbilname= ''


     do i = 1, lg-1
       repbilname(i:i+1) = repname(i:i+1)
       if (repbilname(i:i+1) == "\") repbilname(i:i+1) = "/"
     end do

    !Initialisation culture fauchées
        if (p(1)%P_codeplante == 2        &
              .and. itk(1)%P_codefauche == 1     &
              .and. itk(1)%lecfauche           &
              .and. (itk(1)%P_codemodfauche == 3 .or. itk(1)%P_codemodfauche == 2)) then
            call Initialisation_PrairiePerenne(sc,itk(1),1,p(1)%P_stade0)
        endif

    call Stics_Initialisation_Boucle_Annees(sc,p,pg,itk,c,sta,soil,t)


    call bilans_messages_ENG

    ! Ouverture des fichiers bilans

    if (pg%P_flagEcriture > 0) then
       do i = 1, sc%P_nbplantes

            write(ficbilname,'(A,A,I3.3,A,I4.4,A)') repbilname,'/rapport_',cellId,'_',sc%annee(1),'.bil'

            p(i)%ficbil = 25+cellId

           open (p(i)%ficbil, file=ficbilname,status='unknown')

           call ecrireEnteteBilan(sc,pg,p(i),itk(i),soil,sta,t)  !DR 19/07/2012 c n'est pas utilisé

      end do

    endif




    return

END FUNCTION

!:****************************************************************************
!:: Boucle Journalière AVANT partie 1                                       !*
!:: On incorpore les résidus de litière des arbres                          !*
!:: ires = 10 (feuilles en surface)                                         !*
!:: ires = 20 (feuilles jusqu'à profhum)                                    !*
!:: ires = 21 (racines fines ou de structure jusqu'à profhum)               !*
!:****************************************************************************
FUNCTION apport (pg, sc, p, itk, profmax, carbon, cn, cfeupc, water, ires) BIND(C, name='apport')


    USE Stics
    USE Parametres_Generaux
    USE Plante
    USE Sol
    USE Itineraire_Technique

    TYPE(Stics_Communs_)            :: sc
    TYPE(Parametres_Generaux_)      :: pg
    type(Plante_),dimension(1)      :: p
    type(ITK_),dimension(1)         :: itk

    REAL,VALUE                      :: profmax
    REAL,VALUE                      :: carbon
    REAL,VALUE                      :: cn
    REAL,VALUE                      :: cfeupc
    REAL,VALUE                      :: water
    INTEGER,VALUE                   :: ires


   !!!replaced by hisafe parameter
   !!!Cfeupc = 42.


    call ResidusApportSurfaceSol(carbon,cfeupc,water,ires,pg%P_CNresmin(ires),pg%P_CNresmax(ires),        &
                          profmax,profmax,pg%P_Qmulchdec(ires),sc%nbCouchesSol,pg%nbResidus,itk(1)%nap,   &
                          sc%airg(sc%n+1),cn,sc%Cnondec(1:10),sc%Nnondec(1:10),                           &
                          sc%Cres(1:sc%nbCouchesSol,1:pg%nbResidus),                                      &
                          sc%Nres(1:sc%nbCouchesSol,1:pg%nbResidus),                                      &
                          sc%QCapp,sc%QNapp,sc%QCresorg, sc%QNresorg)



    return

END FUNCTION

!:***************************************************************************
!:: Boucle Journalière partie 1                                            !*
!:: jusqu'à la demande en eau et azote de la plante                        !*
!:***************************************************************************
FUNCTION boucleJour1 (pg, t, sta, c, sc, soil, p, itk) BIND(C, name='boucleJour1')


        USE Stics
        USE Parametres_Generaux
        USE Station
        USE Sol
        USE Itineraire_Technique
        USE Plante
        USE Bilans


        TYPE(Stics_Communs_)            :: sc
        TYPE(Parametres_Generaux_)      :: pg
        TYPE(Station_)                  :: sta
        TYPE(Stics_Transit_)            :: t
        TYPE(Sol_)                      :: soil
        type(Plante_),dimension(1)      :: p
        type(ITK_),dimension(1)         :: itk


       call julien(sc%jul,sc%annee(sc%jjul),mois,sc%jour,sc%nummois)

       call Stics_Jour1(sc,pg,p,itk,soil,c,sta,t)

    return

END FUNCTION

!:***************************************************************************
!:: Boucle Journalière partie 2                                            !*
!:: On passe en parametre le % de RG et le % de ciel visible de la cellule !*
!:: On injecte d'extraction en eau et azote de la plante                   !*
!:***************************************************************************
FUNCTION boucleJour2 (pg, t, sta, c, sc, soil, p, itk, hisafeInfluence, cellTrg, cellVisibleSky) BIND(C, name='boucleJour2')


        USE Stics
        USE Parametres_Generaux
        USE Station
        USE Sol
        USE Itineraire_Technique
        USE Plante
        USE Bilans

        TYPE(Stics_Communs_)            :: sc
        TYPE(Parametres_Generaux_)      :: pg
        TYPE(Station_)                  :: sta
        TYPE(Stics_Transit_)            :: t
        TYPE(Sol_)                      :: soil
        type(Plante_),dimension(1)      :: p
        type(ITK_),dimension(1)         :: itk

        INTEGER,VALUE                   :: hisafeInfluence
        REAL,VALUE                      :: cellTrg
        REAL,VALUE                      :: cellVisibleSky

        real :: v


       call julien (sc%jul,sc%annee(sc%jjul),mois,sc%jour,sc%nummois)

       call Stics_Jour2 (sc,pg,p,itk,soil,c,sta,t, hisafeInfluence, cellTrg, cellVisibleSky)



END FUNCTION

!! ******************************************************************************************
!! Terminaison de la boucle annuelle et ecriture du fichier BILAN
!! ******************************************************************************************

FUNCTION finBoucleAnnuelle (pg, t, sta, c, sc, soil, p, itk, cellId) BIND(C, name='finBoucleAnnuelle')

    USE Stics
    USE Parametres_Generaux
    USE Station
    USE Sol
    USE Itineraire_Technique
    USE Plante
    USE Bilans

    TYPE(Stics_Communs_)            :: sc
    TYPE(Parametres_Generaux_)      :: pg
    TYPE(Station_)                  :: sta
    TYPE(Stics_Transit_)            :: t
    TYPE(Sol_)                      :: soil
    type(Plante_),dimension(1)      :: p
    type(ITK_),dimension(1)         :: itk
 
    INTEGER,VALUE                   :: cellId

    ! ecriture des fichiers bilans

    if (pg%P_flagEcriture > 0) then

        ! Rapport synthetique 1 ligne par annee
        do i = 1, sc%P_nbplantes

             sc%P_datefin = .TRUE.

            if(itk(i)%P_codefauche == 2) then
                call ecrireBilanDefaut(sc,pg,soil,p(i),itk(i))
            end if

            close(p(i)%ficbil)

        end do
   end if

END FUNCTION
