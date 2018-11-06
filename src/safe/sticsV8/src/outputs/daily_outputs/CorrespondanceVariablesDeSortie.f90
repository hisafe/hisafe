 ! **************************************************************
!    Getting variables values for a table of variable names
!    
! **************************************************************
!
!  MODIFICATIONS (last commit)
!    $Date: 2016-05-31 14:14:34 +0200 (mar., 31 mai 2016) $
!    $Author: domi $
!    $Revision: 1056 $
!
!***************************************************************

 subroutine CorrespondanceVariablesDeSorties(sc,p,soil,c,sta,nbVars,tabNomVars,tabValVars)

     USE Stics
     USE Plante
     USE Sol
     USE Climat
     USE Station
 
     implicit none
 
 
     type(Stics_Communs_),       intent(IN)      :: sc
     type(Plante_),              intent(IN)      :: p
     type(Sol_),                 intent(IN)      :: soil
     type(Climat_),              intent(IN)      :: c
     type(Station_),             intent(IN)      :: sta

     integer,                    intent(IN)      :: nbVars
     character(len=20),          intent(IN)      :: tabNomVars(nbVars) ! TODO: taille fixe ou dynamique ?
     real,                       intent(OUT)     :: tabValVars(nbVars) ! TODO: taille fixe ou dynamique ?
 
       character(20) :: nom
       character(100) :: tmp
       integer           :: i 
       integer           :: n
       integer           :: aoas
       integer           :: ao
       integer           :: as
       
 
     ! simplest use
       aoas = sc%aoas
       ao = sc%ao
       as = sc%as
       n = sc%n
 
 B1:   do i=1, nbVars
      nom=tabNomVars(i)
 
    if (tabNomVars(i) == 'abso(n)') then
      tabValVars(i) = p%abso(aoas,n)
      CYCLE
    endif
    if (tabNomVars(i) == 'age_prairie') then
      tabValVars(i) = float(p%age_prairie)
      CYCLE
    endif
    if (tabNomVars(i) == 'airg(n)') then
      tabValVars(i) = sc%airg(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'albedolai') then
      tabValVars(i) = sc%albedolai
      CYCLE
    endif
    if (tabNomVars(i) == 'allocfruit') then
      tabValVars(i) = p%allocfruit(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'ammomes') then
      tabValVars(i) = sc%ammomes
      CYCLE
    endif
    if (tabNomVars(i) == 'amptcultmat') then
      tabValVars(i) = c%amptcultmat
      CYCLE
    endif
    if (tabNomVars(i) == 'anit(n)') then
      tabValVars(i) = sc%anit(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'anoxmoy') then
      tabValVars(i) = p%anoxmoy
      CYCLE
    endif
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
    if (tabNomVars(i) == 'AZamm(1)') then
      !!!tabValVars(i) = sc%AZamm(1)
      tabValVars(i) = soil%AZamm(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'AZamm(2)') then
      !!!tabValVars(i) = sc%AZamm(2)
      tabValVars(i) = soil%AZamm(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'AZamm(3)') then
      !!!tabValVars(i) = sc%AZamm(3)
      tabValVars(i) = soil%AZamm(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'AZamm(4)') then
      !!!tabValVars(i) = sc%AZamm(4)
      tabValVars(i) = soil%AZamm(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'AZamm(5)') then
      !!!tabValVars(i) = sc%AZamm(5)
      tabValVars(i) = soil%AZamm(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'azlesd') then
      tabValVars(i) = soil%azlesd
      CYCLE
    endif
    if (tabNomVars(i) == 'AZnit(1)') then
      tabValVars(i) = soil%AZnit(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'AZnit(2)') then
      tabValVars(i) = soil%AZnit(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'AZnit(3)') then
      tabValVars(i) = soil%AZnit(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'AZnit(4)') then
      tabValVars(i) = soil%AZnit(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'AZnit(5)') then
      tabValVars(i) = soil%AZnit(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'azomes') then
      tabValVars(i) = sc%azomes
      CYCLE
    endif
    if (tabNomVars(i) == 'bouchon') then
      tabValVars(i) = sc%bouchon
      CYCLE
    endif
    if (tabNomVars(i) == 'Cb') then
      tabValVars(i) = sc%Cb
      CYCLE
    endif
    if (tabNomVars(i) == 'Cbmulch') then
      tabValVars(i) = sc%Cbmulch
      CYCLE
    endif
    if (tabNomVars(i) == 'cdemande') then
      tabValVars(i) = p%cdemande
      CYCLE
    endif
    if (tabNomVars(i) == 'cep') then
      tabValVars(i) = p%cep
      CYCLE
    endif
    if (tabNomVars(i) == 'ces') then
      tabValVars(i) = p%ces
      CYCLE
    endif
    if (tabNomVars(i) == 'cestout') then
      tabValVars(i) = sc%cestout
      CYCLE
    endif
    if (tabNomVars(i) == 'cet') then
      tabValVars(i) = p%cet
      CYCLE
    endif
    if (tabNomVars(i) == 'cetm') then
      tabValVars(i) = p%cetm
      CYCLE
    endif
    if (tabNomVars(i) == 'Cetmtout') then
      tabValVars(i) = c%Cetmtout
      CYCLE
    endif
    if (tabNomVars(i) == 'cetp') then
      tabValVars(i) = p%cetp
      CYCLE
    endif
    if (tabNomVars(i) == 'chargefruit') then
      tabValVars(i) = p%chargefruit
      CYCLE
    endif
    if (tabNomVars(i) == 'Chuma') then
      tabValVars(i) = sc%Chuma
      CYCLE
    endif
    if (tabNomVars(i) == 'Chumi') then
      tabValVars(i) = sc%Chumi
      CYCLE
    endif
    if (tabNomVars(i) == 'Chumt') then
      tabValVars(i) = sc%Chumt
      CYCLE
    endif
    if (tabNomVars(i) == 'cintermulch') then
      tabValVars(i) = sc%cintermulch
      CYCLE
    endif
    if (tabNomVars(i) == 'cinterpluie') then
      tabValVars(i) = p%cinterpluie
      CYCLE
    endif
    if (tabNomVars(i) == 'Cmulch') then
      tabValVars(i) = sc%Cmulch
      CYCLE
    endif
    if (tabNomVars(i) == 'Cmulchdec') then
      tabValVars(i) = sc%Cmulchdec
      CYCLE
    endif
    if (tabNomVars(i) == 'Cmulchnd') then
      tabValVars(i) = sc%Cmulchnd
      CYCLE
    endif
    if (tabNomVars(i) == 'CNgrain') then
      tabValVars(i) = p%CNgrain(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cnondec(1)') then
      tabValVars(i) = sc%Cnondec(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cnondec(10)') then
      tabValVars(i) = sc%Cnondec(10)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cnondec(2)') then
      tabValVars(i) = sc%Cnondec(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cnondec(3)') then
      tabValVars(i) = sc%Cnondec(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cnondec(4)') then
      tabValVars(i) = sc%Cnondec(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cnondec(5)') then
      tabValVars(i) = sc%Cnondec(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cnondec(6)') then
      tabValVars(i) = sc%Cnondec(6)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cnondec(7)') then
      tabValVars(i) = sc%Cnondec(7)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cnondec(8)') then
      tabValVars(i) = sc%Cnondec(8)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cnondec(9)') then
      tabValVars(i) = sc%Cnondec(9)
      CYCLE
    endif
    if (tabNomVars(i) == 'CNplante') then
      tabValVars(i) = p%CNplante(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'co2(n)') then
      tabValVars(i) = c%co2(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'CO2hum') then
      tabValVars(i) = sc%CO2hum
      CYCLE
    endif
    if (tabNomVars(i) == 'CO2res') then
      tabValVars(i) = sc%CO2res
      CYCLE
    endif
    if (tabNomVars(i) == 'CO2sol') then
      tabValVars(i) = sc%CO2sol
      CYCLE
    endif
    if (tabNomVars(i) == 'codebbch_output') then
      tabValVars(i) = float(p%codebbch_output)
      CYCLE
    endif
    if (tabNomVars(i) == 'concNO3les') then
      tabValVars(i) = soil%concNO3les
      CYCLE
    endif
    if (tabNomVars(i) == 'concNO3sol(1)') then
      tabValVars(i) = sc%concNO3sol(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'concNO3sol(2)') then
      tabValVars(i) = sc%concNO3sol(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'concNO3sol(3)') then
      tabValVars(i) = sc%concNO3sol(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'concNO3sol(4)') then
      tabValVars(i) = sc%concNO3sol(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'concNO3sol(5)') then
      tabValVars(i) = sc%concNO3sol(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'condenit') then
      tabValVars(i) = soil%condenit
      CYCLE
    endif
    if (tabNomVars(i) == 'couvermulch') then
      tabValVars(i) = sc%couvermulch
      CYCLE
    endif
    if (tabNomVars(i) == 'cpluie') then
      tabValVars(i) = sc%cpluie
      CYCLE
    endif
    if (tabNomVars(i) == 'cprecip') then
      tabValVars(i) = p%cprecip
      CYCLE
    endif
    if (tabNomVars(i) == 'cpreciptout') then
      tabValVars(i) = sc%cpreciptout
      CYCLE
    endif
    if (tabNomVars(i) == 'Cr') then
      tabValVars(i) = sc%Cr
      CYCLE
    endif
    if (tabNomVars(i) == 'Crac') then
      tabValVars(i) = p%Crac
      CYCLE
    endif
    if (tabNomVars(i) == 'Cresiduprofil(1)') then
      tabValVars(i) = sc%Cresiduprofil(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cresiduprofil(10)') then
      tabValVars(i) = sc%Cresiduprofil(10)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cresiduprofil(2)') then
      tabValVars(i) = sc%Cresiduprofil(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cresiduprofil(3)') then
      tabValVars(i) = sc%Cresiduprofil(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cresiduprofil(4)') then
      tabValVars(i) = sc%Cresiduprofil(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cresiduprofil(5)') then
      tabValVars(i) = sc%Cresiduprofil(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cresiduprofil(6)') then
      tabValVars(i) = sc%Cresiduprofil(6)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cresiduprofil(7)') then
      tabValVars(i) = sc%Cresiduprofil(7)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cresiduprofil(8)') then
      tabValVars(i) = sc%Cresiduprofil(8)
      CYCLE
    endif
    if (tabNomVars(i) == 'Cresiduprofil(9)') then
      tabValVars(i) = sc%Cresiduprofil(9)
      CYCLE
    endif
    if (tabNomVars(i) == 'crg') then
      tabValVars(i) = p%crg
      CYCLE
    endif
    if (tabNomVars(i) == 'crgtout') then
      tabValVars(i) = c%crgtout
      CYCLE
    endif
    if (tabNomVars(i) == 'CsurNres_pature') then
      tabValVars(i) = sc%CsurNres_pature
      CYCLE
    endif
    if (tabNomVars(i) == 'ctairtout') then
      tabValVars(i) = c%ctairtout
      CYCLE
    endif
    if (tabNomVars(i) == 'ctcult') then
      tabValVars(i) = p%ctcult
      CYCLE
    endif
    if (tabNomVars(i) == 'ctculttout') then
      tabValVars(i) = c%ctculttout
      CYCLE
    endif
    if (tabNomVars(i) == 'ctetptout') then
      tabValVars(i) = c%ctetptout
      CYCLE
    endif
    if (tabNomVars(i) == 'ctmoy') then
      tabValVars(i) = p%ctmoy
      CYCLE
    endif
    if (tabNomVars(i) == 'Ctousresidusprofil') then
      tabValVars(i) = sc%Ctousresidusprofil
      CYCLE
    endif
    if (tabNomVars(i) == 'cum_et0') then
      tabValVars(i) = p%cum_et0
      CYCLE
    endif
    if (tabNomVars(i) == 'et0') then
      tabValVars(i) = p%et0
      CYCLE
    endif
    if (tabNomVars(i) == 'cum_immob') then
      tabValVars(i) = sc%cum_immob
      CYCLE
    endif
    if (tabNomVars(i) == 'cumlracz') then
      tabValVars(i) = p%cumlracz
      CYCLE
    endif
    if (tabNomVars(i) == 'cumraint') then
      tabValVars(i) = p%cumraint
      CYCLE
    endif
    if (tabNomVars(i) == 'cumrg') then
      tabValVars(i) = p%cumrg
      CYCLE
    endif
    if (tabNomVars(i) == 'cumvminh') then
      tabValVars(i) = soil%cumvminh
      CYCLE
    endif
    if (tabNomVars(i) == 'cumvminr') then
      tabValVars(i) = soil%cumvminr
      CYCLE
    endif
    if (tabNomVars(i) == 'da(1)') then
      tabValVars(i) = soil%da(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'da(2)') then
      tabValVars(i) = soil%da(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'deltai(n)') then
      tabValVars(i) = p%deltai(aoas,n)
      CYCLE
    endif
    if (tabNomVars(i) == 'deltaz') then
      tabValVars(i) = p%deltaz
      CYCLE
    endif
    if (tabNomVars(i) == 'demande') then
      tabValVars(i) = p%demande(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'densite') then
      tabValVars(i) = p%densite
      CYCLE
    endif
    if (tabNomVars(i) == 'densiteequiv') then
      tabValVars(i) = p%densiteequiv
      CYCLE
    endif
    if (tabNomVars(i) == 'dfol') then
      tabValVars(i) = p%dfol
      CYCLE
    endif
    if (tabNomVars(i) == 'diftemp1intercoupe') then
      tabValVars(i) = p%diftemp1intercoupe
      CYCLE
    endif
    if (tabNomVars(i) == 'diftemp2intercoupe') then
      tabValVars(i) = p%diftemp2intercoupe
      CYCLE
    endif
    if (tabNomVars(i) == 'dltags') then
      tabValVars(i) = p%dltags(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'dltaisen') then
      tabValVars(i) = p%dltaisen(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'dltams(n)') then
      tabValVars(i) = p%dltams(aoas,n)
      CYCLE
    endif
    if (tabNomVars(i) == 'dltamsen') then
      tabValVars(i) = p%dltamsen(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'dltaremobil') then
      tabValVars(i) = p%dltaremobil(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'dltmsrac_plante') then
      tabValVars(i) = p%dltmsrac_plante
      CYCLE
    endif
    if (tabNomVars(i) == 'drain') then
      tabValVars(i) = sc%drain
      CYCLE
    endif
    if (tabNomVars(i) == 'drain_from_plt') then
      tabValVars(i) = sc%drain_from_plt
      CYCLE
    endif
    if (tabNomVars(i) == 'drat') then
      tabValVars(i) = sc%drat
      CYCLE
    endif
    if (tabNomVars(i) == 'drlsenmortalle') then
      tabValVars(i) = p%drlsenmortalle
      CYCLE
    endif
    if (tabNomVars(i) == 'dtj(n)') then
      tabValVars(i) = p%dtj(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'dureehumec') then
      tabValVars(i) = c%dureehumec
      CYCLE
    endif
    if (tabNomVars(i) == 'dureeRH') then
      tabValVars(i) = c%dureeRH
      CYCLE
    endif
    if (tabNomVars(i) == 'durvie(n)') then
      tabValVars(i) = p%durvie(aoas,n)
      CYCLE
    endif
    if (tabNomVars(i) == 'ebmax') then
      tabValVars(i) = p%ebmax
      CYCLE
    endif
    if (tabNomVars(i) == 'ebmax_gr') then
      tabValVars(i) = p%ebmax_gr
      CYCLE
    endif
    if (tabNomVars(i) == 'Edirect') then
      tabValVars(i) = sc%Edirect
      CYCLE
    endif
    if (tabNomVars(i) == 'efda') then
      tabValVars(i) = p%efda
      CYCLE
    endif
    if (tabNomVars(i) == 'efdensite') then
      tabValVars(i) = p%efdensite
      CYCLE
    endif
    if (tabNomVars(i) == 'efdensite') then
      tabValVars(i) = p%efdensite
      CYCLE
    endif
    if (tabNomVars(i) == 'efNrac_mean') then
      tabValVars(i) = p%efNrac_mean
      CYCLE
    endif
    if (tabNomVars(i) == 'em_N2O') then
      tabValVars(i) = sc%em_N2O
      CYCLE
    endif
    if (tabNomVars(i) == 'em_N2Oden') then
      tabValVars(i) = sc%em_N2Oden
      CYCLE
    endif
    if (tabNomVars(i) == 'em_N2Onit') then
      tabValVars(i) = sc%em_N2Onit
      CYCLE
    endif
    if (tabNomVars(i) == 'emd') then
      tabValVars(i) = p%emd
      CYCLE
    endif
    if (tabNomVars(i) == 'emulch') then
      tabValVars(i) = sc%emulch
      CYCLE
    endif
    if (tabNomVars(i) == 'eo') then
      tabValVars(i) = sc%eo
      CYCLE
    endif
    if (tabNomVars(i) == 'eop') then
      tabValVars(i) = p%eop(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'eos') then
      tabValVars(i) = sc%eos
      CYCLE
    endif
    if (tabNomVars(i) == 'ep') then
      tabValVars(i) = p%ep(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'epc_recal(1)') then
      tabValVars(i) = soil%epc_recal(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'epc_recal(2)') then
      tabValVars(i) = soil%epc_recal(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'epc_recal(3)') then
      tabValVars(i) = soil%epc_recal(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'epc_recal(4)') then
      tabValVars(i) = soil%epc_recal(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'epc_recal(5)') then
      tabValVars(i) = soil%epc_recal(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'epsib') then
      tabValVars(i) = p%epsib(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'esol') then
      tabValVars(i) = sc%esol
      CYCLE
    endif
    if (tabNomVars(i) == 'et') then
      tabValVars(i) = sc%et
      CYCLE
    endif
    if (tabNomVars(i) == 'etm') then
      tabValVars(i) = sc%etm
      CYCLE
    endif
    if (tabNomVars(i) == 'etpp(n)') then
      tabValVars(i) = c%etpp(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'exces(1)') then
      tabValVars(i) = sc%exces(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'exces(2)') then
      tabValVars(i) = sc%exces(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'exces(3)') then
      tabValVars(i) = sc%exces(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'exces(4)') then
      tabValVars(i) = sc%exces(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'exces(5)') then
      tabValVars(i) = sc%exces(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'exobiom') then
      tabValVars(i) = p%exobiom
      CYCLE
    endif
    if (tabNomVars(i) == 'exofac') then
      tabValVars(i) = p%exofac
      CYCLE
    endif
    if (tabNomVars(i) == 'exofac1moy') then
      tabValVars(i) = p%exofac1moy
      CYCLE
    endif
    if (tabNomVars(i) == 'exofac2moy') then
      tabValVars(i) = p%exofac2moy
      CYCLE
    endif
    if (tabNomVars(i) == 'exolai') then
      tabValVars(i) = p%exolai
      CYCLE
    endif
    if (tabNomVars(i) == 'fapar') then
      tabValVars(i) = p%fapar(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'fco2') then
      tabValVars(i) = p%fco2
      CYCLE
    endif
    if (tabNomVars(i) == 'fco2s') then
      tabValVars(i) = p%fco2s
      CYCLE
    endif
    if (tabNomVars(i) == 'fgelflo') then
      tabValVars(i) = p%fgelflo
      CYCLE
    endif
    if (tabNomVars(i) == 'fixmaxvar') then
      tabValVars(i) = p%fixmaxvar(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'fixpot') then
      tabValVars(i) = p%fixpot(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'fixreel') then
      tabValVars(i) = p%fixreel(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'flurac') then
      tabValVars(i) = p%flurac
      CYCLE
    endif
    if (tabNomVars(i) == 'flusol') then
      tabValVars(i) = p%flusol
      CYCLE
    endif
    if (tabNomVars(i) == 'fpari') then
      tabValVars(i) = p%fpari
      CYCLE
    endif
    if (tabNomVars(i) == 'fpari_gr') then
      tabValVars(i) = p%fpari_gr
      CYCLE
    endif
    if (tabNomVars(i) == 'fpft') then
      tabValVars(i) = p%fpft(aoas)
      CYCLE
    endif
  !!!  if (tabNomVars(i) == 'fpv(n)') then
  !!!    tabValVars(i) = p%fpv(aoas,n)
  !!!    CYCLE
  !!!  endif
    if (tabNomVars(i) == 'FsNH3') then
      tabValVars(i) = sc%FsNH3
      CYCLE
    endif
    if (tabNomVars(i) == 'fstressgel') then
      tabValVars(i) = p%fstressgel
      CYCLE
    endif
    if (tabNomVars(i) == 'ftemp') then
      tabValVars(i) = p%ftemp
      CYCLE
    endif
    if (tabNomVars(i) == 'fxa') then
      tabValVars(i) = sc%fxa
      CYCLE
    endif
    if (tabNomVars(i) == 'fxn') then
      tabValVars(i) = sc%fxn
      CYCLE
    endif
    if (tabNomVars(i) == 'fxt') then
      tabValVars(i) = sc%fxt
      CYCLE
    endif
    if (tabNomVars(i) == 'fxw') then
      tabValVars(i) = sc%fxw
      CYCLE
    endif
    if (tabNomVars(i) == 'gel1') then
      tabValVars(i) = p%gel1
      CYCLE
    endif
    if (tabNomVars(i) == 'gel1_percent') then
      tabValVars(i) = p%gel1_percent
      CYCLE
    endif
    if (tabNomVars(i) == 'gel2') then
      tabValVars(i) = p%gel2
      CYCLE
    endif
    if (tabNomVars(i) == 'gel2_percent') then
      tabValVars(i) = p%gel2_percent
      CYCLE
    endif
    if (tabNomVars(i) == 'gel3') then
      tabValVars(i) = p%gel3
      CYCLE
    endif
    if (tabNomVars(i) == 'gel3_percent') then
      tabValVars(i) = p%gel3_percent
      CYCLE
    endif
    if (tabNomVars(i) == 'H2Orec') then
      tabValVars(i) = p%H2Orec(aoas)
      CYCLE
    endif
   !!! if (tabNomVars(i) == 'H2Orec_percent') then
   !!!   tabValVars(i) = p%H2Orec_percent(aoas)
   !!!   CYCLE
   !!! endif
    if (tabNomVars(i) == 'hauteur') then
      tabValVars(i) = p%hauteur(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'Hmax') then
      tabValVars(i) = soil%Hmax
      CYCLE
    endif
    if (tabNomVars(i) == 'Hnappe') then
      tabValVars(i) = soil%Hnappe
      CYCLE
    endif
    if (tabNomVars(i) == 'Hpb') then
      tabValVars(i) = soil%Hpb
      CYCLE
    endif
    if (tabNomVars(i) == 'Hph') then
      tabValVars(i) = soil%Hph
      CYCLE
    endif
!!!MODIF HISAFE 7 : Déplacement de variables (de Stics.f90 dans Sol.f90)
    if (tabNomVars(i) == 'HR(1)') then
      !!!tabValVars(i) = sc%HR(1)
      tabValVars(i) = soil%HR(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'HR(2)') then
      !!!tabValVars(i) = sc%HR(2)
      tabValVars(i) = soil%HR(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'HR(3)') then
      !!!tabValVars(i) = sc%HR(3)
      tabValVars(i) = soil%HR(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'HR(4)') then
      !!!tabValVars(i) = sc%HR(4)
      tabValVars(i) = soil%HR(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'HR(5)') then
      !!!tabValVars(i) = sc%HR(5)
      tabValVars(i) = soil%HR(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'HR_vol_1_30') then
      tabValVars(i) = sc%HR_vol_1_30
      CYCLE
    endif
    if (tabNomVars(i) == 'HR_vol_121_150') then
      tabValVars(i) = sc%HR_vol_121_150
      CYCLE
    endif
    if (tabNomVars(i) == 'HR_vol_31_60') then
      tabValVars(i) = sc%HR_vol_31_60
      CYCLE
    endif
    if (tabNomVars(i) == 'HR_vol_61_90') then
      tabValVars(i) = sc%HR_vol_61_90
      CYCLE
    endif
    if (tabNomVars(i) == 'HR_vol_91_120') then
      tabValVars(i) = sc%HR_vol_91_120
      CYCLE
    endif
    if (tabNomVars(i) == 'HR_vol_151_180') then
      tabValVars(i) = sc%HR_vol_151_180
      CYCLE
    endif

    if (tabNomVars(i) == 'huile') then
      tabValVars(i) = p%huile(aoas)
      CYCLE
    endif
  !!!  if (tabNomVars(i) == 'huile_percent') then
  !!!    tabValVars(i) = p%huile_percent(aoas)
  !!!    CYCLE
  !!!  endif
    if (tabNomVars(i) == 'humair') then
      tabValVars(i) = c%humair
      CYCLE
    endif
    if (tabNomVars(i) == 'humair_percent') then
      tabValVars(i) = c%humair_percent
      CYCLE
    endif
    if (tabNomVars(i) == 'humidite') then
      tabValVars(i) = sc%humidite
      CYCLE
    endif
    if (tabNomVars(i) == 'humidite_percent') then
      tabValVars(i) = sc%humidite_percent
      CYCLE
    endif
    if (tabNomVars(i) == 'humirac_mean') then
      tabValVars(i) = p%humirac_mean
      CYCLE
    endif
    if (tabNomVars(i) == 'iamfs') then
      tabValVars(i) = float(p%iamfs)
      CYCLE
    endif
    if (tabNomVars(i) == 'idebdess') then
      tabValVars(i) = float(p%idebdess)
      CYCLE
    endif
    if (tabNomVars(i) == 'idebdorms') then
      tabValVars(i) = float(p%idebdorms)
      CYCLE
    endif
    if (tabNomVars(i) == 'idrps') then
      tabValVars(i) = float(p%idrps)
      CYCLE
    endif
    if (tabNomVars(i) == 'ifindorms') then
      tabValVars(i) = float(p%ifindorms)
      CYCLE
    endif
    if (tabNomVars(i) == 'iflos') then
      tabValVars(i) = float(p%iflos)
      CYCLE
    endif
    if (tabNomVars(i) == 'igers') then
      tabValVars(i) = float(p%igers)
      CYCLE
    endif
    if (tabNomVars(i) == 'ilans') then
      tabValVars(i) = float(p%ilans)
      CYCLE
    endif
    if (tabNomVars(i) == 'ilaxs') then
      tabValVars(i) = float(p%ilaxs)
      CYCLE
    endif
    if (tabNomVars(i) == 'ilevs') then
      tabValVars(i) = float(p%ilevs)
      CYCLE
    endif
    if (tabNomVars(i) == 'imats') then
      tabValVars(i) = float(p%imats)
      CYCLE
    endif
    if (tabNomVars(i) == 'imontaisons') then
      tabValVars(i) = float(p%imontaisons)
      CYCLE
    endif
    if (tabNomVars(i) == 'infil_recal(1)') then
      tabValVars(i) = soil%infil_recal(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'infil_recal(2)') then
      tabValVars(i) = soil%infil_recal(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'infil_recal(3)') then
      tabValVars(i) = soil%infil_recal(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'infil_recal(4)') then
      tabValVars(i) = soil%infil_recal(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'infil_recal(5)') then
      tabValVars(i) = soil%infil_recal(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'inn') then
      tabValVars(i) = p%inn(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'inn1intercoupe') then
      tabValVars(i) = p%inn1intercoupe
      CYCLE
    endif
    if (tabNomVars(i) == 'inn1moy') then
      tabValVars(i) = p%inn1moy
      CYCLE
    endif
    if (tabNomVars(i) == 'inn2intercoupe') then
      tabValVars(i) = p%inn2intercoupe
      CYCLE
    endif
    if (tabNomVars(i) == 'inn2moy') then
      tabValVars(i) = p%inn2moy
      CYCLE
    endif
    if (tabNomVars(i) == 'innlai') then
      tabValVars(i) = p%innlai(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'inns') then
      tabValVars(i) = p%inns(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'innsenes') then
      tabValVars(i) = p%innsenes(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'inous') then
      tabValVars(i) = p%inous
      CYCLE
    endif
    if (tabNomVars(i) == 'intermulch') then
      tabValVars(i) = sc%intermulch
      CYCLE
    endif
    if (tabNomVars(i) == 'interpluie') then
      tabValVars(i) = p%interpluie(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'iplts') then
      tabValVars(i) = float(p%iplts)
      CYCLE
    endif
    if (tabNomVars(i) == 'irazo(n)') then
      tabValVars(i) = p%irazo(aoas,n)
      CYCLE
    endif
    if (tabNomVars(i) == 'ircarb(n)') then
      tabValVars(i) = p%ircarb(aoas,n)
      CYCLE
    endif
    if (tabNomVars(i) == 'irecs') then
      tabValVars(i) = float(p%irecs)
      CYCLE
    endif
    if (tabNomVars(i) == 'irrigjN') then
      tabValVars(i) = sc%irrigjN
      CYCLE
    endif
    if (tabNomVars(i) == 'irrigN') then
      tabValVars(i) = sc%irrigN
      CYCLE
    endif
    if (tabNomVars(i) == 'isens') then
      tabValVars(i) = float(p%isens)
      CYCLE
    endif
    if (tabNomVars(i) == 'izrac') then
      tabValVars(i) = p%izrac
      CYCLE
    endif
    if (tabNomVars(i) == 'lai(n)') then
      tabValVars(i) = p%lai(aoas,n)
      CYCLE
    endif
    if (tabNomVars(i) == 'laimax') then
      tabValVars(i) = p%laimax(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'laisen(n)') then
      tabValVars(i) = p%laisen(aoas,n)
      CYCLE
    endif
    if (tabNomVars(i) == 'largeur') then
      tabValVars(i) = p%largeur
      CYCLE
    endif
    if (tabNomVars(i) == 'leaching_from_plt') then
      tabValVars(i) = sc%leaching_from_plt
      CYCLE
    endif
    if (tabNomVars(i) == 'lessiv') then
      tabValVars(i) = sc%lessiv
      CYCLE
    endif
    if (tabNomVars(i) == 'LRACH(1)') then
      tabValVars(i) = p%LRACH(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'LRACH(2)') then
      tabValVars(i) = p%LRACH(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'LRACH(3)') then
      tabValVars(i) = p%LRACH(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'LRACH(4)') then
      tabValVars(i) = p%LRACH(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'LRACH(5)') then
      tabValVars(i) = p%LRACH(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'lracsentot') then
      tabValVars(i) = p%lracsentot
      CYCLE
    endif
    if (tabNomVars(i) == 'mabois') then
      tabValVars(i) = p%mabois(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'maenfruit') then
      tabValVars(i) = p%maenfruit(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'mafeuil') then
      tabValVars(i) = p%mafeuil(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'mafeuil_kg_ha') then
      tabValVars(i) = p%mafeuil_kg_ha
      CYCLE
    endif
    if (tabNomVars(i) == 'mafeuiljaune') then
      tabValVars(i) = p%mafeuiljaune(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'mafeuiltombe') then
      tabValVars(i) = p%mafeuiltombe(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'mafeuilverte') then
      tabValVars(i) = p%mafeuilverte(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'mafrais') then
      tabValVars(i) = p%mafrais(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'mafruit') then
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!    if ( p%P_codeplante == 'bet') then
    if ( p%P_codeplante == 18) then
      tabValVars(i) = p%matuber
    else
      tabValVars(i) = p%mafruit
    endif
      CYCLE
    endif
    if (tabNomVars(i) == 'mafruit_kg_ha') then
      tabValVars(i) = p%mafruit_kg_ha
      CYCLE
    endif
    if (tabNomVars(i) == 'masec(n)') then
      tabValVars(i) = p%masec(aoas,n)
      CYCLE
    endif
    if (tabNomVars(i) == 'masec_kg_ha') then
      tabValVars(i) = p%masec_kg_ha
      CYCLE
    endif
    if (tabNomVars(i) == 'masecneo') then
      tabValVars(i) = p%masecneo(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'masectot') then
      tabValVars(i) = p%masectot
      CYCLE
    endif
    if (tabNomVars(i) == 'masecveg') then
      tabValVars(i) = p%masecveg(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'matigestruc') then
      tabValVars(i) = p%matigestruc(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'matigestruc_kg_ha') then
      tabValVars(i) = p%matigestruc_kg_ha
      CYCLE
    endif
    if (tabNomVars(i) == 'matuber') then
      tabValVars(i) = p%matuber
      CYCLE
    endif
    if (tabNomVars(i) == 'mortalle') then
      tabValVars(i) = p%mortalle
      CYCLE
    endif
    if (tabNomVars(i) == 'mortmasec') then
      tabValVars(i) = p%mortmasec
      CYCLE
    endif
    if (tabNomVars(i) == 'mortreserve') then
      tabValVars(i) = p%mortreserve
      CYCLE
    endif
    if (tabNomVars(i) == 'MSexporte') then
      tabValVars(i) = p%MSexporte
      CYCLE
    endif
    if (tabNomVars(i) == 'msjaune') then
      tabValVars(i) = p%msjaune(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'msneojaune') then
      tabValVars(i) = p%msneojaune(aoas)
      CYCLE
    endif
   if (tabNomVars(i) == 'msrac(n)') then
     tabValVars(i) = p%msrac(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'msrec_fou') then
      tabValVars(i) = p%msrec_fou
      CYCLE
    endif
    if (tabNomVars(i) == 'MSrecycle') then
      tabValVars(i) = p%MSrecycle
      CYCLE
    endif
    if (tabNomVars(i) == 'msresjaune') then
      tabValVars(i) = p%msresjaune(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nb') then
      tabValVars(i) = sc%Nb
      CYCLE
    endif
    if (tabNomVars(i) == 'nbfeuille') then
      tabValVars(i) = float(p%nbfeuille)
      CYCLE
    endif
    if (tabNomVars(i) == 'nbinflo_recal') then
      !tabValVars(i) = float(p%nbinflo_recal)
      tabValVars(i) = p%nbinflo_recal
      CYCLE
    endif
    if (tabNomVars(i) == 'nbj0remp') then
      tabValVars(i) = float(p%nbj0remp)
      CYCLE
    endif
    if (tabNomVars(i) == 'nbjechaudage') then
      tabValVars(i) = float(c%nbjechaudage)
      CYCLE
    endif
    if (tabNomVars(i) == 'nbjgel') then
      tabValVars(i) = float(p%nbjgel)
      CYCLE
    endif
    if (tabNomVars(i) == 'nbjpourdecirecolte') then
      tabValVars(i) = float(p%nbjpourdecirecolte)
      CYCLE
    endif
    if (tabNomVars(i) == 'nbjpourdecisemis') then
      tabValVars(i) = float(p%nbjpourdecisemis)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nbmulch') then
      tabValVars(i) = sc%Nbmulch
      CYCLE
    endif
    if (tabNomVars(i) == 'NCbio') then
      tabValVars(i) = sc%NCbio
      CYCLE
    endif
    if (tabNomVars(i) == 'Ndenit') then
      tabValVars(i) = soil%Ndenit
      CYCLE
    endif
    if (tabNomVars(i) == 'Nexporte') then
      tabValVars(i) = p%Nexporte
      CYCLE
    endif
    if (tabNomVars(i) == 'nfruit(1)') then
      tabValVars(i) = p%nfruit(aoas,1)
      CYCLE
    endif
    if (tabNomVars(i) == 'nfruit(2)') then
      tabValVars(i) = p%nfruit(aoas,2)
      CYCLE
    endif
    if (tabNomVars(i) == 'nfruit(3)') then
      tabValVars(i) = p%nfruit(aoas,3)
      CYCLE
    endif
    if (tabNomVars(i) == 'nfruit(4)') then
      tabValVars(i) = p%nfruit(aoas,4)
      CYCLE
    endif
    if (tabNomVars(i) == 'nfruit(5)') then
      tabValVars(i) = p%nfruit(aoas,5)
      CYCLE
    endif
    if (tabNomVars(i) == 'nfruit(nboite)') then
      tabValVars(i) = p%nfruit(aoas,p%P_nboite)
      CYCLE
    endif
    if (tabNomVars(i) == 'nfruit(nboite-1)') then
      tabValVars(i) = p%nfruit(aoas,p%P_nboite-1)
      CYCLE
    endif
    if (tabNomVars(i) == 'nfruitnou') then
      tabValVars(i) = p%nfruitnou(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nhuma') then
      tabValVars(i) = sc%Nhuma
      CYCLE
    endif
    if (tabNomVars(i) == 'Nhumi') then
      tabValVars(i) = sc%Nhumi
      CYCLE
    endif
    if (tabNomVars(i) == 'Nhumt') then
      tabValVars(i) = sc%Nhumt
      CYCLE
    endif
    if (tabNomVars(i) == 'nitetcult(n)') then
      tabValVars(i) = float(sc%nitetcult(n))
      CYCLE
    endif
    if (tabNomVars(i) == 'nitrifj') then
      tabValVars(i) = soil%nitrifj
      CYCLE
    endif
    if (tabNomVars(i) == 'Nmineral_from_plt') then
      tabValVars(i) = sc%Nmineral_from_plt
      CYCLE
    endif
    if (tabNomVars(i) == 'Nmulch') then
      tabValVars(i) = sc%Nmulch
      CYCLE
    endif
    if (tabNomVars(i) == 'Nmulchdec') then
      tabValVars(i) = sc%Nmulchdec
      CYCLE
    endif
    if (tabNomVars(i) == 'Nmulchnd') then
      tabValVars(i) = sc%Nmulchnd
      CYCLE
    endif
    if (tabNomVars(i) == 'Nnondec(1)') then
      tabValVars(i) = sc%Nnondec(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nnondec(10)') then
      tabValVars(i) = sc%Nnondec(10)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nnondec(2)') then
      tabValVars(i) = sc%Nnondec(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nnondec(3)') then
      tabValVars(i) = sc%Nnondec(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nnondec(4)') then
      tabValVars(i) = sc%Nnondec(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nnondec(5)') then
      tabValVars(i) = sc%Nnondec(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nnondec(6)') then
      tabValVars(i) = sc%Nnondec(6)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nnondec(7)') then
      tabValVars(i) = sc%Nnondec(7)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nnondec(8)') then
      tabValVars(i) = sc%Nnondec(8)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nnondec(9)') then
      tabValVars(i) = sc%Nnondec(9)
      CYCLE
    endif
    if (tabNomVars(i) == 'nodn') then
      tabValVars(i) = sc%nodn
      CYCLE
    endif
    if (tabNomVars(i) == 'Norgeng') then
      tabValVars(i) = soil%Norgeng
      CYCLE
    endif
    if (tabNomVars(i) == 'Nr') then
      tabValVars(i) = sc%Nr
      CYCLE
    endif
    if (tabNomVars(i) == 'Nrac') then
      tabValVars(i) = p%Nrac
      CYCLE
    endif
    if (tabNomVars(i) == 'Nrecycle') then
      tabValVars(i) = p%Nrecycle
      CYCLE
    endif
    if (tabNomVars(i) == 'Nresiduprofil(1)') then
      tabValVars(i) = sc%Nresiduprofil(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nresiduprofil(10)') then
      tabValVars(i) = sc%Nresiduprofil(10)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nresiduprofil(2)') then
      tabValVars(i) = sc%Nresiduprofil(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nresiduprofil(3)') then
      tabValVars(i) = sc%Nresiduprofil(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nresiduprofil(4)') then
      tabValVars(i) = sc%Nresiduprofil(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nresiduprofil(5)') then
      tabValVars(i) = sc%Nresiduprofil(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nresiduprofil(6)') then
      tabValVars(i) = sc%Nresiduprofil(6)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nresiduprofil(7)') then
      tabValVars(i) = sc%Nresiduprofil(7)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nresiduprofil(8)') then
      tabValVars(i) = sc%Nresiduprofil(8)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nresiduprofil(9)') then
      tabValVars(i) = sc%Nresiduprofil(9)
      CYCLE
    endif
    if (tabNomVars(i) == 'Ntousresidusprofil') then
      tabValVars(i) = sc%Ntousresidusprofil
      CYCLE
    endif
    if (tabNomVars(i) == 'numcoupe') then
      tabValVars(i) = float(p%numcoupe)
      CYCLE
    endif
    if (tabNomVars(i) == 'numcult') then
      tabValVars(i) = float(sc%numcult)
      CYCLE
    endif
    if (tabNomVars(i) == 'Nvolat_from_plt') then
      tabValVars(i) = sc%Nvolat_from_plt
      CYCLE
    endif
    if (tabNomVars(i) == 'Nvoleng') then
      tabValVars(i) = soil%Nvoleng
      CYCLE
    endif
    if (tabNomVars(i) == 'offrenod') then
      tabValVars(i) = p%offrenod(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'p1000grain') then
      tabValVars(i) = p%p1000grain
      CYCLE
    endif
    if (tabNomVars(i) == 'pdsfruit(1)') then
      tabValVars(i) = p%pdsfruit(aoas,1)
      CYCLE
    endif
    if (tabNomVars(i) == 'pdsfruit(2)') then
      tabValVars(i) = p%pdsfruit(aoas,2)
      CYCLE
    endif
    if (tabNomVars(i) == 'pdsfruit(3)') then
      tabValVars(i) = p%pdsfruit(aoas,3)
      CYCLE
    endif
    if (tabNomVars(i) == 'pdsfruit(4)') then
      tabValVars(i) = p%pdsfruit(aoas,4)
      CYCLE
    endif
    if (tabNomVars(i) == 'pdsfruit(5)') then
      tabValVars(i) = p%pdsfruit(aoas,5)
      CYCLE
    endif
    if (tabNomVars(i) == 'pdsfruit(nboite)') then
      tabValVars(i) = p%pdsfruit(aoas,p%P_nboite)
      CYCLE
    endif
    if (tabNomVars(i) == 'pdsfruit(nboite-1)') then
      tabValVars(i) = p%pdsfruit(aoas,p%P_nboite-1)
      CYCLE
    endif
    if (tabNomVars(i) == 'pdsfruitfrais') then
      tabValVars(i) = p%pdsfruitfrais(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'penfruit') then
      tabValVars(i) = p%penfruit(aoas)
      CYCLE
    endif
  !!!  if (tabNomVars(i) == 'pfeuil(n)') then
  !!!    tabValVars(i) = p%pfeuil(aoas,n)
  !!!    CYCLE
  !!!  endif
    if (tabNomVars(i) == 'pfeuiljaune') then
      tabValVars(i) = p%pfeuiljaune(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'pfeuilverte(n)') then
      tabValVars(i) = p%pfeuilverte(aoas,n)
      CYCLE
    endif
    if (tabNomVars(i) == 'phoi') then
      tabValVars(i) = c%phoi
      CYCLE
    endif
    if (tabNomVars(i) == 'pHvol') then
      tabValVars(i) = soil%pHvol
      CYCLE
    endif
    if (tabNomVars(i) == 'pousfruit') then
      tabValVars(i) = p%pousfruit(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'poussracmoy') then
      tabValVars(i) = p%poussracmoy
      CYCLE
    endif
    if (tabNomVars(i) == 'precip') then
      tabValVars(i) = sc%precip
      CYCLE
    endif
    if (tabNomVars(i) == 'precipjN') then
      tabValVars(i) = sc%precipjN
      CYCLE
    endif
    if (tabNomVars(i) == 'precipN') then
      tabValVars(i) = sc%precipN
      CYCLE
    endif
    if (tabNomVars(i) == 'preserve') then
      tabValVars(i) = p%preserve(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'profexteau') then
      tabValVars(i) = p%profexteau
      CYCLE
    endif
    if (tabNomVars(i) == 'profextN') then
      tabValVars(i) = p%profextN
      CYCLE
    endif
    if (tabNomVars(i) == 'profnappe') then
      tabValVars(i) = soil%profnappe
      CYCLE
    endif
    if (tabNomVars(i) == 'psibase') then
      tabValVars(i) = p%psibase
      CYCLE
    endif
    if (tabNomVars(i) == 'ptigestruc') then
      tabValVars(i) = p%ptigestruc(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'QCapp') then
      tabValVars(i) = sc%QCapp
      CYCLE
    endif
    if (tabNomVars(i) == 'QCO2hum') then
      tabValVars(i) = sc%QCO2hum
      CYCLE
    endif
    if (tabNomVars(i) == 'QCO2mul') then
      tabValVars(i) = sc%QCO2mul
      CYCLE
    endif
    if (tabNomVars(i) == 'QCO2res') then
      tabValVars(i) = sc%QCO2res
      CYCLE
    endif
    if (tabNomVars(i) == 'QCO2sol') then
      tabValVars(i) = sc%QCO2sol
      CYCLE
    endif
    if (tabNomVars(i) == 'QCplantetombe') then
      tabValVars(i) = p%QCplantetombe(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'QCprimed') then
      tabValVars(i) = sc%QCprimed
      CYCLE
    endif
    if (tabNomVars(i) == 'QCrac') then
      tabValVars(i) = p%QCrac
      CYCLE
    endif
    if (tabNomVars(i) == 'QCresorg') then
      tabValVars(i) = sc%QCresorg
      CYCLE
    endif
    if (tabNomVars(i) == 'QCressuite') then
      tabValVars(i) = p%QCressuite
      CYCLE
    endif
    if (tabNomVars(i) == 'QCrogne') then
      tabValVars(i) = p%QCrogne
      CYCLE
    endif
    if (tabNomVars(i) == 'Qdrain') then
      tabValVars(i) = soil%Qdrain
      CYCLE
    endif
    if (tabNomVars(i) == 'Qdraincum') then
      tabValVars(i) = soil%Qdraincum
      CYCLE
    endif
    if (tabNomVars(i) == 'Qem_N2O') then
      tabValVars(i) = sc%Qem_N2O
      CYCLE
    endif
    if (tabNomVars(i) == 'Qem_N2Oden') then
      tabValVars(i) = sc%Qem_N2Oden
      CYCLE
    endif
    if (tabNomVars(i) == 'Qem_N2Onit') then
      tabValVars(i) = sc%Qem_N2Onit
      CYCLE
    endif
    if (tabNomVars(i) == 'Qfix') then
      tabValVars(i) = p%Qfix(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'Qles') then
      tabValVars(i) = sc%Qles
      CYCLE
    endif
    if (tabNomVars(i) == 'Qlesd') then
      tabValVars(i) = soil%Qlesd
      CYCLE
    endif
    if (tabNomVars(i) == 'Qminh') then
      tabValVars(i) = sc%Qminh
      CYCLE
    endif
    if (tabNomVars(i) == 'Qminr') then
      tabValVars(i) = sc%Qminr
      CYCLE
    endif
    if (tabNomVars(i) == 'qmulch') then
      tabValVars(i) = sc%qmulch
      CYCLE
    endif
    if (tabNomVars(i) == 'QNapp') then
      tabValVars(i) = sc%QNapp
      CYCLE
    endif
    if (tabNomVars(i) == 'QNdenit') then
      tabValVars(i) = soil%QNdenit
      CYCLE
    endif
    if (tabNomVars(i) == 'QNdenit_from_plt') then
      tabValVars(i) = sc%QNdenit_from_plt
      CYCLE
    endif
    if (tabNomVars(i) == 'QNgrain') then
      tabValVars(i) = p%QNgrain(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'Qnitrif') then
      tabValVars(i) = sc%Qnitrif
      CYCLE
    endif
    if (tabNomVars(i) == 'QNorgeng') then
      tabValVars(i) = soil%QNorgeng
      CYCLE
    endif
    if (tabNomVars(i) == 'QNplante') then
      tabValVars(i) = p%QNplante(aoas,n)
      CYCLE
    endif
    if (tabNomVars(i) == 'QNplantetombe') then
      tabValVars(i) = p%QNplantetombe(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'QNprimed') then
      tabValVars(i) = sc%QNprimed
      CYCLE
    endif
    if (tabNomVars(i) == 'QNrac') then
      tabValVars(i) = p%QNrac
      CYCLE
    endif
    if (tabNomVars(i) == 'QNresorg') then
      tabValVars(i) = sc%QNresorg
      CYCLE
    endif
    if (tabNomVars(i) == 'QNressuite') then
      tabValVars(i) = p%QNressuite
      CYCLE
    endif
    if (tabNomVars(i) == 'QNrogne') then
      tabValVars(i) = p%QNrogne
      CYCLE
    endif
    if (tabNomVars(i) == 'QNvoleng') then
      tabValVars(i) = soil%QNvoleng
      CYCLE
    endif
    if (tabNomVars(i) == 'QNvolorg') then
      tabValVars(i) = soil%QNvolorg
      CYCLE
    endif
    if (tabNomVars(i) == 'qres_pature') then
      tabValVars(i) = sc%qres_pature
      CYCLE
    endif
    if (tabNomVars(i) == 'Qressuite') then
      tabValVars(i) = p%Qressuite
      CYCLE
    endif
    if (tabNomVars(i) == 'ra_recal') then
      tabValVars(i) = sta%ra_recal
      CYCLE
    endif
    if (tabNomVars(i) == 'raint') then
      tabValVars(i) = p%raint(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'ras') then
      tabValVars(i) = sc%ras
      CYCLE
    endif
    if (tabNomVars(i) == 'Ratm') then
      tabValVars(i) = sc%Ratm
      CYCLE
    endif
    if (tabNomVars(i) == 'rc') then
      tabValVars(i) = p%rc
      CYCLE
    endif
    if (tabNomVars(i) == 'rdif') then
      tabValVars(i) = sc%rdif
      CYCLE
    endif
    if (tabNomVars(i) == 'remobilj') then
      tabValVars(i) = p%remobilj(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'remontee') then
      tabValVars(i) = soil%remontee
      CYCLE
    endif
    if (tabNomVars(i) == 'rendementsec') then
      tabValVars(i) = p%rendementsec
      CYCLE
    endif
    if (tabNomVars(i) == 'resmes') then
      tabValVars(i) = sc%resmes
      CYCLE
    endif
    if (tabNomVars(i) == 'resperenne') then
      tabValVars(i) = p%resperenne(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'resrac') then
      tabValVars(i) = p%resrac
      CYCLE
    endif
    if (tabNomVars(i) == 'rfpi') then
      tabValVars(i) = p%rfpi
      CYCLE
    endif
    if (tabNomVars(i) == 'rfvi') then
      tabValVars(i) = p%rfvi
      CYCLE
    endif
    if (tabNomVars(i) == 'rlj') then
      tabValVars(i) = p%rlj
      CYCLE
    endif
    if (tabNomVars(i) == 'rltot') then
      tabValVars(i) = p%rltot
      CYCLE
    endif
    if (tabNomVars(i) == 'rmaxi') then
      tabValVars(i) = p%rmaxi
      CYCLE
    endif
    if (tabNomVars(i) == 'rnet') then
      tabValVars(i) = sc%rnet
      CYCLE
    endif
    if (tabNomVars(i) == 'rnetS') then
      tabValVars(i) = sc%rnetS
      CYCLE
    endif
    if (tabNomVars(i) == 'rombre') then
      tabValVars(i) = p%rombre
      CYCLE
    endif
    if (tabNomVars(i) == 'rsoleil') then
      tabValVars(i) = p%rsoleil
      CYCLE
    endif
    if (tabNomVars(i) == 'RsurRU') then
      tabValVars(i) = sc%RsurRU
      CYCLE
    endif
    if (tabNomVars(i) == 'RsurRUrac') then
      tabValVars(i) = p%RsurRUrac
      CYCLE
    endif
    if (tabNomVars(i) == 'RU') then
      tabValVars(i) = sc%RU
      CYCLE
    endif
    if (tabNomVars(i) == 'ruissel') then
      tabValVars(i) = sc%ruissel
      CYCLE
    endif
    if (tabNomVars(i) == 'ruisselsurf') then
      tabValVars(i) = sc%ruisselsurf
      CYCLE
    endif
    if (tabNomVars(i) == 'ruisselt') then
      tabValVars(i) = sc%ruisselt
      CYCLE
    endif
    if (tabNomVars(i) == 'runoff_from_plt') then
      tabValVars(i) = sc%runoff_from_plt
      CYCLE
    endif
    if (tabNomVars(i) == 'RUrac') then
      tabValVars(i) = p%RUrac
      CYCLE
    endif
    if (tabNomVars(i) == 'saturation') then
      tabValVars(i) = sc%saturation
      CYCLE
    endif
    if (tabNomVars(i) == 'senfac') then
      tabValVars(i) = p%senfac(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'sla') then
      tabValVars(i) = p%sla(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'SoilAvW') then
      tabValVars(i) = sc%SoilAvW
      CYCLE
    endif
    if (tabNomVars(i) == 'SoilNM') then
      tabValVars(i) = sc%SoilNM
      CYCLE
    endif
    if (tabNomVars(i) == 'SoilWatM') then
      tabValVars(i) = sc%SoilWatM
      CYCLE
    endif
    if (tabNomVars(i) == 'somcour') then
      tabValVars(i) = p%somcour
      CYCLE
    endif
    if (tabNomVars(i) == 'somcourdrp') then
      tabValVars(i) = p%somcourdrp
      CYCLE
    endif
    if (tabNomVars(i) == 'somcourfauche') then
      tabValVars(i) = p%somcourfauche
      CYCLE
    endif
    if (tabNomVars(i) == 'somcourmont') then
      tabValVars(i) = p%somcourmont
      CYCLE
    endif
    if (tabNomVars(i) == 'somdifftculttair') then
      tabValVars(i) = c%somdifftculttair
      CYCLE
    endif
    if (tabNomVars(i) == 'somtemp') then
      tabValVars(i) = p%somtemp
      CYCLE
    endif
    if (tabNomVars(i) == 'somudevair') then
      tabValVars(i) = p%somudevair
      CYCLE
    endif
    if (tabNomVars(i) == 'somudevcult') then
      tabValVars(i) = p%somudevcult
      CYCLE
    endif
    if (tabNomVars(i) == 'somupvtsem') then
      tabValVars(i) = p%somupvtsem
      CYCLE
    endif
    if (tabNomVars(i) == 'sourcepuits') then
      tabValVars(i) = p%sourcepuits(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'spfruit') then
      tabValVars(i) = p%spfruit(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'splai') then
      tabValVars(i) = p%splai(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'stemflow') then
      tabValVars(i) = p%stemflow
      CYCLE
    endif
    if (tabNomVars(i) == 'str1intercoupe') then
      tabValVars(i) = p%str1intercoupe
      CYCLE
    endif
    if (tabNomVars(i) == 'str2intercoupe') then
      tabValVars(i) = p%str2intercoupe
      CYCLE
    endif
    if (tabNomVars(i) == 'stu1intercoupe') then
      tabValVars(i) = p%stu1intercoupe
      CYCLE
    endif
    if (tabNomVars(i) == 'stu2intercoupe') then
      tabValVars(i) = p%stu2intercoupe
      CYCLE
    endif
    if (tabNomVars(i) == 'sucre') then
      tabValVars(i) = p%sucre(aoas)
      CYCLE
    endif
   !!! if (tabNomVars(i) == 'sucre_percent') then
   !!!   tabValVars(i) = p%sucre_percent(aoas)
   !!!   CYCLE
   !!! endif
    if (tabNomVars(i) == 'surf(ao)') then
      tabValVars(i) = p%surf(ao)
      CYCLE
    endif
    if (tabNomVars(i) == 'surf(as)') then
      tabValVars(i) = p%surf(as)
      CYCLE
    endif
    if (tabNomVars(i) == 'swfac') then
      tabValVars(i) = p%swfac(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'swfac1moy') then
      tabValVars(i) = p%swfac1moy
      CYCLE
    endif
    if (tabNomVars(i) == 'swfac2moy') then
      tabValVars(i) = p%swfac2moy
      CYCLE
    endif
    if (tabNomVars(i) == 'tairveille') then
      tabValVars(i) = sc%tairveille
      CYCLE
    endif
    if (tabNomVars(i) == 'tauxcouv(n)') then
    if ( p%P_codelaitr /= 1) then
      tabValVars(i) = sc%tauxcouv(n)
    else
      tabValVars(i) = p%lai(aoas,n)
    endif
      CYCLE
    endif
    if (tabNomVars(i) == 'tcult') then
      tabValVars(i) = sc%tcult
      CYCLE
    endif
    if (tabNomVars(i) == 'tcultmax') then
      tabValVars(i) = sc%tcultmax
      CYCLE
    endif
    if (tabNomVars(i) == 'tcultmin') then
      tabValVars(i) = sc%tcultmin
      CYCLE
    endif
    if (tabNomVars(i) == 'tempeff') then
      tabValVars(i) = p%tempeff
      CYCLE
    endif
    if (tabNomVars(i) == 'tetp(n)') then
      tabValVars(i) = c%tetp(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'tetstomate') then
      tabValVars(i) = p%tetstomate
      CYCLE
    endif
    if (tabNomVars(i) == 'teturg') then
      tabValVars(i) = p%teturg
      CYCLE
    endif
    if (tabNomVars(i) == 'tmax(n)') then
      tabValVars(i) = c%tmax(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'tmaxext(n)') then
      tabValVars(i) = c%tmaxext(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'tmin(n)') then
      tabValVars(i) = c%tmin(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'tminext(n)') then
      tabValVars(i) = c%tminext(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'tmoy(n)') then
      tabValVars(i) = c%tmoy(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'tmoyext(n)') then
      tabValVars(i) = c%tmoyext(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'tmoyIpltJuin') then
      tabValVars(i) = p%tmoyIpltJuin
      CYCLE
    endif
    if (tabNomVars(i) == 'tmoyIpltSept') then
      tabValVars(i) = p%tmoyIpltSept
      CYCLE
    endif
    if (tabNomVars(i) == 'tncultmat') then
      tabValVars(i) = c%tncultmat
      CYCLE
    endif
    if (tabNomVars(i) == 'tnhc') then
      tabValVars(i) = sc%tnhc
      CYCLE
    endif
    if (tabNomVars(i) == 'tnrc') then
      tabValVars(i) = sc%tnrc
      CYCLE
    endif
    if (tabNomVars(i) == 'totapN') then
      tabValVars(i) = sc%totapN
      CYCLE
    endif
    if (tabNomVars(i) == 'totapNres') then
      tabValVars(i) = sc%totapNres
      CYCLE
    endif
    if (tabNomVars(i) == 'totir') then
      tabValVars(i) = sc%totir
      CYCLE
    endif
    if (tabNomVars(i) == 'tpm(n)') then
      tabValVars(i) = c%tpm(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'trg(n)') then
      tabValVars(i) = c%trg(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'trgext(n)') then
      tabValVars(i) = c%trgext(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'trr(n)') then
      tabValVars(i) = c%trr(n)
      CYCLE
    endif
    !!!MODIF HISAFE 7 : déplacement de variables
    !!!déplacement de Stics.f90 vers Sol.f90
    if (tabNomVars(i) == 'TS(1)') then
      !!!tabValVars(i) = sc%TS(1)
      tabValVars(i) = soil%TS(1)
      CYCLE
    endif
    if (tabNomVars(i) == 'TS(2)') then
      !!!tabValVars(i) = sc%TS(2)
      tabValVars(i) = soil%TS(2)
      CYCLE
    endif
    if (tabNomVars(i) == 'TS(3)') then
      !!!tabValVars(i) = sc%TS(3)
      tabValVars(i) = soil%TS(3)
      CYCLE
    endif
    if (tabNomVars(i) == 'TS(4)') then
      !!!tabValVars(i) = sc%TS(4)
      tabValVars(i) = soil%TS(4)
      CYCLE
    endif
    if (tabNomVars(i) == 'TS(5)') then
      !!!tabValVars(i) = sc%TS(5)
      tabValVars(i) = soil%TS(5)
      CYCLE
    endif
    if (tabNomVars(i) == 'turfac') then
      tabValVars(i) = p%turfac(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'turfac1moy') then
      tabValVars(i) = p%turfac1moy
      CYCLE
    endif
    if (tabNomVars(i) == 'turfac2moy') then
      tabValVars(i) = p%turfac2moy
      CYCLE
    endif
    if (tabNomVars(i) == 'tustress') then
      tabValVars(i) = sc%tustress
      CYCLE
    endif
    if (tabNomVars(i) == 'tvent(n)') then
      tabValVars(i) = c%tvent(n)
      CYCLE
    endif
    if (tabNomVars(i) == 'udevair') then
      tabValVars(i) = p%udevair
      CYCLE
    endif
    if (tabNomVars(i) == 'udevcult') then
      tabValVars(i) = p%udevcult
      CYCLE
    endif
    if (tabNomVars(i) == 'ulai(n)') then
      tabValVars(i) = p%ulai(n)
      CYCLE
    endif

    if (tabNomVars(i) == 'vitmoy') then
      tabValVars(i) = p%vitmoy(aoas)
      CYCLE
    endif
    if (tabNomVars(i) == 'xmlch1') then
      tabValVars(i) = sc%xmlch1
      CYCLE
    endif
    if (tabNomVars(i) == 'zrac') then
      tabValVars(i) = p%zrac
      CYCLE
    endif
! ajout DR 22042016 pour les CAS
    if (tabNomVars(i) == 'efdensite_rac') then
      tabValVars(i) = p%efdensite_rac
      CYCLE
    endif
! ajout DR 22042016 pour les prairies
    if (tabNomVars(i) == 'day_cut') then
      tabValVars(i) = p%day_cut
      CYCLE
    endif
    if (tabNomVars(i) == 'QNexport') then
      tabValVars(i) = p%QNexport
      CYCLE
    endif
! DR 31/05/2016 pour AgMIP ET
    if (tabNomVars(i) == 'day_after_sowing') then
      tabValVars(i) = sc%day_after_sowing
      CYCLE
    endif
	
      call EnvoyerMsgHistorique('')
      write(tmp,*) nom, ': unknown variable name (check case sensitivity)'
      call EnvoyerMsgHistorique(tmp)
    end do B1
return
end subroutine CorrespondanceVariablesDeSorties
