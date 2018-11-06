!> This subroutine contains the messages written to the file mod_b*.sti
subroutine bilans_messages_ENG

USE Bilans

implicit none


mes1008 = "(' Balance of the STICS simulation ',a50,', model ',A7/1x,48('*'))"
mes1009 = "(' 1. INPUT DATA',/1x,19('*'))"
! dr 16/07/2012 on met le nom de station sur 30 pour prendre les noms longs de agmip
mes1010 = "(3x,'Weather file            : ',A50)"
mes1011 = "(3x,'Cultural practices file : ',A50)"
mes10111 = "(3x,'Initialisations file : ',A50)"
mes1012 = "(3x,'Plant file                : ',A50,6x,'Variety : ',a10)"
mes1013 = "(3x,'lai   file                : ',A12)"
mes1014 = "(3x,'Initial soil values      : ',A12,/,6x,'Z (cm)',6x,'Water (%)      NO3 (kg/ha)    NH4 (kg/ha)')"
mes1020 = "(3x,'Initial plant values      : ',/,3x,'stage    LAI     MS   ZRAC MAGRAIN QNPLANTE INN')"
mes1021 = "(/3x,'Start of simulation :',I3,'-',A3,'-',I4,6x,'day',I4)"
mes1022 = "( 3x,'End of simulation   :',I3,'-',A3,'-',I4,6x,'day',i4,3x,'(or',i4,')')"
mes1023 = "(/3x,'Irrigation:',11x,'Number of irrigations =',i3)"
mes1025 = "(5x,'10-day period of irrigation',8x,'amount (mm)',/, 5x,'-----------------' ,8x,'---------')"
mes1024 = "(5x,'date of irrigation',  8x,'amount (mm)',/,5x,'---------------',   12x,'---------')"
mes1125 = "(5x,'Total quantity applied:',F8.1,' mm')"
mes1026 = "(/3x,'Fertilisation:',8x,'Number of applications =',i2,:,5x,'Type of fertiliser =',i2)"
mes1027 = "(3x,'date of application N',7x,'amount (kg N/ha)',7x,'type of fertilizer')"
!            & /5x, 15('-'),12x,14('-'),10x,14('-'))"
mes1027b = "(5x, 15('-'),12x,14('-'),10x,14('-'))"
mes1028 = "(27x,'total',f7.0)"
mes1029 = "(/3x,'Organic residues and/or soil tillage')"
mes1030 = "(9x,'No application, no tillage')"
mes1031 = "(5x,'Tillage ',7x,   'day',i4,4x,'on',f4.0,'cm')"
mes1032 = "(5x,'Residues of type',i3,4x,'day',i4,4x,'on',f4.0,'cm',4x,'DM=',f4.1,' t/ha',4x,'C/N=',f4.0)"
mes1033 = "(//,' 2. CROP DEVELOPMENT',/,1x,30('*'))"
mes1040 = "(5x,'type                   : long-day plant')"
mes1041 = "(5x,'type                   : short day plant')"
mes1042 = "(5x,'development unit : vernalo-photo-thermic')"
mes1043 = "(5x,'development unit : photo-thermic')"
mes1044 = "(5x,'development unit : thermic')"
mes1045 = "(5x,'temperature used : air temperature')"
mes1046 = "(5x,'temperature used : crop temperature')"
mes1050 = "(/5x,'Vernalisation or dormancy impossible: it is too hot')"
!mes1051 = "(/8x,'stage',57x,'date',11x,'units',5x,'cumulative units',/5x,11('-'),7x,53('-'),9x,6('-'),5x,15('-'))"
mes1052 = "(5x,'initial stage : ',a3)"
mes1053 = "('   Vegetative stages')"
mes1054 = "('   Reproductive stages')"
mes1055 = "(/,'    Length of cycle :',i4,' days')"
mes1056 = "(/,' Attention: for this simulation the date of harvest is a finishing date',/,&
    &' the sum of units from sowing till harvest can not have been reached on this date')"
mes1057 = "(//,' 3. GROWTH AND COMPONENTS OF YIELD'/1x,41('*'))"
mes1157 = "(3x,' ',I3,'-',A3,'-',I4)"
mes1058 = "(3x,'Aerial biomass at harvest (0% water)  =',f8.2,' t/ha'/3x,   &
    &'Yield grain or fruit   (0% water)       =',f8.2,&
    &' t/ha'/3x,   'Yield grain or fruit (',f3.0,'% water)       =',f8.2,' t/ha'//3x,&
    &   'Number of grains or fruits              =',f8.0,' /m2')"
mes1059 = "(3x,'Plant density                         =',f8.1,' /m2'/3x,'Weight of grain, fruit (',f3.0,'% water)   = ',f9.3,' g')"
mes1160 = "(3x,'Number of inflorescences              =',f8.1)"
mes1161 = "(3x,'Number of unripe fruits =',f8.0,'/m2, or a weight',' dry =',f8.2,'t/ha')"
mes1159 = "(3x,'Growth rate (lag phase)           =', f8.1,' mg/m2/j')"
mes1360 = "(3x,'Pruning Weight  (0% water)            =',f8.2,' t/ha')"
mes1361 = "(3x,'Reserves at the end of simulation (0% water)   =',f8.2,' t/ha')"
mes2092 = "(/3x,'Number of emerged leaves              =',i6)"
mes2100 = "( 3x,'Number of hot      or cold days       =',i6)"
mes1060 = "(3x,'Senescent aerial biomass (0% water)   =',f7.2,' t/ha')"
mes1061 = "(/3x,'Quantity of N in the crop             =',f8.0,' kg/ha'/3x,&
    &'Quantity of N in the grain and fruit  =',f8.0,' kg/ha'//3x,&
    &'N content of entire plant             =',f6.2,' % DM')"
mes1461 = "( 3x,'N content of grain, fruit             =',f6.2,' % DM',/3x,&
    &'Protein content of grain, fruit       =',f6.1,' % DM')"
mes1258 = "(/3x,'Nitrogen fertiliser efficiency        =',f5.2)"
mes1158 = "(/3x,'Harvest index                         =',f5.2)"
mes1261 = "(/2x,'Composition of the dry matter of grain/fruit',/3x,&
    &'Dry matter content                    =',f6.0,' % FW'/3x,&
    &'Nitrogen content                      =',f6.2,' % FW')"
mes1262 = "( 3x,'Sugar content                         =',f6.1,' % FW')"
mes1263 = "( 3x,'Oil content                           =',f6.1,' % FW')"


mes1063 = "('4. WATER and NITROGEN BALANCE over the crop life',/,48('*'))"
mes1064 = "(29x,'Sum of Maximal ET (eos+eop) =',f6.0,' mm'/29x,'Sum of AET =',f6.0,' mm',&
    &/29x,'Sum of Soil Ev. =',f6.0,' mm'/29x,'Sum of TRansp.  =',f6.0,' mm',/29x,&
    &'Sum of Rain + Irrigation   =',f6.0,' mm'/)"
mes1065 = "(5X,'Maximum water reserve used by plant  =',f6.0,' mm',/5x,&
    &'Maximum rooting depth     =',f6.0,' cm')"
mes1066 = "(/2x,'Mean STRESS indices :                swfac   turfac','   inns   tcult-tair  exofac'&
          &,'    etr/etm    etm/etr',/,3x,'vegetative phase    (lev-drp) ',7f10.2&
          &,/,3x,'reproductive phase  (drp-mat) ',7f10.2)"


mes1166 = "(/3x,'Frost damage on leaves : before AMF  ',f5.0,'%',4x,&
    &'after AMF',f5.0,'%')"
mes1167 = "( 3x,'Plant frosted and no reserve')"
mes1168 = "( 3x,'Frost damage to flowers or fruit     ',f5.0,'%')"
mes1067 = "('Nitrogen stress calculated but inactive on the plants')"
mes1068 = "('Water stress calculated but inactive on the plants')"
mes1069 = "(/,/,'5. WATER, NITROGEN, CARBON BALANCE over the whole simulation period  (',i3,' days)'/,79('*'))"
mes1070 = "(3x,'Normalised days at',f4.0,'degrees C :',4x,'Humus =',f5.0,6x,'Residues =',f5.0)"
mes4070 = "(3x,'Potential mineralisation rate =',f5.2,' kg N/ha/day',3x,'or',f5.2,'% per year')"
mes1072 = "('WATER BALANCE  (mm)')"
mes1073 = "(&
     &  5x,'initial water content  ',f6.0,8x,'final water content    ',f6.0,&
     & /5x,'rain                   ',f6.0,8x,'evaporation            ',f6.0,&
     & /5x,'irrigation             ',f6.0,8x,'transpiration          ',f6.0,&
     & /5x,'capillary rise         ',f6.0,8x,'runoff                 ',f6.0,&
     & /42x,                                 'deep infiltration      ',f6.0,&
     & /42x,                                 'mole drainage          ',f6.0,&
     & /42x,                                 'leaf interception      ',f6.0,&
     & /42x,                                 'mulch interception     ',f6.0,&
     & /42x,                                 'ineffective irrigation ',f6.0,&
     & /5x,29('-'),8x,29('-'),&
     & /5x,'TOTAL INPUTS           ',f6.0,8x,'TOTAL OUTPUTS          ',f6.0)"
!Elsa 30/07/2012 on change l'ordre des lignes pour faire apparaitre comme dans les bilans organiques
!les pools dans les premieres lignes, puis les entrées, puis les sorties
!mes1074 = "(/'MINERAL NITROGEN BALANCE (kg N/ha)')"
!mes1075 = "(5x,&
!          &'rain                   ',f6.0,8x,'crop removal           ',f6.0,/5x,&
!          &'irrigation             ',f6.0,8x,'crop return            ',f6.0,/5x,&
!          &'fertiliser             ',f6.0,8x,'leaching               ',f6.0,/5x,&
!          &'symbiotic fixation     ',f6.0,8x,'fertiliser N immobilis.',f6.0,/5x,&
!          &'humus mineralisation   ',f6.0,8x,'fertiliser N volatilis.',f6.0,/5x,&
!          &'residue mineralisation ',f6.0,8x,'manure N volatilis.    ',f6.0,/42x,&
!                                            &'denitrification + N2O  ',f6.0,/5x, &
!          &'initial plant N        ',f6.0,8x,'leaching in mole drains',f6.0,/5x, &
!          &'initial soil NO3       ',f6.0,8x,'final soil NO3         ',f6.0,/5x, &
!          &'initial soil NH4       ',f6.0,8x,'final soil NH4         ',f6.0,/5x, &
!          & 29('-'),8x,29('-'),/5x, &
!          &'TOTAL INPUTS           ',f6.0,8x,'TOTAL OUTPUTS          ',f6.0,/)"
mes1074 = "(/'MINERAL NITROGEN BALANCE (kg N/ha)')"
mes1075 = "(5x,&
          &'initial plant N        ',f6.0,8x,'final plant N           ',f6.0,/42x,&
                                            &'N uptake exported       ',f6.0,/42x,&
                                            &'N uptake returned       ',f6.0,/5x,&
          &'initial soil NH4       ',f6.0,8x,'final soil NH4          ',f6.0,/5x,&
          &'initial soil NO3       ',f6.0,8x,'final soil NO3          ',f6.0,/5x,&
          &'rain                   ',f6.0,8x,'leaching                ',f6.0,/5x,&
          &'irrigation             ',f6.0,8x,'leaching in mole drains ',f6.0,/5x,&
          &'fertiliser             ',f6.0,8x,'fertiliser N immobilis. ',f6.0,/5x,&
          &'symbiotic fixation     ',f6.0,8x,'fertiliser N volatilis. ',f6.0,/5x,&
          &'humus mineralisation   ',f6.0,8x,'manure N volatilis.     ',f6.0,/5x,&
          &'residue mineralisation ',f6.0,8x,'N2 and N2O losses       ',f6.0,/5x,&
          & 29('-'),8x,29('-'),/5x, &
          &'TOTAL INPUTS           ',f6.0,8x,'TOTAL OUTPUTS          ',f6.0,/)"

 ! Bruno mai 2012

 mes1080 = "('ORGANIC NITROGEN BALANCE (kg N/ha)')"
 mes1081 = "(/'ORGANIC CARBON BALANCE   (kg C/ha)')"
 mes1082 = "(&
      & 5x,'Active humified pool    ',f8.0,8x,'                     ',f8.0,&
      &/5x,'Inert humified pool     ',f8.0,8x,'                     ',f8.0,&
      &/5x,'Zymogeneous biomass pool',f8.0,8x,'                     ',f8.0,&
      &/5x,'Soil residues pool      ',f8.0,8x,'                     ',f8.0,&
      &/5x,'Mulch residues pool     ',f8.0,8x,'                     ',f8.0,&
      &/5x,'Fertiliser Immobilised  ',f8.0,8x,&
      &/5x,'Added organic fertil.   ',f8.0,8x,&
      &/5x,'Added Crop residues     ',f8.0,8x,&
      &/5x,'Added Roots             ',f8.0,8x,&
      &/5x,'Added Fallen leaves     ',f8.0,8x,'Priming (PE)         ',f8.0,&
      &/5x,'Added Trimmed leaves    ',f8.0,8x,'Mineralisation - PE  ',f8.0,&
      &/5x, 31('-'),8x,29('-'),&
      &/5x,'TOTAL INITIAL           ',f8.0,8x,'TOTAL FINAL          ',f8.0)"

mes1083 = "(/,'Heterotrophic Respiration (kg C/ha): Residues =',f6.0,/,&
             &'                                     Humus    =',f6.0,/,&
             &'                                     Total    =',f6.0)"

mes1084 = "(/,'Cumulative N2O emissions  (kg N/ha): nitrification      =',f6.2,/,&
             &'                                     denitrification    =',f6.2,/,&
             &'                                     Total              =',f6.2)"

mes1062 = "(/,'CROP RESIDUES returned to soil: ',a15,6x,'DM =',f5.1,' t/ha',5x,'C/N =',f4.0)"
! domi 29/04/2002
mes3181  = "(//,'   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&  ',i4,'   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&',//)"
mes3008  = "(3x,'weather station at an altitude of    ',f6.1,' m')"
mes3009  = "(3x,'simulation at an altitude of      ',f6.1,' m')"
mes3110  = "('   adret option ')"
mes3111  = "(' ubac option ')"
mes3101   = "('     harvest num',i2,' on ',i2,'-',a3,'-',i4,': ',f7.2,'t/ha (',f8.1,' fruits/m2 -- ',f6.2,'% FW)' )"
! drainage
mes3011    = "(/,'B / Ldrains (cm) / Ksol (cm/j) / profdrain (cm)/sensanox /distdrain:',/,f5.2,1x,'/',f8.1,1x,&
    &'/',f8.1,3(1x,'/',f7.2))"
mes3015    = "('  Permeability to bottom of profile (mm) : ',f5.2)"
! bilcoupe
! --------
mes2001 = "(' Balance of the STICS simulation ',a40,', model ',A7,' forage version'/1x,67('*'))"
mes2012 = "(3x,'Plant file                : ',A15)"
mes2014 = "(5x,'variety group : ',i2)"
mes2017 = "(5x,'End of simulation   : ',I2,'-',A3,'-',I4,6x,'jour',I4)"
mes2018 = "(' In this simulation the mowing dates',' are imposed by the dates')"
mes2019 = "(' In this simulation the mowing dates',' are imposed by the temperature sum')"
mes2020 = "(' In this simulation the mowing dates',' are calculated at stage ',a3)"
mes2021 = "(' Irrigation read ')"
mes2022 = "(' Irrigations calculated with a satisfaction threshold = ',f3.1)"
mes20221 = "(' The authorized period for calculated irrigations is : ',i3,' and ',i3)"
mes2023 = "(' Fertilisation read')"
mes2024 = "(' Fertilisations calculated with a N stress <',f3.1)"
mes2025 = "(///,20x,20('%'),//,' Balance and end of crop summary '/1x,38('*'))"
mes2026 = "(/,3x,'Irrigation')"
mes2029 = "(5x,'Number of irrigations: ',I3)"
mes2030 = "(/3x,'Fertilisation')"
mes2033 = "(5x,'Total quantity applied until cutting',' :',F9.0)"
mes2034 = "(/,3x,'Residues of previous crop : no residues')"
mes2035 = "(/,3x,'Residues of the previous crop :',/5x,'quantity =',f5.1,' t/ha',9x,'C/N =',f5.0,   /5x,&
     &'incorporated on day',i4,' to a depth of',f4.0,' cm')"
mes2036 = "(/8x,'cut',15x,'date'/5x,11('-'),7x,11('-'))"
mes2038 = "(11x,'Total aerial biomass harvested(0 %)  =',f7.1,' t/ha'/)"
mes2039 = "(11x,'Quantity of nitrogen absorbed by the crop =',f5.0,' kg/ha',/11x,'[N] entire plant = ',f6.2,' %',/)"
mes2040 = "(10x,' Residues for the following crop :   ',a20,/11x,'Quantity =',f5.1,' t/ha',9x,'C/N =',f4.0//)"
mes2041 = "('4. WATER and NITROGEN BALANCES',/,'   Start cut 1 to end of simulation',/,50('*'))"
mes2064 = "(///20x,' 1.CUT NUMBER ',i2 ,/,20x,20('*')//)"
mes2065 = "('    cutting day : ',I2,'-',A3,'-',I4,' , jour ',i3)"
mes2068 = "('    What is remaining is calculated from the cut height: ',f4.3)"
mes2069 = "('    What is remaining is read in the tec file : lai ',f6.3,' ms ',f6.3)"
mes2066 = "('    the cutting date has been delayed ( aerial biomass < ',f5.2, 't/ha)')"
mes2067 = "(5x,'Total quantity applied until the cutting date',':',F8.1,' mm')"
mes2076 = "(/8x,'stage',36x,'date',11x,'units',5x,'cumulative units',5x,'lai',/5x,11('-'),28x,11('-'),9x,6('-'),&
     &5x,15('-'),5x,3('-'))"
mes2082 = "(11x,'Aerial biomass (0 %)  =',f7.1,' t/ha')"
mes2083 = "(11x,'of which:',f7.1,' t/ha green ',/11x,'et  :',f7.1,' t/ha  senescent'/)"
mes2084 = "(11x,'',f7.1,' t/ha of residual biomass coming from',' previous crop cycle',/,11x,'et  :',f7.1,&
     &' t/ha',' of newly-formed biomass')"
mes2085 = "(11x,'residual green residual biomass          :',f7.1,' t/ha')"
mes2086 = "(11x,'residual dry matter for following cycle :',f7.1,' t/ha',/)"
mes2087 = "(11x,'Plant density=',f8.2,' /m2'/)"
mes2088 = "('4. Between-cut WATER and NITROGEN BALANCE (mowing',i2,'-mowing ',i2,')',/,55('*'))"
mes2089 = "(/2x,'Mean STRESS indices :swfac   turfac','   inns   tcult-tair   exofac',/,3x,&
     &'vegetative phase   (lev-drp) ',4f8.2)"
mes2090 = "(3x,'reproductive phase (drp-mat) ',4f8.2)"
mes2091 = "(2x,'last age classes did not reach maturity')"
mes2094 = "(/,5x,'beginning of dormancy ',7x,I2,'-',A3,'-',I4)"
mes2093 = "(5x,'End of dormancy  ',7x,I2,'-',A3,'-',I4,4x,'Chill Units (Richardson)= ', f5.0)"
mes2095 = "(5x,'End of dormancy  ',7x,I2,'-',A3,'-',I4,4x,'Chill Units (Bidabe)= ', f5.0)"

! NB le08/01/02
!   lecplant
!!! en attente mes3273 = "(5(/),10X,'I cannot find the plant file ',a15)"
!!! en attente  mes3274 = "(5(/),10X,'I cannot find the technique file ',a15)"
!
! lixiv
!----------
! domi 29/04/2002
!!! en attente mes666 = "(2(/),'*** Attention, you have both a deep drainage',' and a non-zero deep infiltration value ***')"
! initial
!---------

! leclai
!---------
! domi 25/06/02
!    04/08 a enlever mes4000 = "('I cannot find the file lai : ',a20)"
! domi 29/08/03 for caro if reading dates of application in upvt
! dans bilan
 mes4001 = "('   the application dates are fixed in upvt')"
!
 mes2031 = "('In this simulation the sums of cuts are calculated in units photo-vernalo-thermic')"
 mes2032 = "('In this simulation the sums of dish are calculated in thermal units (t air)')"
! DR 08/11/05 rajout pour bilcoupe
 mes2096 = "('the cut has been delayed for',i4,' days with regard to initial date')"
 mes2098 = "(11x,'Harvested biomass(0 %)  =',f7.2,' t/ha'/)"
 mes2099 = "(i4,':', 3x,'===== Death of the plant any nitrogen ===')"
 mes2097 = "(3x,'===== Death of the plant any nitrogen =====')"


! bilan
!------
      mes410 = "('Trimming')"
      mes411 = "('Leaf thinning')"
      mes412 = "('   Method of harvest : all plant harvest')"
      mes414 = "('   Method of harvest : fruit picking at end of cycle')"
      mes415 = "('   Method of harvest : fruit picking, once every',i5,' days')"
!      mes416 = 'days'
      mes417 = "('   Harvest at physiological maturity')"
      mes418 = "('   Harvest according to water content')"
      mes419 = "('   Harvest according to sugar content')"
      mes420 = "('   Harvest according to protein content')"
      mes421 = "('   Harvest according to oil content')"
      mes422 = "('   Spreading out of harvest')"

      mes48 = "('warning harvest before maturity')"
      mes445 = "('sowing day was delayed of this number of days : ',i6)"
      mes447 = "('harvest day was delayed of this number of days : ',i6)"
      mes448 = "('  method of PET calculation ',a60)"

! DR 03/08/2015
      mes449 = "(' This year is in case of rotation and chained with the previous one ')"
     ! mes450 = "(' codeinitprec = 2, the climatic series are in succession ')"

end subroutine
 
 
