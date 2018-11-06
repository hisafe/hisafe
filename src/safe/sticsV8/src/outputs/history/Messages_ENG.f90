!>Warning messages for the file history.sti
!!
!! general parameters
subroutine remplirMessages_ENG(messages,nbMessages)

USE iso_varying_string

    implicit none

!    type(Stics_Communs_), intent(INOUT) :: sc

! Dans l'absolu, faudrait allouer le tableau ici plutot. Mais on peut pas avec la contrainte
! de l'intégration de Stics dans la plateforme ***** de Toulouse où les bornes d'un
! argument de type tableau doivent être connues et définies.
    integer,              intent(IN)    :: nbMessages  
    type(varying_string), intent(INOUT) :: messages(nbMessages)  

!TODO: Tester si messages n'a pas déjà été alloué, auquel cas, désallouer pour réallouer
!    allocate(messages(1000))


!    messages (5)    = '(///A)'
! not used   messages(10) = 'soil working depth > profhum : impossible !'
! not used   messages (48) = 'warning harvest before maturity'
! not used   messages (53) = 'You have arrived at drp and you no longer have masec'
! not used   messages (102) = ' parameter number '
! not used   messages (103) = ' has not been found'
! not used   messages (106) = 'I cannot find the file PARAM.PAR'
! not used   messages(550) = 'error in reading the file param.sti: P_zracplantule parameter does not exist!'
! not used   messages (141) = 'start the crop at least one day later'
! not used   messages (142) = 'start of simulation (defined in the USM)'
! not used   messages (143) = 'error in technique file'
! not used   messages (435) = 'think of forcing stages stades lev and lan'
! not used   messages (107) = 'sowing too deep for this variety'
! not used   messages (108) = 'inconsistencies in the photoperiod threshold'
! not used   messages (109) = 'non-zero value at stlevsenms '
! not used   messages (111) = 'codefauche=1 and msaer0 non-zero not authorised'
! not used   messages (157) = 'I enter in'
! not used   messages (158) = 'I have left the rapport'
! not used   messages (145) = 'the day of incorporating residues is'
! not used   messages (134) = 'I cannot find the plant file '

! dans bilan voir si on le mets dans decisionrecolte !   messages (447) = 'harvest day was delayed of this number of days : '

! Général
!---------
     messages (5)    = '-------------------------------------------------------------------------------'
     messages (6)    = '!!!!!!!!!!!!!!!!!!! ---- WARNING the model stop ---- !!!!!!!!!!!!!!!!!!!!!!!!!!'

! croira
! ------
    messages (20) = 'transpiration nil because root system is in dry soil, day'
    messages (23) = 'profsem > initial rooting depth !'
    messages (24) = 'inconsistency in initialising rooting front, you must have densinitial no deeper as zrac0 &
                     &it will be limited to the layer where the roots are presents'
! densirac
! --------
    messages (30) = 'the crop cannot begin growing : soil surface too dry'
    messages (31) = 'attention: there are no longer any effective roots, day :'

! develop
! --------
! ML le 28/05/04
   messages (32) = 'there is an inconsistency in the initialisations :the simulation begins at the end of &
                    &vernalisation (stage0=lev) but at a date after julvernal!'
    messages (41) = 'lax observed before amf'
    messages (42) = 'sen observed before lax'
    messages (43) = 'lan observed before sen'
    messages (44) = 'mat observed before drp'
    messages (45) = 'rec observed before mat'
    messages (46) = 'amf observed before emergence'
    messages (47) = 'drp before flo'

! Bruno
    messages (49) = 'if option hourly scale, then air temperature for driving development'
    messages (50) = 'there is an inconsistency in the initialisations: simulation begins at the start of vernalisation &
                     &(stage0=dor) but at a date before julvernal!'

! grain
! -----
    messages (51) = 'the plant has not emerged'
! iniclim
! -------
    messages (61) = 'attention: start of simulation after sowing : risk of error in output dates'
!    messages (62) = ' : risk of error in output dates'
    messages (63) = 'incomplete weather file : impossible chaining'
    messages (64) = 'the start of the simulation is earlier than the beginning of the weather file'
    messages (65) = '!! your irrigations :'
    messages (66) = 'are < the start of the simulation :'
    messages (67) = ' correct this date and start again'
    messages (68) = 'the pg is interrupted; see history.sti'
    messages (69) = '!! Your nitrogen applications :'
    messages (60) =  'The simulation runs but the day of incorporation of residues is later than the sowing date.'
! DR 13/08/2012 je rajoute ce message recupere dans message_fr avant son effacement    
    messages(260) = 'The simulation runs but the day of soil cultivation operations is later than the sowing date.'
    messages (71) = 'not the weather variables needed for the resistive model option'
    messages (72) =  'not the reference PET values : calculation of Priestley-Taylor PET'
    messages (73) = 'amf observed and vernalisation impossible so stlevamf=0 and execution error'
!    messages (74) = 'stlevamf=0 and execution error '
    messages (70) = 'attention: the simulation begins with emergence, vernalisation also begins with emergence and &
    &not with germination, as is the case for simulations beginning with sowing; you may be able to reduce jvcmini &
    &to take account of this shortage of vernalising day'

    messages (76) = 'you cannot start at the dor stage (beginning of dormancy) if your crop has no cold requirements'


! inicoupe
! --------
    messages (75) = 'lai < lairesiduel : cut is delayed'

! initial
! -------
    messages (80) = 'obstarac too big : obstarac = soil depth'
    messages (82) = 'proftrav > soil depth'
    messages (83) = 'initial stage not recorded: '
    messages (84) = 'inconsistency in initialisations'
    messages (85) = 'QNplante0 = 0 then critical value'
    messages (86) = 'INITIALISATIONS PLANT'
    messages (87) = 'inconsistency between stade0 et magrain0'
    messages (88) = 'depth of measurements) = '
    messages (89) = ' greater than soil depth) ) = '
    messages (91) = ' P_lairesiduel >0. et P_msresiduel < = 0.0'
    messages (180) = 'soils > 1000 cm are rejected !'
    messages (181) = 'then profmes = profsol'
    messages (182) = 'zesx > profsol then zesx=profsol '
    messages (171) = 'profhum > soil depth --> profhum = profsol'
    messages (172) = 'linking [or sequence?]of simulations impossible : wrong starting and finishing dates'
    messages (173) =  'Attention : a previous incomplete weather file defines the end of the current simulation'
    messages (191) = 'Attention you have chosen "rate of ground cover" option'
    messages (192) = 'water interception by foliage is not allowed'
    messages (193) = 'water requirement calculated with crop coefficient'
    messages (194) = 'the crop cannot be mowed'
    messages (195) = 'calculation of crop temperature  is forced to empirical relation'
    messages (196) = 'the perennial plant must have roots (densinitial in usm)'
    messages (197) = 'soil depth > impermeable !'
    messages (198) = 'epd (dispersion thickness, soil file) fixed at 1 cm'
    messages (199) = 'codrainage = 1 implies macroporosite error mentioned above forced the stop program'
    messages (330) = 'distdrain > ecartdrain/2'
    messages (333) =  'impossible to simulate a staggered harvest with the option chosen (nbceuille=2) '
    messages (334) = 'no fruit removal with determined plants'
    messages (335) = 'end of dormancy forced before the beginning of simulation'
    messages (336) = 'Attention humcapil  > hcc : risk of excess water'
    messages (337) = ' splaimin=splaimax or spfrmin=spfrmax'
    messages (338) = 'stade0 = snu or plt impossible with a perennial plant, put stade0 = dor for before &
                      &dormancy/vernalisation  and put stade0 = lev for bud burst or beginning of leaf growth'
    messages (339) = 'Attention no nitrogen reserve'
    messages (340) = 'problem codeinitprec=2 and no link [or sequence?]'
    messages (341) = 'Attention : You start with a bare soil therefore your plant initialisations have been&
                      & set to 0.0 (state of plant at start)'
    messages (342) = 'Attention if start at plt stage you must initialise the plant by (laiplantule and nbfeuilplant&
                      & in the parameters in the plant file)'
    messages (520) = 'problem with smoothing of water profile'

! irrig
! -----
    messages (90) = 'ATTENTION: neither rain nor irrigation at sowing'

! leclai
! ------
    messages (100) = '!!! attention !!!'
    messages (101) = 'the observed lai file is not consecutive'

! lecparam
! --------
   messages (104) = 'GENERAL PARAMETERS USEFUL FOR THE SIMULATION'
   messages (105) = 'error in file PARAM.PAR'
   messages (437) = 'STICS Version '

! lecparamv6 DR et ML le 23/03/04
! ----------
   messages(405) = 'error in file PARAMV6.PAR'
   messages(406) = 'I cannot find the file PARAMV6.PAR'
   messages(407) = 'Attention. It seems that your file paramv6_gen.xml does not correspond to the version of&
                 & stics which you are using.For your information, below is a list of the non-initialised variables. As a &
                 &precaution the programme has been stopped. If you have questions please contact stics@avignon.inra.fr'
!   messages(408) = 'when coupling Stics with the pathogen model Mila , the calculation of surface wetness &
!                    &duration is automatically activated'
   messages(408) = 'Attention the dates of beginning and ending irrigations in case of calendar imposed are bad : begin>end'


! LECPLANT  NB le 08/01/02
! --------
!   messages (110) = 'impossible to have 2 consecutive stages simultaneously'
   messages (438) = 'In CAS simulation mode, the codebeso has to be set to 2. It is forced.'
   messages (112) = 'Give a suitable value to sla (between 150 et 600)'
   messages (113) = 'rsmin strange !'
   messages (114) = 'hautmax strange !'
   messages (115) = 'hautbase strange !'
   messages (116) = 'hautmax < hautbase !'
   messages (117) = 'inconsistency in profile type'
   messages (118) = 'h2ogrmat > h2ograin !'
   messages (119) = 'low draclong : risk of execution problems'
   messages (121) = 'PLANT PARAMETERS NEEDED FOR THE SIMULATION'
   messages (122) = 'driving temperature, air '
   messages (123) = 'driving temperature, crop'
   messages (124) = 'delay water stress before DRP stage '
   messages (125) = 'temin=tcmin and temax=tcmax'
   messages (126) = 'inconsistency between codetransrad and codetradtec'
   messages (127) = 'extin used for Esol'
   messages (128) = 'root driving temperature = crop'
   messages (129) = 'root driving temperature = soil'
   messages (130) = 'impossible to simulate dormancy for annual crops'
   messages (131) = 'inconsistency between chosen initial stage, stade0=dor, and the fact that this crop has no cold requirement'
   messages (132) = 'legume'
   messages (232) = 'nitrogen effect on nb fruit and grain'
   messages (133) = 'error in plant file. '
   messages (370) = 'number of grains maxi (Nbgrmax) = 0'
   messages (371) = 'invalid stoprac'
   messages (372) = 'iplt inactive as it is a perennial plant'
   messages (373) = 'day temperature'
   messages (374) = 'hour temperature'
   messages (375) = 'with the option laibrut the residuel lai disappears entirely on the first day of senescence'
   messages (376) = 'if your crop is planted, it is better to use the initial stages stade0=snu or plt and the initialisation &
       &parameters of the seedling(laiplantule and nbfeuilplant in the plant parameter file, and masecplantule and zracplantule)'
   messages (377) = 'in the simulation mode "mixed cropping" the parameter coderacine of the plants must be 2'
   messages (378) = 'abscission parameter must be between 0 and 1, correct and run again'

! lecsol   NB le 8/1/02
! ------
   messages (135) = 'SOIL PARAMETERS '
   messages (136) = 'the soil number has not been found !!!'
   messages (137) = 'error in soil file'
   messages (138) = 'depth of humification > 60 cm: impossible'
   messages (210) = 'amount of pebbles in wrong units'
   messages (211) = 'profhum = 0 : impossible'
   messages (360) = 'moisture threshold for mineralisation : min=HMIN and opt=HCC'
   messages (361) = 'alphaco2 > 2 : impossible'
   messages (212) = 'attention strange value of hccf (<1), stopped'
   messages (213) = 'attention strange value of hminf (<1) stopped'
   messages (217) = 'attention P_hccf<P_hminf , stopped'
   messages (214) = 'profhum  must be between 0 and profsol'
   messages (215) = '(1 active , 2 inactive)'
   messages (216) = 'you selected "stones" but you did not inform their nature and quantity, check your soil'
! lecstat
! -------
   messages (139) = 'Error in file STAT.DAT '
   messages (442) = 'you selected an "over- 2-years" crop though your simulating period is just over 1 year,&
                      & correct the USM and run again'

! lectech     le 8/1/02
! -------
    messages (140) = 'name of cutting stage not recorded'
    messages (144) = 'Bare soil therefore parameter ressuite =0'
    messages (146) =  'jultrav before the start of simulation; set it to 0'
    messages (147) = '(no residues) or after the start of simulation'
    messages (149) = 'jultrav=0 thus qres set to 0'
    messages (150) = 'your coderes is strange !'
    messages (151) = 'irrigation depth zero ! change locirrig'
    messages (152) = 'TECHNICAL PARAMETERS NEEDED FOR THE SIMULATION, file: '
    messages (380) = 'thinning from below '
    messages (381) = 'thinning from above'
    messages (382) = 'harvest = all plant harvest'
    messages (383) = 'harvest = fruit picking'
    messages (384) = '1 at end of cycle'
    messages (385) =  '1 every '
    messages (386) = 'days'
    messages (387) = '    harvest at physiological maturity'
    messages (388) = 'harvest when water content (fruit) >'
    messages (389) = 'harvest when water content (fruit) <'
    messages (390) = 'harvest if sugar content >'
    messages (391) = 'harvest if sugar content>'
    messages (392) = 'harvest if oil content >'
    messages (393) = 'locirrig cannot exceed 50 -> set to 50'
    messages (394) = 'the value of ratiol must be between 0 and 1.0 it will be forced to 1.0'
    messages (395) = 'the value of the ratiolN cannot be negative; it will be forced to 1.0'
    messages (396) = 'then profsem value cannot be 0 it was given the value 1'
    messages (511) = 'No protected crops in mixed cropping'
    messages (397) = 'you cannot use a reproductive stage for the decision of forage cut'
    messages (398) = 'Attention !!  flowering imposing is useless because reproductive stages are calculated&
                        & from the drp stage.In order to impose reproductive phenology you need to impose drp'
    messages (399) = 'the option devoted to the keeping of all the plant variables after harvest (codemsfinal)&
                        & is not compatible with the picking method of harvest (codcueille)'
    messages (5600) = 'You can combine plastic mulching and addition of organic residues (P_nbjres)'
    messages (601) = 'You can combine plastic mulching and addition of previous crop residues (P_ressuite)'
! USM file
    messages(6011) = 'error in reading the file new_travail.usm: usm does not exist!'


! DR 13/08/09
! ******************************************************************
! Introduction des modifs 6.4 à garder pour la version 7.1 Javastics
! DR et ML et BM 23/06/09
    messages (451) = 'You have added mulch residue type that will be broken down and modify the physical properties&
                    & of the soil surface'
    messages (452) = 'the simulation period is too short given the parameter periode_adapt_CC'

! lixiv
! -----
    messages (153) = 'inconsistency between the values of da and hcc'
    messages (154) = 'leading to negative macroporosity :'
!dr 02/07/2013 correction des messages macroporosité mauvais
    messages (155) = 'codefente=2 :  macropor=((1-da/2.66)-(hcc/100*da)) * 10 * P_epc'
    messages (256) = 'macropor must be > 2. mm'
    messages (255) = 'codefente=1 :  macropor = 0.5 * (hcc - hmin) / 100. * da * 10. * P_epc'

! princip
! -------
    messages (156) = 'Error reading file of USM !'

! lectures
! --------
! domi 12/08/05
    messages (433) = 'for mixed crops -->Shuttleworth and Wallace is obligatory'
    messages (434) = 'check that you have the code  sw in your weather files'

! shutwall
! --------
    messages (159) = 'radiation nil on day'
    messages (160) = 'it is impossible to simulate mixed crops and a mulch'
    messages (161) = 'crop height A > crop height P'
    messages (162) = 'give a reference height weather measurements (P_zr parameter in param_gen.xml) below&
    & the maximum height of the crop (P_hautmax in plant file)'

! solnu
! -----
    messages (163) = 'soil evaporation stopped because of the '
    messages (164) = 'tillers population died day :'
    messages (169) = 'non existing harvest method: change codcueille value (in technical file)'
    messages (170) = 'proftrav (ITK) > profhum (soil), change one of the 2 values before run again'


! lecprofi
! --------
    messages (165) = 'problem with your profile output dates --> they have been recalculated'
    messages (1650) = 'A date for the profile output is bad and does not correspond to the simulation year, the file &
                       &mod_profil.sti will not be complete, correct the date :'

! eauplant
! --------
    messages (200) = 'problem calculating h2orec magrain and h2orec =0'

! iniclim
! -------
    messages (201) = 'your cropping season is more than 1 year the sequence is impossible; correct the date for the end  &
                        & of simulation and start again'
    messages (202) = 'PET < 0 thresholded at 0 , date '
!    messages (203) = 'the date for the end of simulation and start again'
    messages (204) = 'without wind and humidity: impossible to calculate the weather under shelter'
    messages (205) = 'without wind and humidity : impossible to calculate Penman PET; change the option to calculate PET'
    messages (206) = 'Priestley-Taylor has been calculated instead'
    messages (207) = 'you want to read PET Penman but it is missing'
    messages (208) = 'Penman has been calculated instead'
    messages (209) = 'more than 30 PET calculated differently as your choice, change the codetp parameter in general file'
    messages (218) = 'the vapor pressure is missing and has been calculated with tmin and the parameter P_corecTrosee'
! test sur les coherence de calcul des evapotranspiration
    messages (203) = 'In the station file you activated the Shuttleworth and Wallace calculation option of the Potential &
                       & EvapoTranspiration.'
    messages (62)  = 'Thus, you have to activate the resistive approach for water requirements calculation in your plant file '

    messages (1203) = 'In the station file you have not activated the Shuttleworth and Wallace calculation option of the Potential &
                       & EvapoTranspiration .'
    messages (1062)  = 'Thus, you have to choice the crop coefficient approach for water requirements calculation in your plant &
                       & file '



! eauqual
!--------
    messages (311) = 'too much fruit dehydration'
    messages (312) = 'Attention 100% of water in fruit: wrong parameterisation of h2ofrvert or deshydbase'
    messages (313) = 'Fruit biochemistry badly parameterised'
! *- marie 23/03/04
    messages (314) = 'Attention the dehydration of fruit cannot be calculated because stdrpdes has too low&
                      & a value; if you are not interested in dehydration of fruit, give to stdrpdes the same value as stdrpmat'

! P_effeuil
!--------
    messages (320) = 'trimming too early : review julrogne, hautrogne or biorognem'
    messages (321) = 'thinning rather drastic : there are no leaves left !'

! fruits
! ------
    messages (350) = 'Your development course does not make sense and leads to simulating fruit filling &
                       &before the start of vegetative growth'
    messages (351) = ' number of days delay for effective drp ) = '

! levee
!------
    messages (400) = 'deficiencies at emergence > density=0'

! recolte
!--------
    messages (401) = 'you need deshydbase < 0'
    messages (402) = 'you need deshydbase > 0'

! repartir
!---------
    messages (403) = 'high senescence : no more green leaves at LAX'
    messages (404) = 'attention : fruit skin (enfruit) or ratio tigefeuil too high'


! testtransrad
!-------------
    messages (430) = 'Attention : the dominant plant must use the radiation transfer. But codetradtec = 2, meaning&
                 & that the parameters interrang and orientrang have not been given values. To run the simulation, you must set&
                 & codetradtec to 1 and give at least one value to interrang > 0.'

! domi 12/08/05
    messages (432) = ' no parameters given for radiation transfer'

! bfroid
!-------
   messages (431) = 'Date of end of simulation deferred. Vernalisation taking place.'
    ! domi 29/09/05 dans lecplant
   messages (439) = 'attention : your variety number is more than the number of varieties'
   messages (440) = 'change the variety number and run again'
   messages (448) = 'for an annual plant P_julvernal can not be earlier than the sowing we put it equal to sowing'

! dominver
! --------
   messages (441) = 'Attention, change in dominancy, test on the radiative transfer parameters for change in dominancy'

! initnonsol - 16/02/2006
! -----------------------
    messages (443) = 'attention, when chaining the perenial crops (codeinitprec=2 and codeperenne=2) it is&
	                 & not possible to impose stages'
    messages (444) = 'it is not possible to delay the sowing day further on '
    messages (445) = 'sowing day was delayed of this number of days : '
    messages (446) = 'P_tcxstop must be > P_tdmax (if active, if not= 100)'
!    messages (449) = 'P_stdrpmat to P_stdrpdes (codeforcestdrpdes in the paramv6_gen.xml file)'
    messages (450) = 'You are in a sequence of linked simulations for intercrop (CAS) and the dominant crop&
                     & in the prior usm is not the same as in this usm : you have to change the dominance of crops in the &
                     &current usm'
! lecture données cycle précédent
! DR 14/06/2011 modification mise en conformité param.sol
    messages(501) = 'in the case of a sequence is recovered soil'
    messages(502) = 'P_typsol P_argi P_Norg P_profhum P_calc P_pH P_concseuil P_albedo P_q0 P_ruisolnu P_obstarac &
                     & P_pluiebat P_mulchbat P_zesx P_cfes P_z0solnu'
    messages(503) = '  P_codecailloux P_codemacropor P_codefente P_codrainage P_coderemontcap P_codenitrif P_codedenit'
    messages(504) = '  P_profimper P_ecartdrain P_ksol P_profdrain P_capiljour P_humcapil P_profdenit P_vpotdenit'
    messages(505) = '  P_epc   P_hccf   P_hminf  da  P_cailloux P_infil P_epd  P_typecailloux'
! message mythique a garder     messages(570) = 'the upper level of the water table reached the soil surface : Noe mobile : +33(0)6154365998'
    messages(570) = 'the upper level of the water table reached the soil surface '
! DR 29/08/2012 j'ajoute ce message d'erreur venant de sommecouche...
    messages(580) = 'The sum of the number of layers per cell is not consistent with the number of layers&
    & reported in Table summing'
    messages(600) = 'be sure to have parametrized the parameters for this residue ressuite = '
    messages(5000) = 'in case of associated crops , the code P_codelaitr in the file plant must be equal to 1="lai option" &
      & ,Please change your code and run again. '
    messages(5001) = 'nb_plantes in new_travail.usm is greater than the plant max number : 2 --> nbplant in usm :'
    messages(5162) = 'nb_plantes in ficini  /=  nb_plantes in usm : '
    messages (5110) = 'impossible to have 2 consecutive stages simultaneously, amf and lax'
    messages (5111) = 'impossible to have 2 consecutive stages simultaneously, lax and sen'
    messages (5112) = 'impossible to have 2 consecutive stages simultaneously, sen and lan'
    messages (5113) = 'the parameter sensrsec has been limited to 1.0'
    messages (5005) = '*********************************************************'
    messages (5120) = 'no bud break if no dormancy'
    messages (5121) = 'radiation interception = Beers law'
    messages (5122) = 'leaf dynamics = ground cover '
    messages (5123) = 'cold requirements = vernalisation (herbaceous)'
    messages (5124) = 'cold requirements = dormancy (woody)'
    messages (5125) = 'P_codedormance = forcing'
    messages (5126) = 'P_codedormance = Richardson'
    messages (5127) = 'P_codedormance = Bidabe'
    messages (5128) = 'annual'
    messages (5129) = 'germination or latency = yes '
    messages (5130) = 'plantlet growth = hypocotyle growth '
    messages (5131) = 'plantlet growth = planting '
    messages (5132) = 'perennial'
    messages (5133) = 'leaf dynamics = LAI '
    messages (5134) = 'LAI calculation option,  direct LAInet '
    messages (5135) = 'LAI calculation option, LAInet=LAIbrut-senes '
    messages (5136) = 'effect of photoperiod on senescence'
    messages (5137) = 'growing dynamics = determinate growing plant'
    messages (5138) = 'unit Harvest Index = days'
    messages (5139) = 'unit Harvest Index = degree days'
    messages (5140) = 'growing dynamics = indeterminate growing plant'
    messages (5141) = 'number of inflorescences =  prescribed'
    messages (5142) = 'number of inflorescences =  trophic status function'
    messages (5143) = 'thermal stress on filling'
    messages (5144) = 'root density = standard profile'
    messages (5145) = 'root density = true density'
    messages (5146) = 'N effect on root distribution'
    messages (5147) = 'trophic-linked production = continuous link'
    messages (5148) = 'trophic-linked production = threshold'
    messages (5149) = 'plantlet or emergence frost'
    messages (5150) = 'leaf frost at juvenile phase (till AMF)'
    messages (5151) = 'leaf frost at adult phase'
    messages (5152) = 'flower/fruit frost (from FLO)'
    messages (5153) = 'water requirements = crop coefficient'
    messages (5154) = 'water requirements = resistance approach'
    messages (5155) = 'interception of water by foliage'
    messages (5156) = 'calculation nitrogen requirements = dense canopies (initial)'
    messages (5157) = 'calculation nitrogen requirements = isolated plants (new calculation)'
    messages (5158) = 'maximal fixation capacit = constant'
    messages (5159) = 'maximal fixation capacit = growth fonction'
    messages (5160) = 'if artificial drainage activated (macroporosité must be desactivated and =infil not equal  0)'
    messages (3045) = ' Successive simulations impossible: wrong dates for start and finish'
    messages (3046) = ' Attention; if your previous weather file was not complete, &
                     &that may determine the end of the previous simulation !'
    messages (3274) = 'I cannot find the technique file '
    messages (4000) = 'I cannot find the file lai : '
    messages (220) = 'be carrefull ! the model run but one of the climate value of the day is missing : '
    messages (219) = 'error during the station file reading'
    messages (261) = '******** file station ********'
    messages (510) = 'P_julres P_coderes P_qres  P_Crespc  P_CsurNres P_Nminres  P_eaures'
    messages (379) = 'if P_codefauche=1, then P_codcueille=2'
    messages (4001) = 'erreur dans la lecture du fichier initialisations'
    messages (262) = 'reading param.stiin case of parameter forcing '
    messages (263) = 'this parameter name is not correct : '
    messages (506) = 'P_typsol          P_argi      P_Norg   P_profhum      P_calc        P_pH  P_concseuil    P_albedo&
                     &        P_q0  P_ruisolnu  P_obstarac  P_pluiebat P_mulchbat      P_zesx      P_cfes   P_z0solnu&
                     &  P_CsurNsol  P_penterui'
    messages (507) = '         P_epc        P_hccf       P_hminf         P_daf      P_cailloux  P_typecailloux  &
                      &   P_infil         P_epd'
   messages (5170) = 'Nitrogen stress actived '
   messages (5171) = 'Water stress actived'
   messages (5172) = 'Optimum mineralisation in bare soil activated'
   messages (5173) = 'Smoothing of initial profiles activated'
   messages (5174) = 'Depth for mineral N and water stocks calculation = profmes'
   messages (5175) = 'Depth for mineral N and water stocks calculation = profsol'
   messages (5176) = 'Climatic series = reset of the initial conditions'
   messages (5177) = 'Climatic series = succession'
   messages (5178) = 'Biomass and yield conservation after harvest'
   messages (5179) = 'Take account of mulch effect (drying out of soil surface)'
   messages (5180) = 'Fruit load = all fruits (including ripe ones)'
   messages (5181) = 'Fruit load = growing fruits only'
   messages (5182) = 'Hourly microclimate'
   messages (5183) = 'scientific writing in st2 and report'
   messages (5184) = 'Separator spaces in report'
   messages (5185) = 'Separator in report = '
   messages (5186) = 'Activation of model sensitivity analysis'
   messages (5187) = 'this flag use to choose the output files is not ok, change it : P_flagEcriture'
   messages (5188) = 'the output file will be only the report file : P_flagEcriture = '
   messages (5189) = 'remark , you will not obtain the balance file : P_flagEcriture = '
   messages (5190) = 'Mineral nitrogen inhibition = no effect'
   messages (5191) = 'Mineral nitrogen inhibition = nitrogen amount'
   messages (5192) = 'Mineral nitrogen inhibition = nitrogen concentration'
   messages (5193) = 'calculation of groundwater if drainage = average'
   messages (5194) = 'calculation of groundwater if drainage = interdrain localisation'
   messages (5195) = 'monocotyledon '
   messages (5196) = 'dicotyledon '
   messages (5197) = 'post dormancy calculation = daily temperatures'
   messages (5198) = 'post dormancy calculation = hourly temperatures'
   messages (264)  = 'Parameters of param_newform'
   messages (25)   = 'main crop planting is after associated crop planting'
   messages (1167) = 'Plant frosted and no reserve'
   messages (2099) = '===== Death of the plant any nitrogen ==='


return
end subroutine remplirMessages_ENG
 
 
