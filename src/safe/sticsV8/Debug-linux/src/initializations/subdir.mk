################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/initializations/Initialisation_PrairiePerenne.f90 \
../src/initializations/Stics_Initialisation_Boucle_Annees.f90 \
../src/initializations/Stics_Zero.f90 \
../src/initializations/calcDeltaTCC.f90 \
../src/initializations/calc_tmoy_annuel.f90 \
../src/initializations/determinate_sowing_Agmip.f90 \
../src/initializations/effetadaptMOauCC.f90 \
../src/initializations/iniclim.f90 \
../src/initializations/init_prairie.f90 \
../src/initializations/init_prairie_avec_enchainementdescoupes.f90 \
../src/initializations/init_sol.f90 \
../src/initializations/initcycle.f90 \
../src/initializations/initialisations.f90 \
../src/initializations/initnonsol.f90 \
../src/initializations/initsimul.f90 

OBJS += \
./src/initializations/Initialisation_PrairiePerenne.o \
./src/initializations/Stics_Initialisation_Boucle_Annees.o \
./src/initializations/Stics_Zero.o \
./src/initializations/calcDeltaTCC.o \
./src/initializations/calc_tmoy_annuel.o \
./src/initializations/determinate_sowing_Agmip.o \
./src/initializations/effetadaptMOauCC.o \
./src/initializations/iniclim.o \
./src/initializations/init_prairie.o \
./src/initializations/init_prairie_avec_enchainementdescoupes.o \
./src/initializations/init_sol.o \
./src/initializations/initcycle.o \
./src/initializations/initialisations.o \
./src/initializations/initnonsol.o \
./src/initializations/initsimul.o 


# Each subdirectory must supply rules for building sources it contributes
src/initializations/%.o: ../src/initializations/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/initializations/Initialisation_PrairiePerenne.o: ../src/initializations/Initialisation_PrairiePerenne.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/crop_management_parameters/Itineraire_technique.o

src/initializations/Stics_Initialisation_Boucle_Annees.o: ../src/initializations/Stics_Initialisation_Boucle_Annees.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/utilities/Divers.o

src/initializations/Stics_Zero.o: ../src/initializations/Stics_Zero.f90 src/Stics.o

src/initializations/calcDeltaTCC.o: ../src/initializations/calcDeltaTCC.f90 src/Stics.o

src/initializations/calc_tmoy_annuel.o: ../src/initializations/calc_tmoy_annuel.f90 src/outputs/history/Messages.o src/utilities/Divers.o

src/initializations/determinate_sowing_Agmip.o: ../src/initializations/determinate_sowing_Agmip.f90 src/inputs/Climate/Climat.o

src/initializations/effetadaptMOauCC.o: ../src/initializations/effetadaptMOauCC.f90 src/Stics.o src/inputs/Soil/Sol.o src/inputs/generals_parameters/Parametres_Generaux.o

src/initializations/iniclim.o: ../src/initializations/iniclim.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/history/Messages.o src/stics_day/Development/Besoins_en_froid.o src/utilities/Divers.o

src/initializations/init_prairie.o: ../src/initializations/init_prairie.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/history/Messages.o src/utilities/Divers.o

src/initializations/init_prairie_avec_enchainementdescoupes.o: ../src/initializations/init_prairie_avec_enchainementdescoupes.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/history/Messages.o src/utilities/Divers.o

src/initializations/init_sol.o: ../src/initializations/init_sol.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Soil/Sol.o src/inputs/generals_parameters/Parametres_Generaux.o

src/initializations/initcycle.o: ../src/initializations/initcycle.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o

src/initializations/initialisations.o: ../src/initializations/initialisations.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/utilities/Divers.o

src/initializations/initnonsol.o: ../src/initializations/initnonsol.f90 src/Stics.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/history/Messages.o src/utilities/Divers.o

src/initializations/initsimul.o: ../src/initializations/initsimul.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o


