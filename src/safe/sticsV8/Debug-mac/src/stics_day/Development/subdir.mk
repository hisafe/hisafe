################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/stics_day/Development/Ajust_Lai_Obs.f90 \
../src/stics_day/Development/Besoins_en_froid.f90 \
../src/stics_day/Development/CalculNombreDeFeuilles.f90 \
../src/stics_day/Development/Developpement.f90 \
../src/stics_day/Development/Effeuillage.f90 \
../src/stics_day/Development/F_densite_equiv.f90 \
../src/stics_day/Development/PhotoPeriode.f90 \
../src/stics_day/Development/Rognage.f90 \
../src/stics_day/Development/Stics_Battance.f90 \
../src/stics_day/Development/Stics_Calcul_LAI.f90 \
../src/stics_day/Development/Stics_Debour.f90 \
../src/stics_day/Development/Stics_Develop.f90 \
../src/stics_day/Development/Stics_Lai_Developpement.f90 \
../src/stics_day/Development/Stics_Levee.f90 \
../src/stics_day/Development/Stics_Recolte.f90 \
../src/stics_day/Development/TauxRecouvrement.f90 

OBJS += \
./src/stics_day/Development/Ajust_Lai_Obs.o \
./src/stics_day/Development/Besoins_en_froid.o \
./src/stics_day/Development/CalculNombreDeFeuilles.o \
./src/stics_day/Development/Developpement.o \
./src/stics_day/Development/Effeuillage.o \
./src/stics_day/Development/F_densite_equiv.o \
./src/stics_day/Development/PhotoPeriode.o \
./src/stics_day/Development/Rognage.o \
./src/stics_day/Development/Stics_Battance.o \
./src/stics_day/Development/Stics_Calcul_LAI.o \
./src/stics_day/Development/Stics_Debour.o \
./src/stics_day/Development/Stics_Develop.o \
./src/stics_day/Development/Stics_Lai_Developpement.o \
./src/stics_day/Development/Stics_Levee.o \
./src/stics_day/Development/Stics_Recolte.o \
./src/stics_day/Development/TauxRecouvrement.o 


# Each subdirectory must supply rules for building sources it contributes
src/stics_day/Development/%.o: ../src/stics_day/Development/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/stics_day/Development/Ajust_Lai_Obs.o: ../src/stics_day/Development/Ajust_Lai_Obs.f90

src/stics_day/Development/Besoins_en_froid.o: ../src/stics_day/Development/Besoins_en_froid.f90 src/outputs/history/Messages.o

src/stics_day/Development/CalculNombreDeFeuilles.o: ../src/stics_day/Development/CalculNombreDeFeuilles.f90

src/stics_day/Development/Developpement.o: ../src/stics_day/Development/Developpement.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/history/Messages.o src/stics_day/Development/Besoins_en_froid.o src/utilities/Divers.o

src/stics_day/Development/Effeuillage.o: ../src/stics_day/Development/Effeuillage.f90 src/outputs/history/Messages.o

src/stics_day/Development/F_densite_equiv.o: ../src/stics_day/Development/F_densite_equiv.f90

src/stics_day/Development/PhotoPeriode.o: ../src/stics_day/Development/PhotoPeriode.f90

src/stics_day/Development/Rognage.o: ../src/stics_day/Development/Rognage.f90 src/outputs/history/Messages.o

src/stics_day/Development/Stics_Battance.o: ../src/stics_day/Development/Stics_Battance.f90

src/stics_day/Development/Stics_Calcul_LAI.o: ../src/stics_day/Development/Stics_Calcul_LAI.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o

src/stics_day/Development/Stics_Debour.o: ../src/stics_day/Development/Stics_Debour.f90 src/outputs/history/Messages.o src/utilities/Divers.o

src/stics_day/Development/Stics_Develop.o: ../src/stics_day/Development/Stics_Develop.f90 src/outputs/history/Messages.o src/stics_day/Development/Besoins_en_froid.o src/utilities/Divers.o

src/stics_day/Development/Stics_Lai_Developpement.o: ../src/stics_day/Development/Stics_Lai_Developpement.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/stics_day/Development/Developpement.o

src/stics_day/Development/Stics_Levee.o: ../src/stics_day/Development/Stics_Levee.f90 src/outputs/history/Messages.o src/utilities/Divers.o

src/stics_day/Development/Stics_Recolte.o: ../src/stics_day/Development/Stics_Recolte.f90 src/outputs/history/Messages.o

src/stics_day/Development/TauxRecouvrement.o: ../src/stics_day/Development/TauxRecouvrement.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o


