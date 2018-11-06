################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/outputs/daily_outputs/CorrespondanceVariablesDeSortie.f90 \
../src/outputs/daily_outputs/Ecriture_VariablesSortiesJournalieres.f90 \
../src/outputs/daily_outputs/Ecriture_VariablesSortiesJournalieres_agmip.f90 \
../src/outputs/daily_outputs/cumAOetAS.f90 \
../src/outputs/daily_outputs/sortie.f90 

OBJS += \
./src/outputs/daily_outputs/CorrespondanceVariablesDeSortie.o \
./src/outputs/daily_outputs/Ecriture_VariablesSortiesJournalieres.o \
./src/outputs/daily_outputs/Ecriture_VariablesSortiesJournalieres_agmip.o \
./src/outputs/daily_outputs/cumAOetAS.o \
./src/outputs/daily_outputs/sortie.o 


# Each subdirectory must supply rules for building sources it contributes
src/outputs/daily_outputs/%.o: ../src/outputs/daily_outputs/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/outputs/daily_outputs/CorrespondanceVariablesDeSortie.o: ../src/outputs/daily_outputs/CorrespondanceVariablesDeSortie.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o

src/outputs/daily_outputs/Ecriture_VariablesSortiesJournalieres.o: ../src/outputs/daily_outputs/Ecriture_VariablesSortiesJournalieres.f90

src/outputs/daily_outputs/Ecriture_VariablesSortiesJournalieres_agmip.o: ../src/outputs/daily_outputs/Ecriture_VariablesSortiesJournalieres_agmip.f90

src/outputs/daily_outputs/cumAOetAS.o: ../src/outputs/daily_outputs/cumAOetAS.f90 src/inputs/Plant/Plante.o src/inputs/crop_management_parameters/Itineraire_technique.o

src/outputs/daily_outputs/sortie.o: ../src/outputs/daily_outputs/sortie.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/utilities/Divers.o


