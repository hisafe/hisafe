################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/outputs/data_end_ofCycle/Ecriture_DonneesFinDeCycle.f90 

OBJS += \
./src/outputs/data_end_ofCycle/Ecriture_DonneesFinDeCycle.o 


# Each subdirectory must supply rules for building sources it contributes
src/outputs/data_end_ofCycle/%.o: ../src/outputs/data_end_ofCycle/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/outputs/data_end_ofCycle/Ecriture_DonneesFinDeCycle.o: ../src/outputs/data_end_ofCycle/Ecriture_DonneesFinDeCycle.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/utilities/Divers.o


