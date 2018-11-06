################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/Stics.f90 \
../src/Stics_Boucle_Annees.f90 \
../src/main.f90 

OBJS += \
./src/Stics.o \
./src/Stics_Boucle_Annees.o \
./src/main.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/Stics.o: ../src/Stics.f90 src/outputs/history/Messages.o src/utilities/iso_varying_string.o

src/Stics_Boucle_Annees.o: ../src/Stics_Boucle_Annees.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/Balance/Bilans.o src/utilities/Divers.o

src/main.o: ../src/main.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/Balance/Bilans.o src/outputs/history/Messages.o


