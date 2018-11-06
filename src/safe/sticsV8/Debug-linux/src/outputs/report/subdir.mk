################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/outputs/report/Ecriture_Rapport.f90 \
../src/outputs/report/Ecriture_Rapport_AgMIP.f90 \
../src/outputs/report/ecriture_start.f90 

OBJS += \
./src/outputs/report/Ecriture_Rapport.o \
./src/outputs/report/Ecriture_Rapport_AgMIP.o \
./src/outputs/report/ecriture_start.o 


# Each subdirectory must supply rules for building sources it contributes
src/outputs/report/%.o: ../src/outputs/report/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/outputs/report/Ecriture_Rapport.o: ../src/outputs/report/Ecriture_Rapport.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o

src/outputs/report/Ecriture_Rapport_AgMIP.o: ../src/outputs/report/Ecriture_Rapport_AgMIP.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o

src/outputs/report/ecriture_start.o: ../src/outputs/report/ecriture_start.f90 src/Stics.o src/inputs/Soil/Sol.o src/stics_day/Lixivation/Lixivation.o


