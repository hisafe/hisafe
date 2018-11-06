################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/outputs/history/Ecriture_Parametres_Generaux.f90 \
../src/outputs/history/Ecriture_Plante.f90 \
../src/outputs/history/Ecriture_Transit.f90 \
../src/outputs/history/Messages.f90 \
../src/outputs/history/Messages_ENG.f90 

OBJS += \
./src/outputs/history/Ecriture_Parametres_Generaux.o \
./src/outputs/history/Ecriture_Plante.o \
./src/outputs/history/Ecriture_Transit.o \
./src/outputs/history/Messages.o \
./src/outputs/history/Messages_ENG.o 


# Each subdirectory must supply rules for building sources it contributes
src/outputs/history/%.o: ../src/outputs/history/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/outputs/history/Ecriture_Parametres_Generaux.o: ../src/outputs/history/Ecriture_Parametres_Generaux.f90 src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/history/Messages.o

src/outputs/history/Ecriture_Plante.o: ../src/outputs/history/Ecriture_Plante.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/history/Messages.o

src/outputs/history/Ecriture_Transit.o: ../src/outputs/history/Ecriture_Transit.f90 src/Stics.o src/outputs/history/Messages.o

src/outputs/history/Messages.o: ../src/outputs/history/Messages.f90 src/utilities/iso_varying_string.o

src/outputs/history/Messages_ENG.o: ../src/outputs/history/Messages_ENG.f90 src/utilities/iso_varying_string.o


