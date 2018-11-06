################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/outputs/Balance/Bilans.f90 \
../src/outputs/Balance/bilans_messages_ENG.f90 \
../src/outputs/Balance/stadeversbbch.f90 

OBJS += \
./src/outputs/Balance/Bilans.o \
./src/outputs/Balance/bilans_messages_ENG.o \
./src/outputs/Balance/stadeversbbch.o 


# Each subdirectory must supply rules for building sources it contributes
src/outputs/Balance/%.o: ../src/outputs/Balance/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/outputs/Balance/Bilans.o: ../src/outputs/Balance/Bilans.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/history/Messages.o src/utilities/Divers.o

src/outputs/Balance/bilans_messages_ENG.o: ../src/outputs/Balance/bilans_messages_ENG.f90 src/outputs/Balance/Bilans.o

src/outputs/Balance/stadeversbbch.o: ../src/outputs/Balance/stadeversbbch.f90 src/inputs/Plant/Plante.o


