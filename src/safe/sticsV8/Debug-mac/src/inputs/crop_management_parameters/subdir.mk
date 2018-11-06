################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/inputs/crop_management_parameters/Itineraire_technique.f90 \
../src/inputs/crop_management_parameters/testvartech.f90 

OBJS += \
./src/inputs/crop_management_parameters/Itineraire_technique.o \
./src/inputs/crop_management_parameters/testvartech.o 


# Each subdirectory must supply rules for building sources it contributes
src/inputs/crop_management_parameters/%.o: ../src/inputs/crop_management_parameters/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/inputs/crop_management_parameters/Itineraire_technique.o: ../src/inputs/crop_management_parameters/Itineraire_technique.f90 src/Stics.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/history/Messages.o

src/inputs/crop_management_parameters/testvartech.o: ../src/inputs/crop_management_parameters/testvartech.f90 src/inputs/crop_management_parameters/Itineraire_technique.o


