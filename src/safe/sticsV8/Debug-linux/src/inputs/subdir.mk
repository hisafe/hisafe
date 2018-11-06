################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/inputs/Stics_Lectures.f90 

OBJS += \
./src/inputs/Stics_Lectures.o 


# Each subdirectory must supply rules for building sources it contributes
src/inputs/%.o: ../src/inputs/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/inputs/Stics_Lectures.o: ../src/inputs/Stics_Lectures.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/utilities/Divers.o src/utilities/iso_varying_string.o


