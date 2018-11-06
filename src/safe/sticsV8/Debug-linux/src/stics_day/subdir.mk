################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/stics_day/Stics_Jour.f90 \
../src/stics_day/Stics_Jour_after.f90 

OBJS += \
./src/stics_day/Stics_Jour.o \
./src/stics_day/Stics_Jour_after.o 


# Each subdirectory must supply rules for building sources it contributes
src/stics_day/%.o: ../src/stics_day/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/stics_day/Stics_Jour.o: ../src/stics_day/Stics_Jour.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/utilities/Divers.o

src/stics_day/Stics_Jour_after.o: ../src/stics_day/Stics_Jour_after.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o


