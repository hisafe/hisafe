################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/inputs/data_previousCycle/Lecture_DonneesCyclePrecedent.f90 

OBJS += \
./src/inputs/data_previousCycle/Lecture_DonneesCyclePrecedent.o 


# Each subdirectory must supply rules for building sources it contributes
src/inputs/data_previousCycle/%.o: ../src/inputs/data_previousCycle/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/inputs/data_previousCycle/Lecture_DonneesCyclePrecedent.o: ../src/inputs/data_previousCycle/Lecture_DonneesCyclePrecedent.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/utilities/Divers.o


