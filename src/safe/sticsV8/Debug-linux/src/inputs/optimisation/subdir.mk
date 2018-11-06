################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/inputs/optimisation/CorrespondanceParametresEntree.f90 \
../src/inputs/optimisation/Lecture_Optimisation.f90 

OBJS += \
./src/inputs/optimisation/CorrespondanceParametresEntree.o \
./src/inputs/optimisation/Lecture_Optimisation.o 


# Each subdirectory must supply rules for building sources it contributes
src/inputs/optimisation/%.o: ../src/inputs/optimisation/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/inputs/optimisation/CorrespondanceParametresEntree.o: ../src/inputs/optimisation/CorrespondanceParametresEntree.f90 src/Stics.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/history/Messages.o src/utilities/Divers.o src/utilities/iso_varying_string.o

src/inputs/optimisation/Lecture_Optimisation.o: ../src/inputs/optimisation/Lecture_Optimisation.f90


