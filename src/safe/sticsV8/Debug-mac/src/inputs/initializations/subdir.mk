################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/inputs/initializations/Lecture_FichierInitialisations.f90 

OBJS += \
./src/inputs/initializations/Lecture_FichierInitialisations.o 


# Each subdirectory must supply rules for building sources it contributes
src/inputs/initializations/%.o: ../src/inputs/initializations/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/inputs/initializations/Lecture_FichierInitialisations.o: ../src/inputs/initializations/Lecture_FichierInitialisations.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/outputs/history/Messages.o


