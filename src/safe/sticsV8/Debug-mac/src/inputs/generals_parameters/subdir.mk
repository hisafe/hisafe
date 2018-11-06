################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/inputs/generals_parameters/Parametres_Generaux.f90 \
../src/inputs/generals_parameters/Stics_Lecture_Transit.f90 

OBJS += \
./src/inputs/generals_parameters/Parametres_Generaux.o \
./src/inputs/generals_parameters/Stics_Lecture_Transit.o 


# Each subdirectory must supply rules for building sources it contributes
src/inputs/generals_parameters/%.o: ../src/inputs/generals_parameters/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/inputs/generals_parameters/Parametres_Generaux.o: ../src/inputs/generals_parameters/Parametres_Generaux.f90 src/Stics.o src/outputs/history/Messages.o

src/inputs/generals_parameters/Stics_Lecture_Transit.o: ../src/inputs/generals_parameters/Stics_Lecture_Transit.f90 src/Stics.o src/outputs/history/Messages.o


