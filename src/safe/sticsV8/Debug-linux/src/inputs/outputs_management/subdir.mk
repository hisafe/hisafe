################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/inputs/outputs_management/Lecture_Profil.f90 \
../src/inputs/outputs_management/Lecture_VariablesRapport.f90 \
../src/inputs/outputs_management/Lecture_VariablesSortiesJournalieres.f90 

OBJS += \
./src/inputs/outputs_management/Lecture_Profil.o \
./src/inputs/outputs_management/Lecture_VariablesRapport.o \
./src/inputs/outputs_management/Lecture_VariablesSortiesJournalieres.o 


# Each subdirectory must supply rules for building sources it contributes
src/inputs/outputs_management/%.o: ../src/inputs/outputs_management/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/inputs/outputs_management/Lecture_Profil.o: ../src/inputs/outputs_management/Lecture_Profil.f90

src/inputs/outputs_management/Lecture_VariablesRapport.o: ../src/inputs/outputs_management/Lecture_VariablesRapport.f90 src/Stics.o

src/inputs/outputs_management/Lecture_VariablesSortiesJournalieres.o: ../src/inputs/outputs_management/Lecture_VariablesSortiesJournalieres.f90 src/Stics.o


