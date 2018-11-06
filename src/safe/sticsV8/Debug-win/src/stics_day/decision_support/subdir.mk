################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/stics_day/decision_support/F_effettasssurinfil.f90 \
../src/stics_day/decision_support/decisionrecolte.f90 \
../src/stics_day/decision_support/decisionsemis.f90 \
../src/stics_day/decision_support/detassement.f90 \
../src/stics_day/decision_support/modifWsol.f90 \
../src/stics_day/decision_support/tassesemisrecolte.f90 

OBJS += \
./src/stics_day/decision_support/F_effettasssurinfil.o \
./src/stics_day/decision_support/decisionrecolte.o \
./src/stics_day/decision_support/decisionsemis.o \
./src/stics_day/decision_support/detassement.o \
./src/stics_day/decision_support/modifWsol.o \
./src/stics_day/decision_support/tassesemisrecolte.o 


# Each subdirectory must supply rules for building sources it contributes
src/stics_day/decision_support/%.o: ../src/stics_day/decision_support/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/stics_day/decision_support/F_effettasssurinfil.o: ../src/stics_day/decision_support/F_effettasssurinfil.f90

src/stics_day/decision_support/decisionrecolte.o: ../src/stics_day/decision_support/decisionrecolte.f90

src/stics_day/decision_support/decisionsemis.o: ../src/stics_day/decision_support/decisionsemis.f90 src/outputs/history/Messages.o

src/stics_day/decision_support/detassement.o: ../src/stics_day/decision_support/detassement.f90 src/outputs/history/Messages.o src/utilities/Divers.o

src/stics_day/decision_support/modifWsol.o: ../src/stics_day/decision_support/modifWsol.f90

src/stics_day/decision_support/tassesemisrecolte.o: ../src/stics_day/decision_support/tassesemisrecolte.f90 src/outputs/history/Messages.o


