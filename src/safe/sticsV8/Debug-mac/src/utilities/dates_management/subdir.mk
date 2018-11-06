################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/utilities/dates_management/dates.f90 \
../src/utilities/dates_management/isBissextile.f90 \
../src/utilities/dates_management/julien.f90 \
../src/utilities/dates_management/nbjParAnnee.f90 \
../src/utilities/dates_management/nbjan.f90 \
../src/utilities/dates_management/ndate.f90 \
../src/utilities/dates_management/tCal1JAbs.f90 \
../src/utilities/dates_management/tCal1JRel.f90 

OBJS += \
./src/utilities/dates_management/dates.o \
./src/utilities/dates_management/isBissextile.o \
./src/utilities/dates_management/julien.o \
./src/utilities/dates_management/nbjParAnnee.o \
./src/utilities/dates_management/nbjan.o \
./src/utilities/dates_management/ndate.o \
./src/utilities/dates_management/tCal1JAbs.o \
./src/utilities/dates_management/tCal1JRel.o 


# Each subdirectory must supply rules for building sources it contributes
src/utilities/dates_management/%.o: ../src/utilities/dates_management/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/utilities/dates_management/dates.o: ../src/utilities/dates_management/dates.f90 src/utilities/Divers.o

src/utilities/dates_management/isBissextile.o: ../src/utilities/dates_management/isBissextile.f90

src/utilities/dates_management/julien.o: ../src/utilities/dates_management/julien.f90

src/utilities/dates_management/nbjParAnnee.o: ../src/utilities/dates_management/nbjParAnnee.f90 src/utilities/Divers.o

src/utilities/dates_management/nbjan.o: ../src/utilities/dates_management/nbjan.f90

src/utilities/dates_management/ndate.o: ../src/utilities/dates_management/ndate.f90

src/utilities/dates_management/tCal1JAbs.o: ../src/utilities/dates_management/tCal1JAbs.f90

src/utilities/dates_management/tCal1JRel.o: ../src/utilities/dates_management/tCal1JRel.f90 src/utilities/Divers.o


