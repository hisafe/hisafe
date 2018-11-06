################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/utilities/climate/ClimatSousAbri.f90 \
../src/utilities/climate/F_temprosee.f90 \
../src/utilities/climate/calceeq.f90 \
../src/utilities/climate/calpenman.f90 \
../src/utilities/climate/decangle.f90 \
../src/utilities/climate/rgex.f90 \
../src/utilities/climate/tmoy_histo.f90 \
../src/utilities/climate/tvar.f90 

OBJS += \
./src/utilities/climate/ClimatSousAbri.o \
./src/utilities/climate/F_temprosee.o \
./src/utilities/climate/calceeq.o \
./src/utilities/climate/calpenman.o \
./src/utilities/climate/decangle.o \
./src/utilities/climate/rgex.o \
./src/utilities/climate/tmoy_histo.o \
./src/utilities/climate/tvar.o 


# Each subdirectory must supply rules for building sources it contributes
src/utilities/climate/%.o: ../src/utilities/climate/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/utilities/climate/ClimatSousAbri.o: ../src/utilities/climate/ClimatSousAbri.f90

src/utilities/climate/F_temprosee.o: ../src/utilities/climate/F_temprosee.f90

src/utilities/climate/calceeq.o: ../src/utilities/climate/calceeq.f90

src/utilities/climate/calpenman.o: ../src/utilities/climate/calpenman.f90

src/utilities/climate/decangle.o: ../src/utilities/climate/decangle.f90

src/utilities/climate/rgex.o: ../src/utilities/climate/rgex.f90 src/utilities/Divers.o

src/utilities/climate/tmoy_histo.o: ../src/utilities/climate/tmoy_histo.f90

src/utilities/climate/tvar.o: ../src/utilities/climate/tvar.f90


