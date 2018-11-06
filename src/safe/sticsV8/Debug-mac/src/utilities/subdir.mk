################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/utilities/Divers.f90 \
../src/utilities/Divers_develop.f90 \
../src/utilities/charversint.f90 \
../src/utilities/escalin.f90 \
../src/utilities/iso_varying_string.f90 

OBJS += \
./src/utilities/Divers.o \
./src/utilities/Divers_develop.o \
./src/utilities/charversint.o \
./src/utilities/escalin.o \
./src/utilities/iso_varying_string.o 


# Each subdirectory must supply rules for building sources it contributes
src/utilities/%.o: ../src/utilities/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/utilities/Divers.o: ../src/utilities/Divers.f90

src/utilities/Divers_develop.o: ../src/utilities/Divers_develop.f90

src/utilities/charversint.o: ../src/utilities/charversint.f90

src/utilities/escalin.o: ../src/utilities/escalin.f90

src/utilities/iso_varying_string.o: ../src/utilities/iso_varying_string.f90


