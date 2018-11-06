################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/stics_day/stress/Ngrain.f90 \
../src/stics_day/stress/Stress_PhotoPeriodique.f90 \
../src/stics_day/stress/excesdeau.f90 \
../src/stics_day/stress/gel.f90 \
../src/stics_day/stress/stressEau.f90 \
../src/stics_day/stress/stressN.f90 

OBJS += \
./src/stics_day/stress/Ngrain.o \
./src/stics_day/stress/Stress_PhotoPeriodique.o \
./src/stics_day/stress/excesdeau.o \
./src/stics_day/stress/gel.o \
./src/stics_day/stress/stressEau.o \
./src/stics_day/stress/stressN.o 


# Each subdirectory must supply rules for building sources it contributes
src/stics_day/stress/%.o: ../src/stics_day/stress/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/stics_day/stress/Ngrain.o: ../src/stics_day/stress/Ngrain.f90

src/stics_day/stress/Stress_PhotoPeriodique.o: ../src/stics_day/stress/Stress_PhotoPeriodique.f90

src/stics_day/stress/excesdeau.o: ../src/stics_day/stress/excesdeau.f90

src/stics_day/stress/gel.o: ../src/stics_day/stress/gel.f90

src/stics_day/stress/stressEau.o: ../src/stics_day/stress/stressEau.f90

src/stics_day/stress/stressN.o: ../src/stics_day/stress/stressN.f90


