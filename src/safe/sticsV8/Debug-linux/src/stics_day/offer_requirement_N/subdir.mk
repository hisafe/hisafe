################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/stics_day/offer_requirement_N/absoN.f90 \
../src/stics_day/offer_requirement_N/bNpl.f90 \
../src/stics_day/offer_requirement_N/majNsol.f90 \
../src/stics_day/offer_requirement_N/offreN.f90 \
../src/stics_day/offer_requirement_N/offrnodu.f90 

OBJS += \
./src/stics_day/offer_requirement_N/absoN.o \
./src/stics_day/offer_requirement_N/bNpl.o \
./src/stics_day/offer_requirement_N/majNsol.o \
./src/stics_day/offer_requirement_N/offreN.o \
./src/stics_day/offer_requirement_N/offrnodu.o 


# Each subdirectory must supply rules for building sources it contributes
src/stics_day/offer_requirement_N/%.o: ../src/stics_day/offer_requirement_N/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/stics_day/offer_requirement_N/absoN.o: ../src/stics_day/offer_requirement_N/absoN.f90

src/stics_day/offer_requirement_N/bNpl.o: ../src/stics_day/offer_requirement_N/bNpl.f90

src/stics_day/offer_requirement_N/majNsol.o: ../src/stics_day/offer_requirement_N/majNsol.f90

src/stics_day/offer_requirement_N/offreN.o: ../src/stics_day/offer_requirement_N/offreN.f90

src/stics_day/offer_requirement_N/offrnodu.o: ../src/stics_day/offer_requirement_N/offrnodu.f90


