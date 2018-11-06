################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/stics_day/micro_climate/calrnet.f90 \
../src/stics_day/micro_climate/caltcult.f90 \
../src/stics_day/micro_climate/humcouv.f90 \
../src/stics_day/micro_climate/humheure.f90 \
../src/stics_day/micro_climate/ratmo.f90 \
../src/stics_day/micro_climate/surface_wetness_duration_relative_humidity.f90 \
../src/stics_day/micro_climate/tempsol.f90 

OBJS += \
./src/stics_day/micro_climate/calrnet.o \
./src/stics_day/micro_climate/caltcult.o \
./src/stics_day/micro_climate/humcouv.o \
./src/stics_day/micro_climate/humheure.o \
./src/stics_day/micro_climate/ratmo.o \
./src/stics_day/micro_climate/surface_wetness_duration_relative_humidity.o \
./src/stics_day/micro_climate/tempsol.o 


# Each subdirectory must supply rules for building sources it contributes
src/stics_day/micro_climate/%.o: ../src/stics_day/micro_climate/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/stics_day/micro_climate/calrnet.o: ../src/stics_day/micro_climate/calrnet.f90 src/utilities/Divers.o

src/stics_day/micro_climate/caltcult.o: ../src/stics_day/micro_climate/caltcult.f90

src/stics_day/micro_climate/humcouv.o: ../src/stics_day/micro_climate/humcouv.f90 src/utilities/Divers.o

src/stics_day/micro_climate/humheure.o: ../src/stics_day/micro_climate/humheure.f90 src/utilities/Divers.o

src/stics_day/micro_climate/ratmo.o: ../src/stics_day/micro_climate/ratmo.f90 src/utilities/Divers.o

src/stics_day/micro_climate/surface_wetness_duration_relative_humidity.o: ../src/stics_day/micro_climate/surface_wetness_duration_relative_humidity.f90 src/utilities/Divers.o

src/stics_day/micro_climate/tempsol.o: ../src/stics_day/micro_climate/tempsol.f90


