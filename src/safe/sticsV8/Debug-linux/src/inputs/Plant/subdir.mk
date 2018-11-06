################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/inputs/Plant/Plante.f90 

OBJS += \
./src/inputs/Plant/Plante.o 


# Each subdirectory must supply rules for building sources it contributes
src/inputs/Plant/%.o: ../src/inputs/Plant/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/inputs/Plant/Plante.o: ../src/inputs/Plant/Plante.f90 src/outputs/history/Messages.o


