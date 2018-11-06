################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/outputs/profile/Ecriture_Profil.f90 

OBJS += \
./src/outputs/profile/Ecriture_Profil.o 


# Each subdirectory must supply rules for building sources it contributes
src/outputs/profile/%.o: ../src/outputs/profile/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/outputs/profile/Ecriture_Profil.o: ../src/outputs/profile/Ecriture_Profil.f90 src/Stics.o


