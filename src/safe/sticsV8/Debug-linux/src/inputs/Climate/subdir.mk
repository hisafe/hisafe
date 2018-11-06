################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/inputs/Climate/Climat.f90 \
../src/inputs/Climate/Fonctions_Climat.f90 \
../src/inputs/Climate/Station.f90 

OBJS += \
./src/inputs/Climate/Climat.o \
./src/inputs/Climate/Fonctions_Climat.o \
./src/inputs/Climate/Station.o 


# Each subdirectory must supply rules for building sources it contributes
src/inputs/Climate/%.o: ../src/inputs/Climate/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/inputs/Climate/Climat.o: ../src/inputs/Climate/Climat.f90 src/outputs/history/Messages.o src/utilities/Divers.o

src/inputs/Climate/Fonctions_Climat.o: ../src/inputs/Climate/Fonctions_Climat.f90

src/inputs/Climate/Station.o: ../src/inputs/Climate/Station.f90 src/outputs/history/Messages.o


