################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/inputs/LAI/Lecture_Lai.f90 

OBJS += \
./src/inputs/LAI/Lecture_Lai.o 


# Each subdirectory must supply rules for building sources it contributes
src/inputs/LAI/%.o: ../src/inputs/LAI/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/inputs/LAI/Lecture_Lai.o: ../src/inputs/LAI/Lecture_Lai.f90 src/Stics.o src/inputs/Plant/Plante.o src/outputs/history/Messages.o


