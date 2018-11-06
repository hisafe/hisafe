################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/utilities/soil/F_humirac.f90 \
../src/utilities/soil/ajupro.f90 \
../src/utilities/soil/macroporosite.f90 \
../src/utilities/soil/spline.f90 

OBJS += \
./src/utilities/soil/F_humirac.o \
./src/utilities/soil/ajupro.o \
./src/utilities/soil/macroporosite.o \
./src/utilities/soil/spline.o 


# Each subdirectory must supply rules for building sources it contributes
src/utilities/soil/%.o: ../src/utilities/soil/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/utilities/soil/F_humirac.o: ../src/utilities/soil/F_humirac.f90

src/utilities/soil/ajupro.o: ../src/utilities/soil/ajupro.f90

src/utilities/soil/macroporosite.o: ../src/utilities/soil/macroporosite.f90

src/utilities/soil/spline.o: ../src/utilities/soil/spline.f90


