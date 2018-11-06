################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/stics_day/Lixivation/Lixivation.f90 \
../src/stics_day/Lixivation/calculRU.f90 \
../src/stics_day/Lixivation/calcurRsurRU.f90 \
../src/stics_day/Lixivation/drainageAgricole.f90 \
../src/stics_day/Lixivation/excesEau.f90 \
../src/stics_day/Lixivation/hauteurNappe.f90 \
../src/stics_day/Lixivation/lixiv.f90 \
../src/stics_day/Lixivation/sommeCouchesParCellule.f90 \
../src/stics_day/Lixivation/transf.f90 \
../src/stics_day/Lixivation/transfertsDescendants.f90 

OBJS += \
./src/stics_day/Lixivation/Lixivation.o \
./src/stics_day/Lixivation/calculRU.o \
./src/stics_day/Lixivation/calcurRsurRU.o \
./src/stics_day/Lixivation/drainageAgricole.o \
./src/stics_day/Lixivation/excesEau.o \
./src/stics_day/Lixivation/hauteurNappe.o \
./src/stics_day/Lixivation/lixiv.o \
./src/stics_day/Lixivation/sommeCouchesParCellule.o \
./src/stics_day/Lixivation/transf.o \
./src/stics_day/Lixivation/transfertsDescendants.o 


# Each subdirectory must supply rules for building sources it contributes
src/stics_day/Lixivation/%.o: ../src/stics_day/Lixivation/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/stics_day/Lixivation/Lixivation.o: ../src/stics_day/Lixivation/Lixivation.f90

src/stics_day/Lixivation/calculRU.o: ../src/stics_day/Lixivation/calculRU.f90

src/stics_day/Lixivation/calcurRsurRU.o: ../src/stics_day/Lixivation/calcurRsurRU.f90

src/stics_day/Lixivation/drainageAgricole.o: ../src/stics_day/Lixivation/drainageAgricole.f90 src/outputs/history/Messages.o

src/stics_day/Lixivation/excesEau.o: ../src/stics_day/Lixivation/excesEau.f90 src/outputs/history/Messages.o

src/stics_day/Lixivation/hauteurNappe.o: ../src/stics_day/Lixivation/hauteurNappe.f90

src/stics_day/Lixivation/lixiv.o: ../src/stics_day/Lixivation/lixiv.f90 src/stics_day/Lixivation/Lixivation.o src/utilities/Divers.o

src/stics_day/Lixivation/sommeCouchesParCellule.o: ../src/stics_day/Lixivation/sommeCouchesParCellule.f90

src/stics_day/Lixivation/transf.o: ../src/stics_day/Lixivation/transf.f90

src/stics_day/Lixivation/transfertsDescendants.o: ../src/stics_day/Lixivation/transfertsDescendants.f90 src/stics_day/Lixivation/Lixivation.o


