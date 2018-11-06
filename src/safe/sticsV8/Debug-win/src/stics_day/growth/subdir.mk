################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/stics_day/growth/Module_Croissance.f90 \
../src/stics_day/growth/beer.f90 \
../src/stics_day/growth/biomaer.f90 \
../src/stics_day/growth/calpsibase.f90 \
../src/stics_day/growth/croira.f90 \
../src/stics_day/growth/croissance.f90 \
../src/stics_day/growth/densirac.f90 \
../src/stics_day/growth/eauqual.f90 \
../src/stics_day/growth/epcouche.f90 \
../src/stics_day/growth/fruit.f90 \
../src/stics_day/growth/grain.f90 \
../src/stics_day/growth/raytrans.f90 \
../src/stics_day/growth/repartir.f90 \
../src/stics_day/growth/senescen.f90 \
../src/stics_day/growth/transrad.f90 

OBJS += \
./src/stics_day/growth/Module_Croissance.o \
./src/stics_day/growth/beer.o \
./src/stics_day/growth/biomaer.o \
./src/stics_day/growth/calpsibase.o \
./src/stics_day/growth/croira.o \
./src/stics_day/growth/croissance.o \
./src/stics_day/growth/densirac.o \
./src/stics_day/growth/eauqual.o \
./src/stics_day/growth/epcouche.o \
./src/stics_day/growth/fruit.o \
./src/stics_day/growth/grain.o \
./src/stics_day/growth/raytrans.o \
./src/stics_day/growth/repartir.o \
./src/stics_day/growth/senescen.o \
./src/stics_day/growth/transrad.o 


# Each subdirectory must supply rules for building sources it contributes
src/stics_day/growth/%.o: ../src/stics_day/growth/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/stics_day/growth/Module_Croissance.o: ../src/stics_day/growth/Module_Croissance.f90 src/outputs/history/Messages.o src/utilities/Divers.o

src/stics_day/growth/beer.o: ../src/stics_day/growth/beer.f90

src/stics_day/growth/biomaer.o: ../src/stics_day/growth/biomaer.f90

src/stics_day/growth/calpsibase.o: ../src/stics_day/growth/calpsibase.f90 src/utilities/Divers.o

src/stics_day/growth/croira.o: ../src/stics_day/growth/croira.f90 src/outputs/history/Messages.o src/utilities/Divers.o

src/stics_day/growth/croissance.o: ../src/stics_day/growth/croissance.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/stics_day/growth/Module_Croissance.o src/utilities/Divers.o

src/stics_day/growth/densirac.o: ../src/stics_day/growth/densirac.f90 src/outputs/history/Messages.o src/utilities/Divers.o

src/stics_day/growth/eauqual.o: ../src/stics_day/growth/eauqual.f90 src/outputs/history/Messages.o

src/stics_day/growth/epcouche.o: ../src/stics_day/growth/epcouche.f90

src/stics_day/growth/fruit.o: ../src/stics_day/growth/fruit.f90 src/outputs/history/Messages.o

src/stics_day/growth/grain.o: ../src/stics_day/growth/grain.f90 src/outputs/history/Messages.o

src/stics_day/growth/raytrans.o: ../src/stics_day/growth/raytrans.f90

src/stics_day/growth/repartir.o: ../src/stics_day/growth/repartir.f90 src/outputs/history/Messages.o

src/stics_day/growth/senescen.o: ../src/stics_day/growth/senescen.f90 src/outputs/history/Messages.o src/utilities/Divers.o

src/stics_day/growth/transrad.o: ../src/stics_day/growth/transrad.f90 src/utilities/Divers.o


