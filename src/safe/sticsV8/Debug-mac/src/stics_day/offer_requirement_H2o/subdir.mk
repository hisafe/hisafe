################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/stics_day/offer_requirement_H2o/besoinsEnEauDeLaPlante.f90 \
../src/stics_day/offer_requirement_H2o/calraero.f90 \
../src/stics_day/offer_requirement_H2o/etatsurf.f90 \
../src/stics_day/offer_requirement_H2o/ketp.f90 \
../src/stics_day/offer_requirement_H2o/shutwall.f90 \
../src/stics_day/offer_requirement_H2o/solnu.f90 \
../src/stics_day/offer_requirement_H2o/transpi.f90 

OBJS += \
./src/stics_day/offer_requirement_H2o/besoinsEnEauDeLaPlante.o \
./src/stics_day/offer_requirement_H2o/calraero.o \
./src/stics_day/offer_requirement_H2o/etatsurf.o \
./src/stics_day/offer_requirement_H2o/ketp.o \
./src/stics_day/offer_requirement_H2o/shutwall.o \
./src/stics_day/offer_requirement_H2o/solnu.o \
./src/stics_day/offer_requirement_H2o/transpi.o 


# Each subdirectory must supply rules for building sources it contributes
src/stics_day/offer_requirement_H2o/%.o: ../src/stics_day/offer_requirement_H2o/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/stics_day/offer_requirement_H2o/besoinsEnEauDeLaPlante.o: ../src/stics_day/offer_requirement_H2o/besoinsEnEauDeLaPlante.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o

src/stics_day/offer_requirement_H2o/calraero.o: ../src/stics_day/offer_requirement_H2o/calraero.f90

src/stics_day/offer_requirement_H2o/etatsurf.o: ../src/stics_day/offer_requirement_H2o/etatsurf.f90

src/stics_day/offer_requirement_H2o/ketp.o: ../src/stics_day/offer_requirement_H2o/ketp.f90 src/utilities/Divers.o

src/stics_day/offer_requirement_H2o/shutwall.o: ../src/stics_day/offer_requirement_H2o/shutwall.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Climate/Station.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/outputs/history/Messages.o

src/stics_day/offer_requirement_H2o/solnu.o: ../src/stics_day/offer_requirement_H2o/solnu.f90 src/outputs/history/Messages.o

src/stics_day/offer_requirement_H2o/transpi.o: ../src/stics_day/offer_requirement_H2o/transpi.f90


