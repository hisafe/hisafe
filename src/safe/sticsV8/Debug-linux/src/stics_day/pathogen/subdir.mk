################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/stics_day/pathogen/duree_vie_spore.f90 \
../src/stics_day/pathogen/inputs_patho_ext.f90 \
../src/stics_day/pathogen/maturation_oospores.f90 \
../src/stics_day/pathogen/mod_patho.f90 \
../src/stics_day/pathogen/patho_boucle_journaliere.f90 \
../src/stics_day/pathogen/patho_dispersion.f90 \
../src/stics_day/pathogen/patho_infection.f90 \
../src/stics_day/pathogen/patho_init.f90 \
../src/stics_day/pathogen/patho_latence.f90 \
../src/stics_day/pathogen/patho_lecture_param.f90 \
../src/stics_day/pathogen/patho_outputs.f90 \
../src/stics_day/pathogen/patho_receptivite.f90 \
../src/stics_day/pathogen/patho_sporulation.f90 \
../src/stics_day/pathogen/patho_zero_fin.f90 \
../src/stics_day/pathogen/surface.f90 

OBJS += \
./src/stics_day/pathogen/duree_vie_spore.o \
./src/stics_day/pathogen/inputs_patho_ext.o \
./src/stics_day/pathogen/maturation_oospores.o \
./src/stics_day/pathogen/mod_patho.o \
./src/stics_day/pathogen/patho_boucle_journaliere.o \
./src/stics_day/pathogen/patho_dispersion.o \
./src/stics_day/pathogen/patho_infection.o \
./src/stics_day/pathogen/patho_init.o \
./src/stics_day/pathogen/patho_latence.o \
./src/stics_day/pathogen/patho_lecture_param.o \
./src/stics_day/pathogen/patho_outputs.o \
./src/stics_day/pathogen/patho_receptivite.o \
./src/stics_day/pathogen/patho_sporulation.o \
./src/stics_day/pathogen/patho_zero_fin.o \
./src/stics_day/pathogen/surface.o 


# Each subdirectory must supply rules for building sources it contributes
src/stics_day/pathogen/%.o: ../src/stics_day/pathogen/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/stics_day/pathogen/duree_vie_spore.o: ../src/stics_day/pathogen/duree_vie_spore.f90

src/stics_day/pathogen/inputs_patho_ext.o: ../src/stics_day/pathogen/inputs_patho_ext.f90 src/stics_day/pathogen/mod_patho.o

src/stics_day/pathogen/maturation_oospores.o: ../src/stics_day/pathogen/maturation_oospores.f90

src/stics_day/pathogen/mod_patho.o: ../src/stics_day/pathogen/mod_patho.f90

src/stics_day/pathogen/patho_boucle_journaliere.o: ../src/stics_day/pathogen/patho_boucle_journaliere.f90 src/stics_day/pathogen/mod_patho.o

src/stics_day/pathogen/patho_dispersion.o: ../src/stics_day/pathogen/patho_dispersion.f90

src/stics_day/pathogen/patho_infection.o: ../src/stics_day/pathogen/patho_infection.f90

src/stics_day/pathogen/patho_init.o: ../src/stics_day/pathogen/patho_init.f90 src/stics_day/pathogen/mod_patho.o

src/stics_day/pathogen/patho_latence.o: ../src/stics_day/pathogen/patho_latence.f90

src/stics_day/pathogen/patho_lecture_param.o: ../src/stics_day/pathogen/patho_lecture_param.f90 src/stics_day/pathogen/mod_patho.o

src/stics_day/pathogen/patho_outputs.o: ../src/stics_day/pathogen/patho_outputs.f90

src/stics_day/pathogen/patho_receptivite.o: ../src/stics_day/pathogen/patho_receptivite.f90

src/stics_day/pathogen/patho_sporulation.o: ../src/stics_day/pathogen/patho_sporulation.f90

src/stics_day/pathogen/patho_zero_fin.o: ../src/stics_day/pathogen/patho_zero_fin.f90 src/stics_day/pathogen/mod_patho.o

src/stics_day/pathogen/surface.o: ../src/stics_day/pathogen/surface.f90 src/stics_day/pathogen/mod_patho.o


