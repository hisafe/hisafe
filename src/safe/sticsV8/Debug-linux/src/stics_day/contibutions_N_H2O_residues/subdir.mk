################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/stics_day/contibutions_N_H2O_residues/ApportFeuillesMortes.f90 \
../src/stics_day/contibutions_N_H2O_residues/ApportResidusCulture.f90 \
../src/stics_day/contibutions_N_H2O_residues/CalculAutomatiqueFertilisation.f90 \
../src/stics_day/contibutions_N_H2O_residues/Fertilisations.f90 \
../src/stics_day/contibutions_N_H2O_residues/ResiduParamDec.f90 \
../src/stics_day/contibutions_N_H2O_residues/ResidusApportSurfaceSol.f90 \
../src/stics_day/contibutions_N_H2O_residues/ResidusMelangeSol.f90 \
../src/stics_day/contibutions_N_H2O_residues/ResidusParamDecMelange.f90 \
../src/stics_day/contibutions_N_H2O_residues/affectrrquinoapoquet.f90 \
../src/stics_day/contibutions_N_H2O_residues/apports.f90 \
../src/stics_day/contibutions_N_H2O_residues/apportsNparEngraisMineraux.f90 \
../src/stics_day/contibutions_N_H2O_residues/apportsNparPluieEtIrrigation.f90 \
../src/stics_day/contibutions_N_H2O_residues/apportsOrganiquesEtTravailDuSol.f90 \
../src/stics_day/contibutions_N_H2O_residues/calapNenUpvt.f90 \
../src/stics_day/contibutions_N_H2O_residues/calculApportsIrrigationEnUpvt.f90 \
../src/stics_day/contibutions_N_H2O_residues/denit.f90 \
../src/stics_day/contibutions_N_H2O_residues/eauEntrantSysteme.f90 \
../src/stics_day/contibutions_N_H2O_residues/irrig.f90 \
../src/stics_day/contibutions_N_H2O_residues/mineral.f90 \
../src/stics_day/contibutions_N_H2O_residues/module_apports.f90 \
../src/stics_day/contibutions_N_H2O_residues/nitrif.f90 \
../src/stics_day/contibutions_N_H2O_residues/perteng.f90 \
../src/stics_day/contibutions_N_H2O_residues/restitution_pature_organique.f90 \
../src/stics_day/contibutions_N_H2O_residues/restitution_pature_uree.f90 \
../src/stics_day/contibutions_N_H2O_residues/volatorg.f90 

OBJS += \
./src/stics_day/contibutions_N_H2O_residues/ApportFeuillesMortes.o \
./src/stics_day/contibutions_N_H2O_residues/ApportResidusCulture.o \
./src/stics_day/contibutions_N_H2O_residues/CalculAutomatiqueFertilisation.o \
./src/stics_day/contibutions_N_H2O_residues/Fertilisations.o \
./src/stics_day/contibutions_N_H2O_residues/ResiduParamDec.o \
./src/stics_day/contibutions_N_H2O_residues/ResidusApportSurfaceSol.o \
./src/stics_day/contibutions_N_H2O_residues/ResidusMelangeSol.o \
./src/stics_day/contibutions_N_H2O_residues/ResidusParamDecMelange.o \
./src/stics_day/contibutions_N_H2O_residues/affectrrquinoapoquet.o \
./src/stics_day/contibutions_N_H2O_residues/apports.o \
./src/stics_day/contibutions_N_H2O_residues/apportsNparEngraisMineraux.o \
./src/stics_day/contibutions_N_H2O_residues/apportsNparPluieEtIrrigation.o \
./src/stics_day/contibutions_N_H2O_residues/apportsOrganiquesEtTravailDuSol.o \
./src/stics_day/contibutions_N_H2O_residues/calapNenUpvt.o \
./src/stics_day/contibutions_N_H2O_residues/calculApportsIrrigationEnUpvt.o \
./src/stics_day/contibutions_N_H2O_residues/denit.o \
./src/stics_day/contibutions_N_H2O_residues/eauEntrantSysteme.o \
./src/stics_day/contibutions_N_H2O_residues/irrig.o \
./src/stics_day/contibutions_N_H2O_residues/mineral.o \
./src/stics_day/contibutions_N_H2O_residues/module_apports.o \
./src/stics_day/contibutions_N_H2O_residues/nitrif.o \
./src/stics_day/contibutions_N_H2O_residues/perteng.o \
./src/stics_day/contibutions_N_H2O_residues/restitution_pature_organique.o \
./src/stics_day/contibutions_N_H2O_residues/restitution_pature_uree.o \
./src/stics_day/contibutions_N_H2O_residues/volatorg.o 


# Each subdirectory must supply rules for building sources it contributes
src/stics_day/contibutions_N_H2O_residues/%.o: ../src/stics_day/contibutions_N_H2O_residues/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -fPIC -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/stics_day/contibutions_N_H2O_residues/ApportFeuillesMortes.o: ../src/stics_day/contibutions_N_H2O_residues/ApportFeuillesMortes.f90

src/stics_day/contibutions_N_H2O_residues/ApportResidusCulture.o: ../src/stics_day/contibutions_N_H2O_residues/ApportResidusCulture.f90

src/stics_day/contibutions_N_H2O_residues/CalculAutomatiqueFertilisation.o: ../src/stics_day/contibutions_N_H2O_residues/CalculAutomatiqueFertilisation.f90

src/stics_day/contibutions_N_H2O_residues/Fertilisations.o: ../src/stics_day/contibutions_N_H2O_residues/Fertilisations.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o

src/stics_day/contibutions_N_H2O_residues/ResiduParamDec.o: ../src/stics_day/contibutions_N_H2O_residues/ResiduParamDec.f90

src/stics_day/contibutions_N_H2O_residues/ResidusApportSurfaceSol.o: ../src/stics_day/contibutions_N_H2O_residues/ResidusApportSurfaceSol.f90

src/stics_day/contibutions_N_H2O_residues/ResidusMelangeSol.o: ../src/stics_day/contibutions_N_H2O_residues/ResidusMelangeSol.f90

src/stics_day/contibutions_N_H2O_residues/ResidusParamDecMelange.o: ../src/stics_day/contibutions_N_H2O_residues/ResidusParamDecMelange.f90

src/stics_day/contibutions_N_H2O_residues/affectrrquinoapoquet.o: ../src/stics_day/contibutions_N_H2O_residues/affectrrquinoapoquet.f90

src/stics_day/contibutions_N_H2O_residues/apports.o: ../src/stics_day/contibutions_N_H2O_residues/apports.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o

src/stics_day/contibutions_N_H2O_residues/apportsNparEngraisMineraux.o: ../src/stics_day/contibutions_N_H2O_residues/apportsNparEngraisMineraux.f90

src/stics_day/contibutions_N_H2O_residues/apportsNparPluieEtIrrigation.o: ../src/stics_day/contibutions_N_H2O_residues/apportsNparPluieEtIrrigation.f90

src/stics_day/contibutions_N_H2O_residues/apportsOrganiquesEtTravailDuSol.o: ../src/stics_day/contibutions_N_H2O_residues/apportsOrganiquesEtTravailDuSol.f90

src/stics_day/contibutions_N_H2O_residues/calapNenUpvt.o: ../src/stics_day/contibutions_N_H2O_residues/calapNenUpvt.f90

src/stics_day/contibutions_N_H2O_residues/calculApportsIrrigationEnUpvt.o: ../src/stics_day/contibutions_N_H2O_residues/calculApportsIrrigationEnUpvt.f90 src/Stics.o src/inputs/Plant/Plante.o src/inputs/crop_management_parameters/Itineraire_technique.o

src/stics_day/contibutions_N_H2O_residues/denit.o: ../src/stics_day/contibutions_N_H2O_residues/denit.f90

src/stics_day/contibutions_N_H2O_residues/eauEntrantSysteme.o: ../src/stics_day/contibutions_N_H2O_residues/eauEntrantSysteme.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Plant/Plante.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o src/utilities/Divers.o

src/stics_day/contibutions_N_H2O_residues/irrig.o: ../src/stics_day/contibutions_N_H2O_residues/irrig.f90 src/outputs/history/Messages.o

src/stics_day/contibutions_N_H2O_residues/mineral.o: ../src/stics_day/contibutions_N_H2O_residues/mineral.f90

src/stics_day/contibutions_N_H2O_residues/module_apports.o: ../src/stics_day/contibutions_N_H2O_residues/module_apports.f90 src/Stics.o src/inputs/Climate/Climat.o src/inputs/Plant/Plante.o src/inputs/Soil/Sol.o src/inputs/crop_management_parameters/Itineraire_technique.o src/inputs/generals_parameters/Parametres_Generaux.o

src/stics_day/contibutions_N_H2O_residues/nitrif.o: ../src/stics_day/contibutions_N_H2O_residues/nitrif.f90

src/stics_day/contibutions_N_H2O_residues/perteng.o: ../src/stics_day/contibutions_N_H2O_residues/perteng.f90

src/stics_day/contibutions_N_H2O_residues/restitution_pature_organique.o: ../src/stics_day/contibutions_N_H2O_residues/restitution_pature_organique.f90

src/stics_day/contibutions_N_H2O_residues/restitution_pature_uree.o: ../src/stics_day/contibutions_N_H2O_residues/restitution_pature_uree.f90

src/stics_day/contibutions_N_H2O_residues/volatorg.o: ../src/stics_day/contibutions_N_H2O_residues/volatorg.f90


