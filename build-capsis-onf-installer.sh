# Script to build the Capsis-ONF installer
# Contains only the models to be distributed within ONF with the authors agreement
# The built file capsis-setup.jar may be renamed into something like 
# 'capsis-4.2.3-onf-setup.jar' before distribution
# This is a script for Linux

sh ant clean installer-sync -Dmodules="quebecmrnf/**,artemis/**,lerfob/commons/**,lerfob/abial/**,lerfob/fagacees/**,oakpine1/**,oakpine2/**, sylvestris/**,laricio/**,ca1/**,pnn2/**,nrg/**,samsara/**,mathilde/**,simcop/**,douglas/**"
