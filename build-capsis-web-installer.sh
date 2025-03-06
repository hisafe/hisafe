# Script to build the Capsis-Web installer
# Contains only free models to be distributed to everybody on the Capsis web site
# To change the Capsis version:
# - capsis.app.Starter: CAPSIS_VERSION = "4.2.4";
# - installer.xml: <appversion>4.2.4
# The built file capsis-setup.jar may be renamed into something like 
# 'capsis-4.2.4-setup.jar' before distribution
# This is a script for Linux

sh ant clean installer -Dmodules="lerfob/commons/**,lerfob/abial/**,lerfob/fagacees/**,modispinaster/**,mountain/**,twoe/**,genloader/**,organon/**"
