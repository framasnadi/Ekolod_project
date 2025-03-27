# Ekolod_project
This repo contains all the scripts and info for hydroacoustic data processing developed in the context of the project _"Has the large scale fishery in coastal areas reduced the abundance of herring in the archipelago?"_ (Stockholm University)

**Project Summary**

In order to assess if the recent years intensive large-scale fishery in coastal areas have reduced the abundance of herring in archipelago areas, historical data on inshore herring abundances are needed. However, herring stock estimates are currently available only from fisheries surveys in off-shore areas. The aim of this project is to analyse previously unpublished hydroacoustic data from coastal areas, collected in 1985-2012. Such an extensive dataset would allow comparisons with the current situation, to explore if inshore herring abundances nowadays is lower (as expected) than before. The old data, however, was collected with different kinds of echo sonars and were processed with different software. By making the new and older datasets compatible, we will create a baseline of herring abundance in the archipelago so that future studies can be compared to the old data.

File description:

1) ```dataprocessing_from_ESP3output.R``` :  Processing ESP3 exported data after echo integration in ESP3
   
3) ```App_processESP3output.R``` :  R script to build Shiny APP to process data for online user (https://accessfjordproject.shinyapps.io/processESP3output/)

4) ```TStot_Echohist_Aug_09_2022.xlsx``` : example file of dbTS from ESP3: see User manual point 2: "Single Target and Echoes detection"

5) ```NASCtot_Aug_09_2022.xlsx``` : example file of dbNASC from ESP3: see User manual point 3: "Echo integration of the transect"

6) ``Processing hydroacoustic User manual.pdf`` : User manual for processing hydroacoustic data with ESP3 and ShinyAppR


   
* Project Leader: Agnes Karlson (Stockholm University - DEEP)

* Authors: Francesco Masnadi (Stockholm University - DEEP)  
