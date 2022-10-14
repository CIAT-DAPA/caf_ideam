##################################################################################
## Created by: Camilo Barrios Perez                                             ##
## Description: This script implements pedo-transfer functions to calculate the ##
## saturated soil water content (m3 m-3) for colombian soils.                   ##
##################################################################################

library(raster)
library(rgdal)
library(fields)
library(rasterVis)
library(maps)
library(maptools)
library(rgeos)

###############################
## Set the working directory ##
###############################

setwd("D:/OneDrive - CGIAR/CAF-IDEAM Project/Capitulo 3/Analisis_IDH_Maiz_Arroz/Base de datos/Variables de suelo por region/Cordoba")

############################################
## Import soil information from SoilGrids ##
############################################

  ##################
  ## Clay content ##
  ##################

  Clay = list.files(path = paste(getwd(),"/","Clay content",sep =""), pattern="tif$", full.names = TRUE, recursive = FALSE)
  Clay_2 = Clay
  
  Clay[1] = Clay_2 [1]
  Clay[2] = Clay_2 [4]
  Clay[3] = Clay_2 [2]
  Clay[4] = Clay_2 [3]
  
  Clay_rasters = lapply(Clay, raster)
  Clay_rasters = stack(Clay_rasters)

  ## Units convertion ##
  
  Clay_final = (Clay_rasters/10) # This converts Clay content units from g/kg to %
  

  ##################
  ## Sand content ##
  ##################
  
  Sand = list.files(path = paste(getwd(),"/","Sand content",sep =""), pattern="tif$", full.names = TRUE, recursive = FALSE)
  Sand_2 = Sand
  
  Sand[1] = Sand_2 [1]
  Sand[2] = Sand_2 [4]
  Sand[3] = Sand_2 [2]
  Sand[4] = Sand_2 [3]
  
  Sand_rasters = lapply(Sand, raster)
  Sand_rasters = stack(Sand_rasters)
  
  ## Units convertion ##
  
  Sand_final = (Sand_rasters/10) # This converts Sand content units from g/kg to %
  
  
  ##################
  ## Bulk density ##
  ##################
  
  BLD = list.files(path = paste(getwd(),"/","Bulk density",sep =""), pattern="tif$", full.names = TRUE, recursive = FALSE)
  BLD_2 = BLD
  
  BLD[1] = BLD_2 [1]
  BLD[2] = BLD_2 [4]
  BLD[3] = BLD_2 [2]
  BLD[4] = BLD_2 [3]
  
  BLD_rasters = lapply(BLD, raster)
  BLD_rasters = stack(BLD_rasters)
  
  ## Units convertion ##
  
  BLD_final = (BLD_rasters/100) # This converts BLD units from cg/cm3 to Mg/m3
  
  
  ########
  ## pH ##
  ########
  
  pH = list.files(path = paste(getwd(),"/","pH",sep =""), pattern="tif$", full.names = TRUE, recursive = FALSE)
  pH_2 = pH
  
  pH[1] = pH_2 [1]
  pH[2] = pH_2 [4]
  pH[3] = pH_2 [2]
  pH[4] = pH_2 [3]
  
  pH_rasters = lapply(pH, raster)
  pH_rasters = stack(pH_rasters)
  
  ## Units convertion ##
  
  pH_final = (pH_rasters/10) # This converts pH units
  
  
  #############################
  ## Cation Echange Capacity ##
  #############################
  
  CEC = list.files(path = paste(getwd(),"/","CEC",sep =""), pattern="tif$", full.names = TRUE, recursive = FALSE)
  CEC_2 = CEC
  
  CEC[1] = CEC_2 [1]
  CEC[2] = CEC_2 [4]
  CEC[3] = CEC_2 [2]
  CEC[4] = CEC_2 [3]
  
  CEC_rasters = lapply(CEC, raster)
  CEC_rasters = stack(CEC_rasters) # Units in cmol/kg
  
  ## Units convertion ##
  
  CEC_final = (CEC_rasters/10) # This converts CEC units from mmol(c)/cm3 to cmol/kg
  
#####################################################################################
## Computation of saturated soil water content (SSWC) (m3 m-3) per each soil layer ##
#####################################################################################
  
  ###########################
  ## SSWC for 0-5 cm depth ## 
  ###########################
  
  SSWC_0_5cm = 0.81799 + ((9.9*10^-4)*Clay_final[[1]]) - (0.3142*BLD_final[[1]]) + ((1.8*10^-4)*CEC_final[[1]]) + (0.00451*pH_final[[1]]) - ((5*10^-6)*(Sand_final[[1]])*(Clay_final[[1]]))     
  
  ###########################
  ## SSWC for 5-15 cm depth ## 
  ###########################
  
  SSWC_5_15cm = 0.81799 + ((9.9*10^-4)*Clay_final[[2]]) - (0.3142*BLD_final[[2]]) + ((1.8*10^-4)*CEC_final[[2]]) + (0.00451*pH_final[[2]]) - ((5*10^-6)*(Sand_final[[2]])*(Clay_final[[2]]))
  
  #############################
  ## SSWC for 15-30 cm depth ## 
  #############################
  
  SSWC_15_30cm = 0.81799 + ((9.9*10^-4)*Clay_final[[3]]) - (0.3142*BLD_final[[3]]) + ((1.8*10^-4)*CEC_final[[3]]) + (0.00451*pH_final[[3]]) - ((5*10^-6)*(Sand_final[[3]])*(Clay_final[[3]]))
  
  #############################
  ## SSWC for 30-60 cm depth ## 
  #############################
  
  SSWC_30_60cm = 0.81799 + ((9.9*10^-4)*Clay_final[[4]]) - (0.3142*BLD_final[[4]]) + ((1.8*10^-4)*CEC_final[[4]]) + (0.00451*pH_final[[4]]) - ((5*10^-6)*(Sand_final[[4]])*(Clay_final[[4]]))

################################################################
## Conversion of saturated soil moisture data (m3/m3) into mm ##
################################################################
  
  ######################################
  ## SSWC in mm for 0-5 cm soil depth ##
  ######################################
  
  SSWC_mm_0_5cm = SSWC_0_5cm*50 # 50 is the depth of the first soil layer (5 cm) in mm
  
  #######################################
  ## SSWC in mm for 5-15 cm soil depth ##
  #######################################
  
  SSWC_mm_5_15cm = SSWC_5_15cm*100 # 100 is the depth of the second soil layer (10 cm) in mm
  
  ########################################
  ## SSWC in mm for 15-30 cm soil depth ##
  ########################################
  
  SSWC_mm_15_30cm = SSWC_15_30cm*150 # 150 is the depth of the third soil layer (15 cm) in mm
  
  ########################################
  ## SSWC in mm for 30-60 cm soil depth ##
  ########################################
  
  SSWC_mm_30_60cm = SSWC_30_60cm*300 # 300 is the depth of the fourth soil layer (30 cm) in mm

#########################################
## Maximum soil water holding capacity ##
#########################################
  
  Max_SWHC = SSWC_mm_0_5cm + SSWC_mm_5_15cm + SSWC_mm_15_30cm + SSWC_mm_30_60cm

##########################################################################
## Extract the Max_SWHC data (mm) for specific locations in Meta region ##
##########################################################################

Stations_Meta = read.table ("D:/OneDrive - CGIAR/CAF-IDEAM Project/Capitulo 3/Analisis_IDH_Maiz_Arroz/Base de datos/Daily climate information/Llanos/Coordenadas_Estaciones_Meta_Casanare.csv", head=T, sep=",")
coordinates = as.data.frame(cbind(Stations_Meta$Lon, Stations_Meta$Lat))
names(coordinates)[1:2] = c("x", "y")

data_Max_SWHC = as.data.frame(extract(Max_SWHC, coordinates))
names(data_Max_SWHC)[1] = c("Max_SWHC")

data_Max_SWHC = cbind(Stations_Meta, data_Max_SWHC)

  ## Export Max_SWHC data (mm) ##

  write.table(data_Max_SWHC, "D:/OneDrive - CGIAR/CAF-IDEAM Project/Capitulo 3/Analisis_IDH_Maiz_Arroz/Base de datos/Maximum soil water holding capacity/Llanos/Llanos_Maximum soil water holding capacity.csv", row.names=F, sep=",")


