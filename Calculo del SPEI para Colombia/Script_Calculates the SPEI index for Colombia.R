###########################################################################################
### Script to calculate the SPEI index for rice and maize producing regions in Colombia. ##
### Created by: Camilo Barrios-Perez. MSc, PhD; c.barrios@cgiar.org                      ##  
### Project: CAF-IDEAM                                                                   ##
### Date: May 5, 2022                                                                    ##
###########################################################################################

###############################
## Set the working directory ##
###############################

setwd("D:/OneDrive - CGIAR/CAF-IDEAM Project/Activity 2_Literature review/Calculo SPEI/Datos climáticos históricos_Cordoba")

####################
## Load libraries ##
####################

library(SPEI)

####################################################################################################################
## Computation of Reference Evapotranspiration (ETo)                                                              ##
## Method: Hargreaves equation (1994)                                                                             ##
## This method estimates ETo based on maximum and minimum air temperature, and the extraterrestrial radiation.    ##
## If solar radiation data is not available, it can be estimated from the latitude lat and the month of the year. ##
####################################################################################################################

## Import maximum and minimum temperature data for all stations ##
Tmax_data = read.table("Temperatura Mensual/Tmax_Mensual_Cordoba.csv", head=T, sep = ",")
Tmin_data = read.table("Temperatura Mensual/Tmin_Mensual_Cordoba.csv", head=T, sep = ",")

## Import a table with the latitude, longitude and elevation of each station ##
location_data = read.table("Coordenadas_Estaciones_Cordoba.csv", head=T, sep = ",")

## Create an empty vector to hold the ETo values for each weather station ##
ET_Data = matrix(NA,ncol=ncol(Tmax_data),nrow=nrow(Tmax_data))

## Fill the first two columns of the "ET_Data" table with year and month records ##
ET_Data [,1] = Tmax_data[,1]
ET_Data [,2] = Tmax_data[,2]

## Computation of ETo for each station ##

for (i in 1:nrow(location_data)){
  
  ET_Data [,i+2] = hargreaves(Tmin_data[,i+2],Tmax_data[,i+2],lat=location_data[i,2])
  
}

## Give names to each column of the table "ET_Data" ##

ET_Data = as.data.frame(ET_Data)
names(ET_Data)[1:2] = c("Year", "Month")

for(i in 1:nrow(location_data)){
  
  names(ET_Data)[i+2] = c(location_data[i,7])
  
}

## Export ETo data ##

write.table(ET_Data, "ETo Mensual_Hargreaves/ETo_Mensual_Cordoba.csv", sep = ",", row.names=F)

#######################################################################################
## Computation of the SPEI index for 1, 3, 6 and 12 months, for each weather station ##
#######################################################################################

## Import monthly precipitation data for all stations ##
Rain_data = read.table("Precipitacion Mensual/Precipitacion_Mensual_Cordoba.csv", head=T, sep = ",")

    ####################
    ## SPEI One Month ##
    ####################

    ## Create an empty vector to hold the SPEI_1 values for each weather station ##
    SPEI_1 = matrix(NA,ncol=ncol(Tmax_data),nrow=nrow(Tmax_data))
    
    ## Fill the first two columns of the "SPEI_1" table with year and month records ##
    SPEI_1 [,1] = Tmax_data[,1]
    SPEI_1 [,2] = Tmax_data[,2]
    
    ## Computation of SPEI_1 for each station ##
    
    for (i in 1:nrow(location_data)){
      
      SPEI = spei(Rain_data[,i+2] - ET_Data[,i+2],1)
      SPEI_1 [,i+2] = as.numeric(SPEI$fitted)
      
    }
    
    ## Give names to each column of the table "SPEI_1" ##
    
    SPEI_1 = as.data.frame(SPEI_1)
    names(SPEI_1)[1:2] = c("Year", "Month")
    
    for(i in 1:nrow(location_data)){
      
      names(SPEI_1)[i+2] = c(location_data[i,7])
      
    }
   
    ## Export SPEI_1 data ##
    write.table(SPEI_1, "Indice SPEI/SPEI_1Mes_Cordoba.csv", sep = ",", row.names=F)
    
    
    #######################
    ## SPEI Three Months ##
    #######################
    
    ## Create an empty vector to hold the SPEI_3 values for each weather station ##
    SPEI_3 = matrix(NA,ncol=ncol(Tmax_data),nrow=nrow(Tmax_data))
    
    ## Fill the first two columns of the "SPEI_3" table with year and month records ##
    SPEI_3 [,1] = Tmax_data[,1]
    SPEI_3 [,2] = Tmax_data[,2]
    
    ## Computation of SPEI_3 for each station ##
    
    for (i in 1:nrow(location_data)){
      
      SPEI = spei(Rain_data[,i+2] - ET_Data[,i+2],3)
      SPEI_3 [,i+2] = as.numeric(SPEI$fitted)
      
    }
    
    ## Give names to each column of the table "SPEI_3" ##
    
    SPEI_3 = as.data.frame(SPEI_3)
    names(SPEI_3)[1:2] = c("Year", "Month")
    
    for(i in 1:nrow(location_data)){
      
      names(SPEI_3)[i+2] = c(location_data[i,7])
      
    }
    
    ## Export SPEI_3 data ##
    write.table(SPEI_3, "Indice SPEI/SPEI_3Meses_Cordoba.csv", sep = ",", row.names=F)

    #####################
    ## SPEI Six Months ##
    #####################
    
    ## Create an empty vector to hold the SPEI_6 values for each weather station ##
    SPEI_6 = matrix(NA,ncol=ncol(Tmax_data),nrow=nrow(Tmax_data))
    
    ## Fill the first two columns of the "SPEI_6" table with year and month records ##
    SPEI_6 [,1] = Tmax_data[,1]
    SPEI_6 [,2] = Tmax_data[,2]
    
    ## Computation of SPEI_6 for each station ##
    
    for (i in 1:nrow(location_data)){
      
      SPEI = spei(Rain_data[,i+2] - ET_Data[,i+2],6)
      SPEI_6 [,i+2] = as.numeric(SPEI$fitted)
      
    }
    
    ## Give names to each column of the table "SPEI_6" ##
    
    SPEI_6 = as.data.frame(SPEI_6)
    names(SPEI_6)[1:2] = c("Year", "Month")
    
    for(i in 1:nrow(location_data)){
      
      names(SPEI_6)[i+2] = c(location_data[i,7])
      
    }
    
    ## Export SPEI_6 data ##
    write.table(SPEI_6, "Indice SPEI/SPEI_6Meses_Cordoba.csv", sep = ",", row.names=F)
    

    ########################
    ## SPEI Twelve Months ##
    ########################
    
    ## Create an empty vector to hold the SPEI_12 values for each weather station ##
    SPEI_12 = matrix(NA,ncol=ncol(Tmax_data),nrow=nrow(Tmax_data))
    
    ## Fill the first two columns of the "SPEI_12" table with year and month records ##
    SPEI_12 [,1] = Tmax_data[,1]
    SPEI_12 [,2] = Tmax_data[,2]
    
    ## Computation of SPEI_12 for each station ##
    
    for (i in 1:nrow(location_data)){
      
      SPEI = spei(Rain_data[,i+2] - ET_Data[,i+2],12)
      SPEI_12 [,i+2] = as.numeric(SPEI$fitted)
      
    }
    
    ## Give names to each column of the table "SPEI_12" ##
    
    SPEI_12 = as.data.frame(SPEI_12)
    names(SPEI_12)[1:2] = c("Year", "Month")
    
    for(i in 1:nrow(location_data)){
      
      names(SPEI_12)[i+2] = c(location_data[i,7])
      
    }
    
    ## Export SPEI_12 data ##
    write.table(SPEI_12, "Indice SPEI/SPEI_12Meses_Cordoba.csv", sep = ",", row.names=F)
    