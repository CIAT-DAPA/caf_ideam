############################
### IRRI - April - 2019  ###
### Camilo Barrios Perez ###
############################

###############################################################################
### Script to convert daily rainfall data into accumulated monthly rainfall ###
###############################################################################

##############################
### Import data from excel ###
##############################

### Setting the working directory ###

setwd("D:/OneDrive - CGIAR/CAF-IDEAM Project/Daily climate information/Cordoba")

## Coordinates Stations ##

coordinates = read.table("Coordenadas_Estaciones_Cordoba.csv", head=T, sep = ",")

## List the weather station files in the working directory ##

stations = list.files("Cordoba",pattern=".csv")
stations.copy = list.files("Cordoba",pattern=".csv")

stations[1] = stations.copy[1]  
stations[2] = stations.copy[5]  
stations[3] = stations.copy[6]  
stations[4] = stations.copy[7]  
stations[5] = stations.copy[8]  
stations[6] = stations.copy[9]  
stations[7] = stations.copy[10]  
stations[8] = stations.copy[11]  
stations[9] = stations.copy[12]  
stations[10] = stations.copy[2]  
stations[11] = stations.copy[3]  
stations[12] = stations.copy[4]  

## List to store the montlhy data for each station ##

Monthly_data = list()

    for (j in 1:nrow(coordinates)){
  
      ## Read daily climate data for each station ##
      
      data=read.table(paste("Cordoba/",stations[j],sep = ""),header=T, sep=",")
      
      ##############################################
      ### Creating the vector with monthly dates ###
      ##############################################
      
      fecha=unique(data$Date)
      
      ##############################################################
      ### Empty table called "data_monthly" with two columns and ###
      ### rows number equal to those of "fecha" vector           ###
      ##############################################################
      
      data_monthly=matrix(NA,ncol=2,nrow=length(fecha))
      
      #############################################################
      ### Loop to compute the accumulated monthly precipitation ###
      #############################################################
      
      for(i in 1:length(fecha)){
        data_sub=data[data$Date==fecha[i],]
        data_monthly[i,]=c(dim(data_sub)[1],sum(data_sub$RAIN,na.rm=T))}
      
      result_monthly=data.frame(fecha,data_monthly)
      
      ###################
      ### Column names ##
      ###################
      
      names(result_monthly)[1:3] = c("Date","Num",coordinates[j,3])
      
      ###################################
      ## Store montly data in the list ##
      ###################################
      
      Monthly_data[[j]] = result_monthly
    
    }

############################################################################
### Empty table to store the precipitation monthly data for each station ###
############################################################################

Precipitation_monthly = matrix(NA,ncol=nrow(coordinates),nrow=404)

for (i in 1:nrow(coordinates)){
  
  Precipitation_monthly[1:nrow(Monthly_data[[i]]),i] = Monthly_data[[i]][,3]
  
}

####################
### Station names ##
####################

Precipitation_monthly = as.data.frame(Precipitation_monthly)

for (i in 1:nrow(coordinates)){
  
  names(Precipitation_monthly)[i] = c(coordinates[i,3])
  
}

Precipitation_monthly = cbind(Monthly_data[[4]]$Date, Precipitation_monthly)
names(Precipitation_monthly)[1] = c("Date")

#######################################
## Export monthly precipitation data ##
#######################################

write.table(Precipitation_monthly, "D:/OneDrive - CGIAR/CAF-IDEAM Project/Activity 2_Literature review/Calculo SPEI/Datos climáticos históricos_Cordoba/Precipitacion Mensual/Precipitacion_Mensual_Cordoba.csv",
            row.names = F, sep = ",")



