#######################################################################
## Created by: Camilo Barrios Perez                                  ##
## Description: This script calculates the IDH index for upland rice ##
## in Meta and Yopal departments.                                    ##
#######################################################################

###############################
## Set the working directory ##
###############################

setwd("D:/OneDrive - CGIAR/CAF-IDEAM Project/Capitulo 3/Analisis_IDH_Maiz_Arroz")

####################
## Load libraries ##
####################


####################################################################################################################
## Computation of Reference Evapotranspiration (ETo)                                                              ##
## Method: Hargreaves equation (1994)                                                                             ##
## This method estimates ETo based on maximum and minimum air temperature, and the extraterrestrial radiation.    ##
## If solar radiation data is not available, it can be estimated from the latitude lat and the month of the year. ##
####################################################################################################################

  ## Import station data and save it in a list ##
  
  stations_data = list()
  
  stations = list.files(path = paste(getwd(),"/Base de datos/Daily climate information/Llanos/Meta_Casanare",sep =""), pattern="csv$", full.names = TRUE, recursive = FALSE)
  
  for (i in 1:length(stations)){
    
    stations_data[[i]] = read.table(stations[i], head=T, sep=",")
    
  }
  
  ## Import a table with the latitude, longitude and elevation of each station ##
  
  location_data = read.table("Base de datos/Daily climate information/Llanos/Coordenadas_Elevacion_Estaciones_Meta_Casanare.csv", head=T, sep = ",")
  
  
  ###########################################################################
  ### Vector to be filled with Potential evapotranspiration (ETo) results ###
  ###########################################################################
  
  for (i in 1:length(stations_data)){
    
    stations_data[[i]]$ETo=matrix(NA,ncol=1,nrow=nrow(stations_data[[i]]))
    
  }
  
  ##############################################################################################
  ### The reference crop evapotranspiration rate using the Priestley-Taylor Model.           ###  
  ##############################################################################################
    for(j in 1:length(stations_data)){
      
      for (i in 1:nrow(stations_data[[j]])) {
        
        a= 1.26 # Priestley and Taylor evaporative coefficient, an empirical coefficient that may vary for different regions,
        # because it is influenced by soil moisture and vegetation types.
        
        b= 2.45 # Latent heat of vaporization at 20°C (2.45 MJkg???1).
        
        c= (stations_data[[j]]$TMAX[i] + stations_data[[j]]$TMIN[i])/2      # Mean temperature
        
        d= (4098*(0.6108*(exp((17.27*c)/(c+237.3)))))/((c+237.3)*(c+237.3)) # The slope of the relationship between saturation vapor pressure and air temperature (kPa ???C???1)
        
        e= (-0.000007*location_data$Elevation[j])+0.0666     # Psychrometric constant (??) for different altitudes (z)
        
        Sr= stations_data[[j]]$S.Rad[i] # The daily net radi- ation (MJ m???2 day???1) on
        
        ET_pt= a*(d/(d+e))*(Sr/b)  #Priestley-Taylor Model
        
        stations_data[[j]]$ETo[i]=round(ET_pt,3)
        
      }
  
    }
  
  ##########################################################################
  ### Vector to be filled with Crop evapotranspiration (ET_Crop) values; ###
  ### ET_Crop = ET0 * Kc                                                 ###
  ##########################################################################
  
  for (i in 1:length(stations_data)){
    
    stations_data[[i]]$ET_Crop= (stations_data[[i]]$ETo * stations_data[[i]]$Kc)
                                 
  }
  
  #######################################################################
  ### Convert daily agroclimatic information into accumulated monthly ###
  #######################################################################
  
  ## Save montlhy data in a list ##
  
  monthly_stations_data = list()
  
  for (i in 1:length(stations_data)){
    
    ##############################################
    ### Creating the vector with monthly dates ###
    ##############################################
    
    fecha=unique(stations_data[[i]]$Date)
    
    ###############################################################
    ### Empty table called "data_monthly" with four columns and ###
    ### rows number equal to those of "fecha" vector            ###
    ###############################################################
    
    data_monthly=matrix(NA,ncol=4,nrow=length(fecha))
    
    ####################################################
    ### Loop to compute the accumulated monthly data ###
    ####################################################
    
    for(j in 1:length(fecha)){
      data_sub=stations_data[[i]][stations_data[[i]]$Date==fecha[j],]
      data_monthly[j,]=c(dim(data_sub)[1],sum(data_sub$RAIN,na.rm=T), sum(data_sub$ETo,na.rm=T), sum(data_sub$ET_Crop,na.rm=T))}
    
    result_monthly=data.frame(fecha,data_monthly)
    
    ###################
    ### Colum names ###
    ###################
    
    names(result_monthly)[1:5]=c("Date","Num","Total_Rain", "Total_ETo", "Total_ET_Crop")
    
    #####################################################
    ## Save the final result of each station in a list ##
    #####################################################
    
    monthly_stations_data[[i]] = result_monthly
    
  }
  
########################
## Water Balance (WB) ##
########################
  
R_max = read.table("D:/OneDrive - CGIAR/CAF-IDEAM Project/Capitulo 3/Analisis_IDH_Maiz_Arroz/Base de datos/Maximum soil water holding capacity/Llanos/Llanos_Maximum soil water holding capacity.csv", head=T, sep=",")

  monthly_WB_stations = list()
  
for (i in 1:nrow(R_max)){ 
  
  R_max_sta.1 = R_max[i,4] # Maximum soil water holding capacity
    
  WB_station = monthly_stations_data[[i]] # Choose weather station
    
  WB_station$V.Reserva=matrix(NA,ncol=1,nrow=nrow(WB_station))
  WB_station$R.Almacena=matrix(NA,ncol=1,nrow=nrow(WB_station))
  WB_station$Deficit=matrix(NA,ncol=1,nrow=nrow(WB_station))
  WB_station$Exceso=matrix(NA,ncol=1,nrow=nrow(WB_station))
  WB_station$IDH=matrix(NA,ncol=1,nrow=nrow(WB_station))
  WB_station$IDH_Category=matrix(NA,ncol=1,nrow=nrow(WB_station))
  
 
for (m in 1:nrow(WB_station)){
          
      if (m == 1){
        
        ## Initial storage for the first location ##
        R.Almacena.Ini = R_max_sta.1 - (R_max_sta.1*0.3)
        
      } else {
      
        R.Almacena.Ini = WB_station$R.Almacena[m-1] # Previous month´s storage
      
      }
      
      ##-- Variación de la reserva para el mes actual --##
      
      if (WB_station$Total_Rain[m] > WB_station$Total_ETo[m]){
        
        WB_station$V.Reserva[m] = 0
        
      } else if (
        
        ((R.Almacena.Ini/(R_max_sta.1 + ((WB_station$Total_ETo[m] - WB_station$Total_Rain[m])/2))) * ((WB_station$Total_ETo[m]) - (WB_station$Total_Rain[m]))) > R.Almacena.Ini
        
      ){
        
        WB_station$V.Reserva[m] = R.Almacena.Ini
        
      } else {
        
        WB_station$V.Reserva[m] = ((R.Almacena.Ini/(R_max_sta.1 + ((WB_station$Total_ETo[m] - WB_station$Total_Rain[m])/2))) * ((WB_station$Total_ETo[m]) - (WB_station$Total_Rain[m])))
        
      }
      
      
      ##-- Almacenamiento para el mes actual --##
      
      if (WB_station$Total_Rain[m] < WB_station$Total_ETo[m]){
        
        WB_station$R.Almacena[m] = R.Almacena.Ini - WB_station$V.Reserva[m]
        
      } else if (
        
        (R.Almacena.Ini + (WB_station$Total_Rain[m] - WB_station$Total_ETo[m])) > R_max_sta.1
        
      ){
        
        WB_station$R.Almacena[m] = R_max_sta.1
        
      } else {
        
        WB_station$R.Almacena[m] = (R.Almacena.Ini + (WB_station$Total_Rain[m] - WB_station$Total_ETo[m]))
        
      }
      
      
      ##-- Deficit para el mes actual --##
      
      if (WB_station$Total_Rain[m] > WB_station$Total_ETo[m]){
        
        WB_station$Deficit[m] = 0
        
      } else {
        
        WB_station$Deficit[m] = WB_station$Total_ETo[m] - WB_station$Total_ET_Crop[m]
        
      }
      
      
      ##-- Exceso para el mes actual --##
      
      if (
        
        (R.Almacena.Ini + (WB_station$Total_Rain[m] - WB_station$Total_ETo[m])) > R_max_sta.1
        
      ){
        
        WB_station$Exceso[m] = (R.Almacena.Ini + (WB_station$Total_Rain[m] - WB_station$Total_ETo[m])) - R_max_sta.1
        
      } else {
        
        WB_station$Exceso[m] = 0
        
      }
      
      ##-- IDH para el primer mes --##
      
      WB_station$IDH[m] = ((WB_station$Total_ET_Crop[m] + (0.25*WB_station$Exceso[m]))/WB_station$Total_ETo[m])*100
  
      
      ## Determination of IDH category ###
  
      if (0 <= WB_station$IDH[m] && WB_station$IDH[m] <= 30){
        
        WB_station$IDH_Category[m] = "Muy_Seco"
        
      } else if (31 <= WB_station$IDH[m] && WB_station$IDH[m] <= 60){
        
        WB_station$IDH_Category[m] = "Seco"
        
      } else if (61 <= WB_station$IDH[m] && WB_station$IDH[m] <= 90){
        
        WB_station$IDH_Category[m] = "Semiseco"
        
      } else if (91 <= WB_station$IDH[m] && WB_station$IDH[m] <= 110){
        
        WB_station$IDH_Category[m] = "Adecuado"
        
      } else if (111 <= WB_station$IDH[m] && WB_station$IDH[m] <= 140){
        
        WB_station$IDH_Category[m] = "Semihumedo"
        
      } else if (141 <= WB_station$IDH[m] && WB_station$IDH[m] <= 170){
        
        WB_station$IDH_Category[m] = "Humedo"
        
      } else {
        
        WB_station$IDH_Category[m] = "Muy_Humedo"
        
      }
  
  }
  
  monthly_WB_stations [[i]] = WB_station
  
}
  
  #######################################
  ## Export Results by weather station ##
  #######################################
  
  for (i in 1:length(monthly_WB_stations)){
    
    write.table(monthly_WB_stations [[i]], paste("D:/OneDrive - CGIAR/Desktop/Final_Results_IDH_Upland_Rice/IDH_Arroz_",R_max[i,3],".csv", sep=""),row.names=FALSE, sep = ",")
    
  }
  