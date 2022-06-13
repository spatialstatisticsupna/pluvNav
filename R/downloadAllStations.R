downloadAllStations<-function(pluvDownfolder,
                              start_year,
                              end_year,
                              station.types=c("AUTO","MAN"),
                              download=T){

  if(!file.exists(paste0(pluvDownfolder))){
    dir.create(paste0(pluvDownfolder))
  }

  curl = getCurlHandle()
  curlSetOpt(
    useragent = "Firefox/23.0",
    followlocation = TRUE ,
    autoreferer = TRUE ,
    curl = curl
  )

  print("*********************************")
  print("*         CONFIGURATION         *")
  print("*********************************")
  print(paste0("Start Date: ", start_year))
  print(paste0("End Date: ", end_year))
  print(paste0("Station types: ",toString(station.types)))
  print(paste0("Data will save in '",pluvDownfolder,"' folder."))
  years=start_year:end_year
  #station number name and id
  stations<-getEstationList()


  lapply(stations,
         downloadStationData,
         years=years,
         r.folder=pluvDownfolder,
         curl=curl,
         download=download)
  #get station location and altitude
  st.info.file<-paste0(pluvDownfolder,"/stations_position_altitude.csv")
  st.info<-lapply(stations,getStationLocationAltitude)
  st.info<-as.data.frame(do.call(rbind,st.info))
  write.csv(st.info, file = paste0(pluvDownfolder,"/stations_position_altitude.csv"),row.names = F)



  #unify all csv in one file
  r<-lapply(station.types,unify.csv,years,stations,pluvDownfolder)
}

