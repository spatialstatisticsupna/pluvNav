#' Download pluviometric data from all pluviometric stations
#'
#' @param pluvDownFolder path for the pluviometric data to be stored
#' @param start_year starting year for the data download
#' @param end_year ending year for the data download
#' @param station.types either \code{"MAN"} for data from manual stations,
#' \code{"AUTO"} for data from automatic stations or \code{c("MAN","AUTO")} for
#' data from both types of stations
#' @param download logical argument. If \code{TRUE}, downloads data for all the
#' available stations and dates, overwriting the ones previously saved in the path
#' \code{pluvDownFolder}
#'
#' @importFrom RCurl getCurlHandle curlSetOpt
#' @export
#'
#' @examples # TODO some example?
downloadAllStations<-function(pluvDownFolder,
                              start_year,
                              end_year,
                              station.types=c("AUTO","MAN"),
                              download=T){

  if(!file.exists(paste0(pluvDownFolder))){
    dir.create(paste0(pluvDownFolder))
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
  print(paste0("Data will save in '",pluvDownFolder,"' folder."))
  years=start_year:end_year
  #station number name and id
  stations<-getStationList()


  lapply(stations,
         downloadStationData,
         years=years,
         r.folder=pluvDownFolder,
         curl=curl,
         download=download)
  #get station location and altitude
  st.info.file<-paste0(pluvDownFolder,"/stations_position_altitude.csv")
  st.info<-lapply(stations,getStationLocationAltitude)
  st.info<-as.data.frame(do.call(rbind,st.info))
  write.csv(st.info, file = paste0(pluvDownFolder,"/stations_position_altitude.csv"),row.names = F)



  #unify all csv in one file
  r<-lapply(station.types,unify.csv,years,stations,pluvDownFolder)
}

