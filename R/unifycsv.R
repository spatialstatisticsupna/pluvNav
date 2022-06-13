library(plyr)

#' Title
#'
#' @param station.type
#' @param years
#' @param stations
#' @param d.folder
#'
#' @return
#' @export
#'
#' @examples
unify.csv<-function(station.type,years,stations,d.folder){
  print(paste0("Merge all ",station.type," data in one csv."))
  st.type<-lapply(stations,FUN=function(x)if(x[2]==station.type)return(x))
  st.type<-st.type[!sapply(st.type,is.null)]

  st.names<-unlist(lapply(stations,FUN=function(x)return(x[3])))
  csv.files<-list.files(paste0(d.folder,"/",st.names,"_",station.type),pattern = "\\.csv$",full.names = T)
  y.comp<-lapply(paste0(".*",years,".*"),grepl,csv.files)
  y.comp<-as.logical(Reduce(`+`, y.comp))

  files<-csv.files[y.comp]

  data<-lapply(files,readPluvCSV,stations,header=TRUE,sep=";",dec = ".")
  data<-do.call(rbind.fill,data)

  print(paste0("Result in ",d.folder,"/navarra_day_by_day_",station.type,".csv file."))
  data<-data[!is.na(data$Station.ID),]
  write.csv(data, file = paste0(d.folder,"/navarra_day_by_day_",station.type,"_",years[1],"-",years[length(years)],".csv"),row.names = F)
  saveRDS(data,paste0(d.folder,"/navarra_day_by_day_",station.type,"_",years[1],"-",years[length(years)],".RDS"))
}

getStationID<-function(st.name,stations){
  id<-lapply(stations,function(x,st.name)if(x[3]==st.name)return(x[1]),st.name)
  return(unlist(id[!sapply(id,is.null)]))
}

readPluvCSV<-function(path,stations,...){
  st.name<-gsub("\\.*_.*$","",basename(dirname(path)))
  st.id<-getStationID(st.name,stations)
  aux<-read.csv(path,...)
  aux$Fecha.hora<-format(as.Date(aux$Fecha.hora,"%d/%m/%Y %H:%M:%S"), "%Y/%m/%d")
  aux$Station.ID<-st.id
  aux<-aux[-which(names(aux)=="X")]
  return(aux)
}
