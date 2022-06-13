getStationLocationAltitude<-function(st){
  url<-paste0("http://meteo.navarra.es/estaciones/estacion_detalle.cfm?idestacion=",st[1])
  webpage<-getURL(url)
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  
  altitude<-as.integer(gsub("[^0-9]", "",webpage[grepl(paste0(".*Altitud.*"),webpage)]))
  X<-as.integer(gsub("[^0-9]", "",webpage[grepl(paste0(".*X:.*"),webpage)]))
  Y<-as.integer(gsub("[^0-9]", "",webpage[grepl(paste0(".*Y:.*"),webpage,)]))
  
  return(c("Station.ID"=st[1],"Station.Name"=st[3],"Altitude"=altitude,"UTMX"=X,"UTMY"=Y))
}
