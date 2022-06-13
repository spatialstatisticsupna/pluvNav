library(RCurl)
library(XML)
#' Title
#'
#' @return
#' @export
#'
#' @examples
getStationList<-function(){
  theurl <- "http://meteo.navarra.es/estaciones/mapadeestaciones.cfm"
  webpage <- getURL(theurl)
  # Process escape characters
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  stations<-webpage[grepl("*.doLayer1.*",webpage)]
  stations<-stations[-1]
  st.data<-gsub("^.*?\\((.*)\\)[^)]*$", "\\1", stations)
  st.data<-gsub("'","",st.data)
  #st.data<-gsub(" ","%20",st.data)
  st.data<-lapply(st.data,FUN=function(x)return(unlist(strsplit(x,","))[c(1,3,5)]))
  return(st.data)
}

