#' Download data for a single pluviometric station
#'
#' @param st a vector containing (station.ID, station.type, station.name) in the
#' same way as in the elements from \code{getStationList()}
#' @param years vector of years to download
#' @param r.folder path to the folder to store the downloaded csv files
#' @param curl curl handle
#' @param download logical argument. If \code{TRUE}, downloads all data, overwriting
#' previous files in \code{r.folder}
#' @importFrom RCurl getURL url.exists CFILE curlPerform close
#' @export
#'
downloadStationData<-function(st,years,r.folder,curl,download){

  st.name<-st[3]
  st.url=paste0("http://meteo.navarra.es/estaciones/descargardatos_estacion.cfm?IDEstacion=",st[1])

  #station data folder
  r.path<-paste0(r.folder,"/",st.name,"_",st[2])
  r.path<-gsub('\u00e1','a',r.path)
  r.path<-gsub('\u00e9','e',r.path)
  r.path<-gsub('\u00ed','i',r.path)
  r.path<-gsub('\u00f3','o',r.path)
  r.path<-gsub('\u00fa','u',r.path)
  r.path<-gsub('\u00f1','n',r.path)
  if(!file.exists(r.path)){
    dir.create(r.path)
  }

  webpage <- getURL(st.url)
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  st.name<-sub("\\(","\\\\(",st.name)
  st.name<-sub("\\)","\\\\)",st.name)
  files<-webpage[grepl(paste0(".*",st.name,".*csv.*"),webpage)]
  files<-gsub("^.*?\\((.*)\\)[^)]*$", "\\1", files)
  files<-lapply(files,FUN=function(x)return(unlist(strsplit(x,","))[[4]]))
  files<-gsub("'","",unlist(files))
  files<-paste0(dirname(dirname(st.url)),files)
  files <- unique (grep(paste(years,collapse="|"),
                        files, value=TRUE))
  lapply(files,downloadCSV,r.path,curl,download)
}

downloadCSV<-function(url,r.path,curl,download){
  url<-gsub(" ","%20",url)
  fpath<-paste0(r.path,"/",gsub("%20"," ",basename(url)))
  fpath<-gsub('\u00e1','a',fpath)
  fpath<-gsub('\u00e9','e',fpath)
  fpath<-gsub('\u00ed','i',fpath)
  fpath<-gsub('\u00f3','o',fpath)
  fpath<-gsub('\u00fa','u',fpath)
  fpath<-gsub('\u00f1','n',fpath)
  if(download|!file.exists(fpath)){
    if(url.exists(url)){
      print(paste0("Downloading ",gsub("%20"," ",basename(url))," file."))
      f = CFILE(fpath, mode="wb")
      curlPerform(url = url, writedata = f@ref, noprogress=TRUE, curl=curl)
      close(f)
    }else{
      print(paste0("The url ",url," does not exists."))
    }
  }else{
    print(paste0("File ",basename(url)," already exists."))
  }


}


