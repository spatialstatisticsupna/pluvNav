#' Interpolate precipitation data for satelite-like rasters using Tps
#'
#' @param pluvDownFolder path to the folder containing the data
#' @param dates vector of dates to interpolate
#' @param type type of stations to use for the Tps model, either \code{"AUTO"},\code{"MAN"}, or
#' \code{c("AUTO","MAN")}
#' @param template  \code{RasterLayer} to use as a template for prediction
#' @importFrom raster interpolate clamp addLayer
#' @importFrom fields Tps
#' @export
#'
interpolatePrecTps <- function(pluvDownFolder,dates,type = c("AUTO","MAN"),template) {
  # validate inputs
  if (!dir.exists(pluvDownFolder)) {
    message("Download folder not found")
  }

  if (!min(type) %in% c("AUTO","MAN")) {
    message("Invalid type parameter, use \"AUTO\", \"MAN\", or c(\"AUTO\",\"MAN\")")
    return(-1)
  }

  start.year <- as.numeric(format(dates[1],"%Y"))
  end.year <- as.numeric(format(dates[length(dates)],"%Y"))

  if ("AUTO" %in% type) {
    rds.files.auto <- list.files(path = pluvDownFolder, pattern = "navarra_day_by_day_AUTO_.*\\.RDS")
    rds.start.auto <- as.numeric(gsub("navarra_day_by_day_AUTO_(\\d{4})-\\d{4}.RDS","\\1",rds.files.auto))
    rds.end.auto <- as.numeric(gsub("navarra_day_by_day_AUTO_\\d{4}-(\\d{4}).RDS","\\1",rds.files.auto))
    rds.files.auto <- rds.files.auto[which(start.year > rds.start.auto)]
    rds.files.auto <- rds.files.auto[which(end.year <= rds.end.auto)]
    if (length(rds.files.auto) == 0) {
      message("No automatic data found for the chosen dates")
    }
    df.auto <- readRDS(file = file.path(pluvDownFolder,rds.files.auto[1]))
    df.auto.prec <- df.auto[,c("Fecha.hora","Precipitaci\u00f3n.acumulada.l.m\u00b2","Station.ID")]
  }
  if ("MAN" %in% type) {
    rds.files.man <- list.files(path = pluvDownFolder, pattern = "navarra_day_by_day_MAN_.*\\.RDS")
    rds.start.man <- as.numeric(gsub("navarra_day_by_day_MAN_(\\d{4})-\\d{4}.RDS$","\\1",rds.files.man))
    rds.end.man <- as.numeric(gsub("navarra_day_by_day_MAN_\\d{4}-(\\d{4}).RDS$","\\1",rds.files.man))
    rds.files.man <- rds.files.man[which(start.year > rds.start.man)]
    rds.files.man <- rds.files.man[which(end.year <= rds.end.man)]
    if (length(rds.files.man) == 0) {
      message("No manual data found for the chosen dates")
    }
    df.man <- readRDS(file = file.path(pluvDownFolder,rds.files.man[1]))
    df.man.prec <- df.man[,c("Fecha.hora","Precipitaci\u00f3n.acumulada.l.m\u00b2","Station.ID")]
  }
  if (length(type) == 2) {
    df.prec <- rbind(df.auto.prec,df.man.prec)
  }

  df.station.locations <- read.csv(file.path(pluvDownFolder,"stations_position_altitude.csv"))
  pred.stack <- raster::stack()
  dates <- format(dates,"%Y/%m/%d")
  for (date in dates) {
    print(date)
    df.date.prec <- df.prec[df.prec$Fecha.hora == date,]
    full.df = merge(x = df.date.prec, y = df.station.locations)
    # coordinates and values
    xy <- cbind(as.numeric(full.df$"UTMX"),as.numeric(full.df$"UTMY"))
    v <- full.df$"Precipitaci\u00f3n.acumulada.l.m\u00b2"
    # TPS model
    tps <- Tps(xy[!is.na(v),], v[!is.na(v)])
    # use model to predict over the template raster
    int.raster <- clamp(interpolate(object=template,model=tps),
                       lower=0,
                       upper=Inf,
                       useValues=TRUE)
    names(int.raster) <- date
    pred.stack <- addLayer(pred.stack,int.raster)
  }

  return(pred.stack)

}
