# set working directory------------------------------------------------------------
#setwd("C:/Users/RenkeLuehken/Google Drive/Project/VectorCompetenceRiskMaps")
setwd("G:/NeuAll/Project/VectorCompetenceRiskMaps")


# load libraries---------------------------------------------------------------------------
library(raster)
library(lubridate)
library(RcppRoll)
library(plyr)

# read data
testst <- lapply(2011:2017, function(x) stack(paste("G:/NeuAll/Research_projects/Extract_E_OBS_gridded_dataset/output/tg_0.25deg_reg_v17.0_europe_", 
                                                     x, ".grd", sep = "")))
#testst <- lapply(2007:2016, function(x) stack(paste("C:/Users/RenkeLuehken/Google Drive/Research_projects/Extract_E_OBS_gridded_dataset/output/tg_0.25deg_reg_v14.0_europe_", 
#                                                     x, ".grd", sep = "")))


#cropping_info <- c(-9, 25, 35, 55)

#ell <- lapply(testst, function(x) crop(x, cropping_info))

el <- stack(testst)

gdg2 <- getValues(el)

timeline <- function(start_year = 2007,end_year = 2016,
                     start_month = 1, end_month = 12,
                     start_day_of_month = 1, 
                     end_day_of_month = 31){
  
  A1<-paste(start_year, "-", start_month, "-", start_day_of_month, sep = "")
  A2<-paste(end_year, "-", end_month, "-", end_day_of_month, sep = "")
  time.s=as.POSIXct(A1,tz='UTC')
  time.e = as.POSIXct(A2,tz='UTC')
  seq(time.s, time.e, by='24 hours')
}

ee <- timeline(2011,2017)
ee_month <- month(ee)

ZIKA <- function(values, start_month = 6, end_month = 9){
  
 # values <- gdg2[1,]
  DDU <- as.numeric(values)
  
  DDU2 <- data.frame(year = year(ee),
                     month = month(ee),
                     DDU = as.numeric(values))
  
  DDU3 <- DDU2[(DDU2$month > start_month & DDU2$month < end_month),]
  
  einsd <- ddply(DDU3, "year",
                 summarize, 
                 sf = mean(DDU, na.rm = T))
  
  # output
  einsd$sf
}


zika_risk <- apply(gdg2, 1, function(x) ZIKA(x))

zika_risk_mean <- apply(zika_risk, 1, 
                        function(x) x)

zika_mean_map <- stack(lapply(1:7, function(x) setValues(el[[1]], 
                           zika_risk_mean[,x])))
writeRaster(zika_mean_map,
            filename="G:/NeuAll/1-research_projects/r5-infection_experiment_wnv/output/mean_temp_14dpi_18C.grd", bandorder='BIL', overwrite=TRUE)
