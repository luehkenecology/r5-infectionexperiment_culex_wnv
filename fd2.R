# set working directory ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)
rm(RPROJ)
setwd(PROJHOME)

# load libraries----------------------------------------
library(maptools)
library(raster)

# read shapefiles of European countries----------------------------------------
# all countries in Europe without irland
european_countries <- c("BEL", "BGR", "DNK", "DEU", "EST", "FIN",
                        "FRA", "GRC", "ITA", "HRV", "LTU",
                        "LUX", "MLT", "NLD", "AUT", "POL", "PRT",
                        "ROU", "SWE", "ESP", "SVK", "SVN", "CZE",
                        "HUN", "GBR", "ALB", "BIH", "NOR", "VAT",
                        "LVA","LIE","CHE","MNE","MKD","ALB","SRB", "Kosovo")

country_shapes_nuts0_list <- lapply(1:37, function(x) raster::getData("GADM", country = as.character(european_countries[x]), level = 3))
country_shapes_nuts0_all <- do.call(rbind, country_shapes_nuts0_list)



# read nuts shapefile----------------------------------------
nuts_all <- readShapeSpatial("data/NUTS_2016_01M_SH/NUTS_RG_01M_2016_4326") # read NUTS shape file
(subset(nuts_all, CNTR_CODE == "AL014"))$NUTS_ID
(subset(nuts_all, CNTR_CODE == "AL014"))$NUTS_ID
(subset(nuts_all, CNTR_CODE == "AL"))$NUTS_NAME
(subset(nuts_all, CNTR_CODE == "AL"))$NUTS_ID

(subset(nuts_all, CNTR_CODE == "HU"))$NUTS_ID

plot(subset(nuts_all, NUTS_ID == "EL30"))
(subset(nuts_all, NUTS_NAME == "Pest"))


#nuts_all_3 <- subset(nuts_all, STAT_LEVL_ == 3) # only level 0
#nuts_alldd <- subset()
#plot(nuts_alldd)
#nuts_0 <- subset(nuts_all, STAT_LEVL_ == 0) # only level 0
#crs(nuts_0) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # define projection 
crs(nuts_all) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # define projection 
#crs(nuts_all_3) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # define projection 

# mosquito distribution----------------------------------------
# read data provided by ECDC
invasive_species_distribution <- read.csv("reviewd.csv", sep = ";")
invasive_species_distribution$year <- do.call(rbind, strsplit(as.character(invasive_species_distribution$First.case.reported), "/"))[,3]
invasive_species_distribution2 <- invasive_species_distribution[!(is.na(invasive_species_distribution[,4])),]

# only established Aedes albopictus populations
#invasive_species_distribution_sub <- subset(invasive_species_distribution, vectorspecies == "Aedes_albopictus" & statusDescription == "Established")
#unique(invasive_species_distribution_sub$country)
#nuts_all$NUTS_ID
# select nuts-shapes
#positive_nuts <- subset(nuts_all, 
#                        (NUTS_ID %in% c(unique(as.character(invasive_species_distribution_sub$geoID)), "DE12", "DE13")))
positive_nuts <- lapply(1:length(invasive_species_distribution2$ID), function(x) subset(nuts_all, 
                        (NUTS_ID %in% c((as.character(invasive_species_distribution2$ID[x]))))))
cbind(invasive_species_distribution2$year,
      invasive_species_distribution2$ID)

sdsdsd <- stack("output/mean_temp_14dpi_18C")
sdsdsd[[1]]

subset(nuts_all, 
       (NUTS_ID %in% c((as.character(invasive_species_distribution2$ID)))))

years <- 2011:2017
mm <- lapply(1:7, function(g) lapply(1:379, function(x) mean(unlist(extract(sdsdsd[[g]], positive_nuts[[x]])), na.rm = T)))
mm <- lapply(1:7, function(x) mm[[x]][invasive_species_distribution2$year == years[x]])
mm2 <- lapply(1:7, function(x) invasive_species_distribution2[invasive_species_distribution2$year == years[x],]$Total)

df <- data.frame(weight = rep(unlist(mm), unlist(mm2)))
library(ggplot2)
ggplot(df, aes(x=weight)) + 
  geom_histogram(binwidth=0.3, fill = "gray", color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 420)) +
  scale_x_continuous(limits = c(17.5, 28.5)) +
  xlab("Temperature (°C)") +
  ylab("Number of human WNV cases") +
  theme_bw()

hist(rep(unlist(mm), unlist(mm2)))

mean(rep(unlist(mm), unlist(mm2)), na.rm = T)

setdiff(as.character(invasive_species_distribution2$ID), nuts_all$NUTS_ID)



#SERBIA
dfdf <- c("Branicevski",
                            "Grad Beograd",
                            "Grad Beograd",
                            "Grad Beograd",
                            "Grad Beograd",
                            "Grad Beograd",
                            "Grad Beograd",
                            "Jablanicki",
                            "Juzno-Backi",
                            "Juzno-Backi",
                            "Juzno-Backi",
                            "Juzno-Backi",
                            "Juzno-Backi",
                            "Juzno-Banatski",
                            "Juzno-Banatski",
                            "Juzno-Banatski",
                            "Juzno-Banatski",
                            "Juzno-Banatski",
                            "Juzno-Banatski",
                            "Kolubarski",
                            "Kolubarski",
                            "Kolubarski",
                            "Macvanski",
                            "Macvanski",
                            "Macvanski",
                            "Macvanski",
                            NA,
                            "Nisavski",
                            "Pcinjski",
                            "Podunavski",
                            "Podunavski",
                            "Podunavski",
                            "Podunavski",
                            "Pomoravski",
                            "Rasinski",
                            "Raski",
                            "Severno-Backi",
                            "Severno-Backi",
                            "Severno-Banatski",
                            "Severno-Banatski",
                            "Severno-Banatski",
                            "Sremski",
                            "Sremski",
                            "Sremski",
                            "Sremski",
                            "Sremski",
                            "Sumadijski",
                            "Sumadijski",
                            "Sumadijski",
                            "Zapadno-Backi",
                            "Zapadno-Backi",
                            "Zlatiborski",
                            "Zlatiborski")
srb1 <- subset(invasive_species_distribution, Country == "Serbia")
year <- srb1$year
srb_shp <- raster::getData("GADM", country = "SRB", level = 2)
sort(srb_shp$NAME_2)
which(srb_shp$NAME_1 == "Grad Beograd")
subset(srb_shp, NAME_1 == "Zapadno-Backi")
srb2 <- lapply(1:length(dfdf), function(x) subset(srb_shp, NAME_1 == as.character(dfdf[x]))) 
mm <- lapply(1:7, function(g) lapply(1:53, function(x) mean(unlist(extract(sdsdsd[[g]], srb2[[x]])), na.rm = T)))
mm <- lapply(1:7, function(x) mm[[x]][invasive_species_distribution2$year == years[x]])
mm2 <- lapply(1:7, function(x) invasive_species_distribution2[invasive_species_distribution2$year == years[x],]$Total)


lapply( function(x))
subset(srb2, NAME_1 %in% as.character(dfdf[,1]))

mm <- lapply(1:7, function(g) lapply(1:327, function(x) mean(unlist(extract(sdsdsd[[g]], positive_nuts[[x]])), na.rm = T)))


# cosovo
xko1 <- subset(invasive_species_distribution, Country == "Kosovo*")
xko2 <- raster::getData("GADM", country = "XKO", level = 2)
sort(SRB$NAME_1)

#SLOVENIA
SVN <- raster::getData("GADM", country = "SVN", level = 2)
sort(SVN$NAME_1)

#MONTENEGRO
MNE <- raster::getData("GADM", country = "MNE", level = 1)
sort(MNE$NAME_1)

#ALBANIA
alb <- raster::getData("GADM", country = "ALB", level = 3)
plot(alb[which(alb$NAME_1 == "Lezhë"),])

alb <- raster::getData("GADM", country = "GR", level = 3)
plot(alb)


#Bosnia and Herzegovina
bih <- raster::getData("GADM", country = "BIH", level = 3)
plot(dza[which(bih$NAME_2 %in% c("Modrica", "Tuzla")),])
plot(dza[which(bih$NAME_1 %in% c("Repuplika Srpska")),])


