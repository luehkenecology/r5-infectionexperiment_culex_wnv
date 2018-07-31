# set working directory ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)
rm(RPROJ)
setwd(PROJHOME)

# load libraries--------------------------------------------------------------------------------
library(ggplot2)

# read infection experiments from the literature--------------------------------------------------------------------------------
raw_data <- read.table(file = "Mappe.csv", sep=";", header = T)

# exclue dpi = 0 = controls--------------------------------------------------------------------------------
raw_data_sub1 <- subset(raw_data, dpi > 0)

# only Culex species--------------------------------------------------------------------------------
raw_data_sub2 <- subset(raw_data_sub1, mosquito.species %in% c("Culex pipiens molestus",
                                                               "Culex pipiens Hybrid (pipiens x molestus)",
                                                               "Culex pipiens pipiens",
                                                               "Culex pipiens s.l."))

# read infection experiments from our study--------------------------------------------------------------------------------
raw_data2 <- read.table(file = "data/data_transfer.csv", sep=";", header = T)

ffff <- data.frame(Species = c(as.character(raw_data_sub2$mosquito.species), as.character(raw_data2$species)),
                   tr = c(raw_data_sub2$transmission.rate, raw_data2$TR),
                   incubation_temp = c(raw_data_sub2$incubation.temperature, raw_data2$temperature),
                   dpi = c(raw_data_sub2$dpi, rep(c(14, 21), 11)),
                   study = c(rep("literature", length(raw_data_sub2$dpi)),
                             rep("this study", length(rep(c(14, 21), 11)))))

ttt <- subset(ffff, study == "this study")

p1a <- ggplot() +
  geom_line(data = ttt, aes(incubation_temp, tr, group = interaction(Species, dpi)), colour = "red", linetype = "dashed") +
  geom_point(data = ffff, aes(incubation_temp, tr, 
                              group = Species, 
                              shape = Species,
                              colour = as.factor(dpi),
                              fill = as.factor(study)),
             stroke=1.2, position=position_jitter(w=0.25),
             size = 4) +
  #scale_color_disc("dpi (border colour)", values = cc) +
  scale_color_discrete("dpi (border colour)") +
  scale_fill_manual("Study (fill colour)",values = c("gray", "red")) +
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  guides(shape = guide_legend(order = 1),
         colour = guide_legend(order = 2),
         fill = guide_legend(override.aes=list(colour = c("gray","red")
                                               ,shape=c(16,16))), order = 3)+
  xlab("Temperature (°C)") +
  ylab("Transmission rate (%)") +
  scale_x_continuous(limits = c(17.5, 28.5)) +
  theme_bw()+
  theme(legend.justification = c(0,0.5))+
  theme(text = element_text(size = 15))

pdf(file = "BICx.pdf",width = 7, height=5)
plot(p1a)
dev.off()

p1b <- ggplot() +
  geom_line(data = ttt, aes(incubation_temp, tr, group = interaction(Species, dpi)), colour = "red", linetype = "dashed") +
  geom_point(data = ffff, aes(incubation_temp, tr, 
                              group = Species, 
                              shape = Species,
                              colour = as.factor(dpi),
                              fill = as.factor(study)),
             stroke=1.2, position=position_jitter(w=0.25),
             size = 4) +
  #scale_color_disc("dpi (border colour)", values = cc) +
  scale_color_discrete("dpi (border colour)") +
  scale_fill_manual("Study (fill colour)",values = c("gray", "red")) +
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  guides(shape = guide_legend(order = 1),
         colour = guide_legend(order = 2),
         fill = guide_legend(override.aes=list(colour = c("gray","red")
                                               ,shape=c(16,16))), order = 3)+
  xlab("Temperature (°C)") +
  ylab("Transmission rate (%)") +
  scale_x_continuous(limits = c(17.5, 28.5)) +
  theme_bw()+
  theme(legend.position="none")


# load libraries----------------------------------------
library(maptools)
library(raster)

# read shapefiles of European countries----------------------------------------
# all countries in Europe without irland
european_countries <- c("BEL", "BGR", "DNK", "DEU", "EST", "FIN","IRL",
                        "FRA", "GRC", "ITA", "HRV", "LTU","UKR", "PL",
                        "LUX", "MLT", "NLD", "AUT", "POL", "PRT",
                        "ROU", "SWE", "ESP", "SVK", "SVN", "CZE",
                        "HUN", "GBR", "ALB", "BIH", "NOR", "VAT",
                        "LVA","LIE","CHE","MNE","MKD","ALB","SRB", "Kosovo")

country_shapes_nuts0_list <- lapply(1:37, function(x) raster::getData("GADM", country = as.character(european_countries[x]), level = 0))
country_shapes_nuts0_all <- do.call(rbind, country_shapes_nuts0_list)


european_countries2 <- c("BEL", "BGR", "DNK", "DEU", "EST", "FIN","IRL",
                        "FRA", "GRC", "ITA", "HRV", "LTU",
                        "LUX", "MLT", "NLD", "AUT", "POL", "PRT",
                        "ROU", "SWE", "ESP", "SVK", "SVN", "CZE",
                        "HUN", "GBR", "ALB", "BIH", "NOR", "VAT",
                        "LVA","LIE","CHE","MNE","MKD","ALB","SRB", "Kosovo")
country_shapes_nuts0_list2 <- lapply(1:37, function(x) raster::getData("GADM", country = as.character(european_countries2[x]), level = 0))
country_shapes_nuts0_all2 <- do.call(rbind, country_shapes_nuts0_list2)

# read nuts shapefile---------- ---------- ---------- ---------- ---------- 
nuts_all <- readShapeSpatial("data/NUTS_2016_01M_SH/NUTS_RG_01M_2016_4326") # read NUTS shape file
crs(nuts_all) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # define projection 

# exclude Turkey and Iceland---------- ---------- ---------- ---------- ---------- 
nuts_all <- subset(nuts_all, !(CNTR_CODE %in% c("TR", "IS")))

# select all regions on the NUTS3 level---------- ---------- ---------- ---------- ---------- 
df <- subset(nuts_all, LEVL_CODE == 3)

# exclude oversea regions (e.g. Guadeloupe)
df1 <- subset(df, !(NUTS_ID %in% c("PT200", "PT300", "ES703",
                                   "ES704", "ES705", "ES706",
                                   "ES707", "ES708", "ES709",
                                   "FRY10", "FRY20", "FRY30",
                                   "FRY40", "FRY50")))
kosovo <- raster::getData("GADM", country = "Kosovo", level = 1)
bih <- raster::getData("GADM", country = "BIH", level = 2)
bih2013 <- subset(bih, NAME_2 %in% c("Modrica", "Tuzla"))
bih2014 <- subset(bih, NAME_1 %in% c("Repuplika Srpska"))
kosovo2012 <- subset(kosovo, NAME_1 %in% c("Pristina","Gnjilane", "Prizren"))
greece1014 <- subset(df1, FID %in% c("EL301","EL302",
                            "EL303","EL304",
                            "EL305","EL306",
                            "EL307"))


pdf(file = "figs/supplementary_map_eu_regions.pdf",width = 15, height=12)
plot(df1)
plot(kosovo, add = T)
plot(bih, add = T)
dev.off()

# read data provided by ECDC---------- ---------- ---------- ---------- ---------- 
invasive_species_distribution <- read.csv("reviewd.csv", sep = ";")
invasive_species_distribution <- invasive_species_distribution[-c(42, 43, 44, 45),]
sdsdsd <- stack("output/mean_temp_14dpi_18C")

invasive_species_distribution$year <- do.call(rbind, strsplit(as.character(invasive_species_distribution$First.case.reported), "/"))[,3]
invasive_species_distribution2 <- invasive_species_distribution[!(is.na(invasive_species_distribution[,4])),]
invasive_species_distribution2 <- subset(invasive_species_distribution2, !(Country %in% c("Turkey")))
invasive_species_distribution2[39,]
# select eu regions
positive_nuts <- lapply(1:length(invasive_species_distribution2$ID), function(x) subset(df1, 
                                                                                        (FID %in% c((as.character(invasive_species_distribution2$ID[x]))))))
positive_nuts[39]
positive_nuts2 <- subset(df1, (NUTS_ID %in% c((as.character(invasive_species_distribution2$ID)))))


rbPal <- colorRampPalette(c("yellow","red","purple"))

#cropping_info <- c(-8.99451, 40.22603, 34.56369, 60)
#sdsdsd2sd <- mask(mean(sdsdsd2sd, na.rm = T), country_shapes_nuts0_all2)
#cropping_info <- c(-11, 30, 35.8, 60)
#sdsdsd2sd1 <- crop(mean(sdsdsd, na.rm = T), cropping_info)
sdsdsd2sd1 <- mask(mean(sdsdsd, na.rm = T), country_shapes_nuts0_all2)
cropping_info <- c(-11, 30, 35.8, 60)
sdsdsd2sd2 <- crop(sdsdsd2sd1, cropping_info)
#sdsdsd2sd1 <- crop(sdsdsd2sd1, positive_nuts2)
kosovo_plus <- crop(mean(sdsdsd, na.rm = T), kosovo)

sdsdsd2sd2[is.na((sdsdsd2sd2))] <- max(getValues(sdsdsd2sd2), na.rm = T)+30
sdsdsd2sd3 <- mask(sdsdsd2sd2, country_shapes_nuts0_all2)
plot(subset(positive_nuts2, CNTR_CODE == "EL")[5,], add = T)

plot(subset(positive_nuts2, CNTR_CODE == "TR"))
positive_nuts2_plot <- subset(positive_nuts2, !(CNTR_CODE %in% "CY"))
pdf(file = "figs/map13.pdf",width = 4.5, height=3.9)
par(mar=c(5, 2, 4, 6) + 0.1)
plot(sdsdsd2sd3, breaks = c(5, 10, 15, 20, 25, 36, 60), col = c("green",rbPal(4), "gray"), legend = F)
plot(kosovo_plus, breaks = c(5, 10, 15, 20, 25, 36, 60), col = c("green",rbPal(4), "gray"), add = T, legend = F)
plot(country_shapes_nuts0_all2, border = "gray", add = T, col = NA)
plot(positive_nuts2_plot, add = T)
plot(bih2013, add = T)
plot(bih2014, add = T)
plot(kosovo2012, add = T)
plot(greece1014, add = T)
legend("right",inset=c(-0.7,0), legend = c("NA", "0-5",
                          ">5-10",
                          ">10-15",
                          ">20-25",
                          ">25","","","","WNV circulation"),horiz = F,  xpd = TRUE, 
       bty = "n",cex = 1, bg = "white", 
       border = c("black","black","black","black","black","black","white","white","white","black"),
       fill = c("gray","green",rbPal(4), "white", "white","white","white"))
dev.off()


all_all <- lapply(1:7, function(y) lapply(1:1425, function(x) mean(unlist(extract(sdsdsd[[y]], df1[x,])), na.rm = T)))
all_bih <- lapply(1:7, function(y) lapply(1:15, function(x) mean(unlist(extract(sdsdsd[[y]], bih[x,])), na.rm = T)))
all_kosovo <- lapply(1:7, function(y) lapply(1:7, function(x) mean(unlist(extract(sdsdsd[[y]], kosovo[x,])), na.rm = T)))

all2 <- data.frame(weight = c(unlist(all_all),
                              unlist(all_bih),
                              unlist(all_kosovo)))

years <- 2011:2017
invasive_species_distribution2[39,]
mm <- lapply(1:7, function(g) lapply(1:381, function(x) mean(unlist(extract(sdsdsd[[g]], positive_nuts[[x]])), na.rm = T)))
#mm <- lapply(1:7, function(x) mm[[x]][invasive_species_distribution2$year == years[x]])
mm2 <- lapply(1:7, function(x) invasive_species_distribution2[invasive_species_distribution2$year == years[x],]$Total)
df <- data.frame(weight = rep(unlist(mm), unlist(mm2)))

p2 <- ggplot(df, aes(x=weight)) + 
  geom_histogram(binwidth=0.3, fill = "gray", color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 420)) +
  scale_x_continuous(limits = c(17.5, 28.5)) +
  xlab("Temperature (°C)") +
  ylab("Number of human WNV cases") +
  theme_bw()

AA <- all2
AA$groupp = "European regions"
BB <- df
BB$groupp = "WNV cases"
fddddd <- rbind(AA, BB)

pdd3 <- ggplot(fddddd, aes(x= weight, group = as.factor(groupp), 
                           alpha = groupp, fill = as.factor(groupp))) + 
  geom_histogram(binwidth=0.3, position="identity", color = "black")+
  scale_alpha_discrete(range = c(1, 0.5)) +
  scale_fill_manual(values = c("gray", "red"),name= "")+  #geom_histogram(data = all2, aes(x=weight), binwidth=0.3, fill = "gray", color = "black") +
  #geom_histogram(data = df, aes(x=weight),alpha = 0.3,binwidth=0.3, fill = "red", color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 900)) +
  scale_x_continuous(limits = c(17.5, 28.5))+
  xlab("Temperature (°C)") +
  ylab("Count") +
  theme_bw()+
  theme(legend.justification = c(0,0.5))+
  guides(alpha=FALSE)+
  theme(text = element_text(size = 15))
library(cowplot)
pdf(file = "figs/EINS432111111fdq211.pdf",width = 7, height=8.5)
ggdraw(plot_grid(plot_grid(p1a, pdd3, ncol=1, align='v')))
dev.off()

all <- ggplot(fddddd, aes(x= weight, group = as.factor(groupp), 
                           alpha = groupp, fill = as.factor(groupp))) + 
  geom_histogram(binwidth=0.3, position="identity", color = "black")+
  scale_alpha_discrete(range = c(1, 0.5)) +
  scale_fill_manual(values = c("gray", "red"),name= "")+  #geom_histogram(data = all2, aes(x=weight), binwidth=0.3, fill = "gray", color = "black") +
  #geom_histogram(data = df, aes(x=weight),alpha = 0.3,binwidth=0.3, fill = "red", color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 900)) +
  xlab("Temperature (°C)") +
  ylab("Count") +
  theme_bw()+
  theme(legend.justification = c(0,0.5))+
  guides(alpha=FALSE)+
  theme(text = element_text(size = 15))

pdf(file = "figs/all2.pdf",width = 7, height=4.25)
all
dev.off()

