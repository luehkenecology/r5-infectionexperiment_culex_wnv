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

kosovo <- raster::getData("GADM", country = "Kosovo", level = 2)
bih <- raster::getData("GADM", country = "BIH", level = 3)

pdf(file = "dddd3d3dds1.pdf",width = 15, height=12)
plot(df1)
plot(ks, add = T)
plot(bih, add = T)
dev.off()

# read data provided by ECDC---------- ---------- ---------- ---------- ---------- 
invasive_species_distribution <- read.csv("reviewd.csv", sep = ";")
invasive_species_distribution$year <- do.call(rbind, strsplit(as.character(invasive_species_distribution$First.case.reported), "/"))[,3]
invasive_species_distribution2 <- invasive_species_distribution[!(is.na(invasive_species_distribution[,4])),]

positive_nuts <- lapply(1:length(invasive_species_distribution2$ID), function(x) subset(nuts_all, 
                                                                                        (NUTS_ID %in% c((as.character(invasive_species_distribution2$ID[x]))))))

positive_nuts2 <- subset(nuts_all, (NUTS_ID %in% c((as.character(invasive_species_distribution2$ID)))))
cbind(invasive_species_distribution2$year,
      invasive_species_distribution2$ID)

rbPal <- colorRampPalette(c("yellow","red","purple"))

european_countries2

sdsdsd <- stack("output/mean_temp_14dpi_18C")
cropping_info <- c(-8.99451, 40.22603, 34.56369, 60)
sdsdsd2sd <- mask(mean(sdsdsd2sd, na.rm = T), country_shapes_nuts0_all2)
cropping_info <- c(-13, 30, 34.56369, 60)
sdsdsd2sd1 <- crop(mean(sdsdsd, na.rm = T), cropping_info)

jet_pal
library(scales)
plot(is.na(sdsdsd2sd1))
pdf(file = "map22ss2s42d.pdf",width = 5, height=4)
plot(sdsdsd2sd1, breaks = c(5, 10, 15, 20, 25, 30), col = c("green",rbPal(4)))
plot(country_shapes_nuts0_all, border = "gray", add = T, col = NA)
plot(positive_nuts2, add = T)
dev.off()


all <- lapply(1:7, function(y) lapply(1:2017, function(x) mean(unlist(extract(sdsdsd[[y]], nuts_all[x,])), na.rm = T)))
all2 <- data.frame(weight = unlist(all))
pdf(file = "n_2.pdf",width = 5, height=4)
ggplot(all2, aes(x=weight)) + 
  geom_histogram(binwidth=0.3, fill = "gray", color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 900)) +
  #scale_x_continuous(limits = c(17.5, 28.5))+
  xlab("Temperature (°C)") +
  ylab("Count") +
  theme_bw()
dev.off()


#subset(nuts_all, 
#       (NUTS_ID %in% c((as.character(invasive_species_distribution2$ID)))))

years <- 2011:2017
mm <- lapply(1:7, function(g) lapply(1:390, function(x) mean(unlist(extract(sdsdsd[[g]], positive_nuts[[x]])), na.rm = T)))
mm <- lapply(1:7, function(x) mm[[x]][invasive_species_distribution2$year == years[x]])
mm2 <- lapply(1:7, function(x) invasive_species_distribution2[invasive_species_distribution2$year == years[x],]$Total)


df <- data.frame(weight = rep(unlist(mm), unlist(mm2)))
library(ggplot2)
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
unique(fddddd$groupp)
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
pdf(file = "EINS432111111fdq211.pdf",width = 7, height=8.5)
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

pdf(file = "all1.pdf",width = 7, height=4.25)
all
dev.off()

