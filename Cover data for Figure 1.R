library(ggplot2)
library(lubridate)
library(plyr)
library(reshape2)
library(gridExtra)
library(cowplot)
library(knitr)
library(markdown)
library(xtable)
library(zoo)
library(devtools)
library(dplyr)
library(tidyr)
library(vegan)
library(codyn)

###Read in files### read in files

#All plant cover/volume data
Ccover <- read.csv ("sev129_nppcorequadrat_20161214.csv", header=TRUE, strip.white = TRUE)
colnames(Ccover)[9] <- "SpeciesCode"

#pull out the creosote site
Ccover<- Ccover[Ccover$site=="C"& Ccover$treatment == "C",]
Ccover<-Ccover[Ccover$height >0 & Ccover$cover>0,]
head(Ccover)

##Calculations  Now we need to calculate the different metrics of plant production. 
##First we'll calculate "mean.volume," the average volumetric space (cm^3) that each species occupies per quadrat per season. 
# Calculate the number of quads per site per season

quads <- ddply(Ccover, c("year", "season"), function(x)
  data.frame(quadnum = length(unique(paste(x$web, x$plot, x$quad)))))

# Merge quadnum column
Ccover <- unique(merge(Ccover, quads, c("year", "season"), all=T))
head(Ccover)

# Calculate all the average volumes /species /quad in the harvest dataset
Ccover$volume <- Ccover$cover * 100 * Ccover$height
sumvolumes <- ddply(unique(Ccover[,c("year", "season", "web", "plot", "quad", "quadnum", "SpeciesCode", "volume", "cover")]), 
                    c("year", "season", "web", "plot", "quad", "quadnum", "SpeciesCode"), function(x)
  data.frame(quad.volume = sum(x$volume, na.rm=T),
             quad.cover = sum(x$cover, na.rm=T)))
head(sumvolumes)

# Now calculate the average volume across all quads /species
meanvolumes <- unique(ddply(unique(sumvolumes[,c("year", "season", "SpeciesCode", "quadnum", "quad.volume", "quad.cover")]), 
                     c("year", "season", "SpeciesCode"), function(x)
  data.frame(mean.volume = sum(x$quad.volume, na.rm=T)/x$quadnum,
             mean.cover = sum(x$quad.cover, na.rm=T)/x$quadnum)))

meanvolumes$season <- as.factor(meanvolumes$season)
head(meanvolumes)


# Revalue season column
meanvolumes$season <- revalue(meanvolumes$season, c(
  "1" = "winter",
  "2" = "spring",
  "3" = "fall"))
head(meanvolumes)

#exclude winter
meanvolumes <- meanvolumes[meanvolumes$season != "winter",]
head(meanvolumes)

# Taxonomic, PFT information for all species
sev.species <- read.csv("sevilleta_species_list.csv", header=TRUE, strip.white=TRUE)
colnames(sev.species)[1] <- "SpeciesCode"
head(sev.species)


# Merge npp and species data, dates first
creosote.core.volume <- merge(meanvolumes, sev.species, "SpeciesCode", all.x=T)
head(creosote.core.volume)

# Make a column of average-season dates which will help make prettier figures
creosote.core.volume$fakedate <- NA
creosote.core.volume$fakedate[creosote.core.volume$season=="spring"] <- "4/1"
creosote.core.volume$fakedate[creosote.core.volume$season=="fall"] <- "10/1"
creosote.core.volume$fakedate <- as.Date(paste(creosote.core.volume$fakedate, creosote.core.volume$year), format="%m/%d %Y") 

head(creosote.core.volume)


####Plant functional types Since we don't actually care about species-specific numbers, we can condense all of these data into plant functional groups. I have code for traditional groups (creosote, non-creosote C3 plants, C4, and CAM), but am masking it. Including plots for our new PFTs
### Create our own PFT's
##### We should come up with new PFT's!
# Possible categories: LATR, "woody" perennial forbs, annual juicy forbs, perennial juicy forbs, annual grasses, perennial grasses, OR spring-blooming grasses, monsoon-blooming grasses
creosote.core.volume$PFT <- as.character(NA)
creosote.core.volume$PFT[creosote.core.volume$g_f == "g"] <- "C4: grass"
creosote.core.volume$PFT[creosote.core.volume$g_f == "g" & creosote.core.volume$path == "C3"] <- "C3: grass"
creosote.core.volume$PFT[creosote.core.volume$g_f == "f"] <- "C3: forb"
creosote.core.volume$PFT[creosote.core.volume$g_f == "f" & creosote.core.volume$path == "C4"] <- "C4: forb"
creosote.core.volume$PFT[creosote.core.volume$g_f == "s" & creosote.core.volume$path == "C3"] <- "C3: shrub/subshrub"
creosote.core.volume$PFT[creosote.core.volume$path == "CAM"] <- "CAM"
creosote.core.volume$PFT[creosote.core.volume$SpeciesCode == "PHACE"] <- "C3: forb"
creosote.core.volume$PFT[creosote.core.volume$SpeciesCode == "ASMIM"] <- "C3: forb"
creosote.core.volume$PFT[creosote.core.volume$SpeciesCode == "SCLA6"] <- "C3: forb"
creosote.core.volume$PFT[creosote.core.volume$SpeciesCode == "GUWR"] <- "C3: forb"
creosote.core.volume$PFT[creosote.core.volume$SpeciesCode == "STEM"] <- "C3: shrub/subshrub"
creosote.core.volume$PFT <- as.factor(creosote.core.volume$PFT)
creosote.core.volume[is.na(creosote.core.volume$PFT),]
# Fix photo_path gaps and remove unknowns
unique(creosote.core.volume$SpeciesCode[is.na(creosote.core.volume$SpeciesCode)])
creosote.core.volume$SpeciesCode[!is.na(creosote.core.volume$SpeciesCode) &
                                 creosote.core.volume$SpeciesCode == "NONE"] 
head(creosote.core.volume)

# Summarize plant biomass by Plant functional type
creosote.core.volume <- unique(creosote.core.volume)
creosote.core.volume$fakedate <- as.factor(as.character(creosote.core.volume$fakedate))
PFTseasonsummary <- ddply(unique(creosote.core.volume[,c("year", "season", "fakedate", "PFT", "mean.cover", "mean.volume")]), 
                         c("year", "season", "fakedate", "PFT"), function(x)
  data.frame(mean.coverPFT = sum(x$mean.cover, na.rm=TRUE), 
             mean.volumePFT = sum(x$mean.volume, na.rm=TRUE)))

# Summary of C3 grass, C4 shrub/subshrub, and CAM contribution
PFTseasonsummary <- PFTseasonsummary[!is.na(PFTseasonsummary$PFT),]
kable(PFTseasonsummary[PFTseasonsummary$PFT=="C3: grass" | 
                         PFTseasonsummary$PFT=="C4: shrub/subshrub" | 
                         PFTseasonsummary$PFT=="CAM",
                 c("fakedate", "year", "PFT", "mean.coverPFT", "mean.volumePFT")],
      format="pandoc")

# Fix dates'
PFTseasonsummary$fakedate <- as.Date(PFTseasonsummary$fakedate, format="%Y-%m-%d")

##Plots Now let's make some plots!  
##*by our new PFTs**
ccoverPFT.plot <- ggplot(PFTseasonsummary, aes(x = fakedate, y = mean.coverPFT, group = PFT, colour=PFT, fill=PFT)) +
  theme_bw() + 
  geom_col(width=150) +
  theme(axis.title.x=element_blank(), 
        legend.key.size=unit(0.8, "lines"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.title=element_text(size=11),
        axis.title.y=element_text(size=11)) +
 scale_x_date(date_labels = "%y", date_breaks = "1 year",
               limits = c(as.Date("1999-01-01"), as.Date("2016-10-01"))) +
  xlab("Time") + ylab(expression(atop("Cover", 
                                      "("*cm^2*"/"*m^2*")")))

ccoverPFT.plot


#####Notes:  Creosote (LATR) biomass has been calculated in several different ways throughout time. 
###No method seems entirely consistent, hence the variation in all creosote biomass metrics. 
##I don't know how to correct for these changes. We might want to exclude some groups.

# Met data
met.data <- read.csv("Met_all.csv", header=TRUE, strip.white=TRUE)
# only use station #49 data
met.data <- met.data[met.data$Sta==49,]
# only keep data and precip data
head(met.data)
met.data <- unique(met.data[,c(2, 3, 4, 5, 7, 11)])
head(met.data)
met.data$season <- as.factor(met.data$Month)
met.data$season <- revalue(met.data$season, c("1"="1","2"="1","3"= "1","4"="1","5"="1","6"="1",
                                              "7"="2","8"="2","9"="2","10"="2","11"="1" ,"12"="1"))
colnames(met.data) <- c("mean.date", "month", "day","year", "Avg_Temp", "Precip", "season")
met.data[met.data==-999] <- NA
head(met.data)


# Make a column of average-season dates which will help make prettier figures
# Format date column 
met.data$mean.date <- as.Date(met.data$mean.date, format="%m/%d/%Y")

#making fake date to wrap years
unique(month(met.data$mean.date[met.data$season==1])) 
met.data$fakedate <- NA
met.data$fakedate[met.data$season=="1"] <- "4/1"
met.data$fakedate[met.data$season=="2"] <- "10/1"
met.data$fakedate <- as.Date(paste(met.data$fakedate, met.data$year), format="%m/%d %Y")
met.data$fakedate[met.data$month==11 | met.data$month==12] <- 
met.data$fakedate[met.data$month==11 | met.data$month==12] + years(1)
unique(month(met.data$fakedate[month(met.data$mean.date)==11])) #should be 4/April
unique(met.data$fakedate[month(met.data$mean.date)==11 &
                           year(met.data$mean.date)==2000]) #should be April 2001


# Calculate monthly precip
monthlysums <- unique(ddply(met.data, c("year", "month"), function(x)
  data.frame(month.precip = sum(x$Precip, na.rm=TRUE),
             month.temp = mean(x$Avg_Temp, na.rm=TRUE))))
head(monthlysums)
monthlysums$temp.date <- as.Date(paste(1, monthlysums$month, monthlysums$year), 
                                 format="%d %m %Y")
#Calculate seasonal precip
seasonsums <- unique(ddply(met.data, c("fakedate", "season"), function(x)
  data.frame(season.precip = sum(x$Precip, na.rm=TRUE),
             season.temp = mean(x$Avg_Temp, na.rm=TRUE))))
head(seasonsums)

  ###may use this to change from 1 and 2 to winter and monsoon
seasonsums$season <- revalue(seasonsums$season, c("1" = "winter", "2" = "monsoon")) 
seasonprecip.plot <- ggplot(seasonsums, aes(x = fakedate, y = season.precip, colour=season, fill=season)) +
    geom_col(width=100) + theme_bw() +
  theme(axis.title.x=element_blank(), 
        legend.key.size=unit(0.8, "lines"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.title=element_text(size=11),
        axis.title.y=element_text(size=11)) +
  scale_x_date(date_labels = "%y", date_breaks = "1 year",
               limits = c(as.Date("1999-01-01"), as.Date("2016-10-01"))) +
  xlab("Time") + ylab(expression(atop("Precip","(mm)")))
seasonprecip.plot

### Try to summarize the trapping data, try is the operative word

sev.trap <- read.csv("sev008_rodentpopns_20161027.csv", header=TRUE, strip.white=TRUE)
sev.trap <- sev.trap[sev.trap$location=="5pgrass" | sev.trap$location=="5plarrea",]

# Calculate the number of nights and webs trapped per season
numnight <- ddply(sev.trap, c("year", "location", "season"), function(x)
  data.frame(numnight = length(unique(paste(x$night, x$web)))))
head(numnight)

# Merge numnight column
sev.trap <- unique(merge(sev.trap, numnight, c("year", "location", "season"), all=T))

###calculate the number of trap nights
sev.trap$trapnight <- sev.trap$numnight * 148
head(sev.trap)

##select the creosote site
sev.trap.creo<- sev.trap[sev.trap$location=="5plarrea",]
sev.trap.creo <- subset(sev.trap.creo, year>"1998") 
head(sev.trap.creo)

###summarising by season
crodent.long <- data.frame(summarise(group_by(sev.trap.creo, year, location, season, species, numnight), abundance = length(species)))

##calculate mice per webs set
crodent.long$miceperwebs <- crodent.long$abundance/crodent.long$numnight
crodent.long$season <- as.factor(crodent.long$season)
  
#rename seasons to create two bins
crodent.long$season <- revalue(crodent.long$season, c(
  "1" = "spring", 
  "3" = "fall"))
head(crodent.long)

##create a column of average-season dates which will make it line up with veg and met panels
# Make a column of average-season dates which will help make prettier figures
crodent.long$fakedate <- NA
crodent.long$fakedate[crodent.long$season=="spring"] <- "4/1"
crodent.long$fakedate[crodent.long$season=="fall"] <- "10/1"
crodent.long$fakedate <- as.Date(paste(crodent.long$fakedate, crodent.long$year), format="%m/%d %Y") 

head(crodent.long)

# Combine mice into feeding guilds

crodent.long$guild <- as.character(crodent.long$species)
crodent.long$guild <- revalue(crodent.long$guild, c(
  "chin" = "Other Heteromyid", 
"dime" = "Dipodomys merriami", 
"dior"= "Dipodomys Ordii", 
"dipo" = "Other Heteromyid", 
"disp" = "Dipodomys spectabilis", 
"pgfv" = "Perognathus flavus",
"neal" = "Neotoma sp.", 
"nemi" = "Neotoma sp.",
"onar"= "Onychomys sp.", 
"onle"= "Onychomys sp.", 
"onsp"= "Onychomys sp.", 
"peer"= "Peromyscus sp.", 
"pele"= "Peromyscus sp.", 
"pema"= "Peromyscus sp.", 
"pedi"= "Peromyscus sp.", 
"pmer"= "Peromyscus sp.", 
"pmle"= "Peromyscus sp.", 
"pmtr"= "Peromyscus sp.", 
"pmdi"= "Peromyscus sp.", 
"pmbo"= "Peromyscus sp.",
"remg"= "Reithrodontomys sp.", 
"sihi"="Other Cricetid",
"spsp"= "Other Scurid"))

head(crodent.long)

# Summarize mice by foraging guild
cguildsummary <- ddply(unique(crodent.long[,c("year", "season", "miceperwebs", "fakedate", "guild")]), 
                          c("year", "season", "fakedate", "guild"), function(x)
                            data.frame(sum.guild = sum(x$miceperwebs, na.rm=TRUE)))
head(cguildsummary)
crodent.long <- subset(crodent.long, fakedate>="1999-01-01")
crodent.long$fakedate <- as.Date(crodent.long$fakedate, format="%Y-%m-%d")

csevmouse.plot <- ggplot(crodent.long, aes(x = fakedate, y = miceperwebs, group=guild, colour=guild, fill=guild)) +
  geom_col(width=150) + theme_bw() +
  theme(axis.title.x=element_blank(), 
   legend.key.size=unit(0.8, "lines"),
   legend.background = element_rect(fill = "transparent", colour = "transparent"),    
   legend.title=element_text(size=11),
   axis.title.y=element_text(size=11)) +
  scale_x_date(date_labels = "%y", date_breaks = "1 year",
               limits = c(as.Date("1999-01-01"), as.Date("2016-10-01"))) +
  xlab("Time") + ylab(expression(atop("Small Mammals", 
                                      "(trapped per bout)")))
csevmouse.plot

cfinalthreeplots <- plot_grid(seasonprecip.plot, ccoverPFT.plot, csevmouse.plot,
                             align = 'v', ncol=1, scale=0.98,
                             rel_heights = c(1,1.1,1.2),
                             labels=c('A', 'B', 'C'), hjust=-0.2) 
cfinalthreeplots


##Write out PFTsummary table and final figures
#print(cfinalthreeplots)
#ggsave(file="Desktop/Sevilleta_Paper/Figure1crosote_15May2017.png", width=9, height=8, units="in", dpi=300)

#write.csv(PFTseasonsummary, file="Desktop/Sevilleta_Paper/Fig1.creosotecoverDATA_15May2017.csv", row.names = F)

# Summary of species in each category
#write.csv(creosote.core.site, file="Desktop/Sevilleta_Paper/Fig1.creosotecoverDATAbyspecies_15May2017.csv", row.names = F)

# Summary of precip data
#write.csv(seasonsums, file="Desktop/Sevilleta_Paper/Fig1.metstationDATA_15May2017.csv", row.names = F)
############################
############################
######Grassland
Gcover <- read.csv ("sev129_nppcorequadrat_20161214.csv", header=TRUE, strip.white = TRUE)
colnames(Gcover)[9] <- "SpeciesCode"

#pull out the grassland site
Gcover<- Gcover[Gcover$site=="G"& Gcover$treatment == "C",]
Gcover<-Gcover[Gcover$height >0 & Gcover$cover>0,]
head(Gcover)

##Calculations  Now we need to calculate the different metrics of plant production. 
##First we'll calculate "mean.volume," the average volumetric space (cm^3) that each species occupies per quadrat per season. 
# Calculate the number of quads per site per season

quads <- ddply(Gcover, c("year", "season"), function(x)
  data.frame(quadnum = length(unique(paste(x$web, x$plot, x$quad)))))

# Merge quadnum column
Gcover <- unique(merge(Gcover, quads, c("year", "season"), all=T))
head(Gcover)

# Calculate all the average volumes /species /quad in the harvest dataset
Gcover$volume <- Gcover$cover * 100 * Gcover$height
sumvolumes <- ddply(unique(Gcover[,c("year", "season", "web", "plot", "quad", "quadnum", "SpeciesCode", "volume", "cover")]), 
                    c("year", "season", "web", "plot", "quad", "quadnum", "SpeciesCode"), function(x)
                      data.frame(quad.volume = sum(x$volume, na.rm=T),
                                 quad.cover = sum(x$cover, na.rm=T)))
head(sumvolumes)

# Now calculate the average volume across all quads /species
meanvolumes <- unique(ddply(unique(sumvolumes[,c("year", "season", "SpeciesCode", "quadnum", "quad.volume", "quad.cover")]), 
                            c("year", "season", "SpeciesCode"), function(x)
                              data.frame(mean.volume = sum(x$quad.volume, na.rm=T)/x$quadnum,
                                         mean.cover = sum(x$quad.cover, na.rm=T)/x$quadnum)))

meanvolumes$season <- as.factor(meanvolumes$season)
head(meanvolumes)


# Revalue season column
meanvolumes$season <- revalue(meanvolumes$season, c(
  "1" = "winter",
  "2" = "spring",
  "3" = "fall"))
head(meanvolumes)

#exclude winter
meanvolumes <- meanvolumes[meanvolumes$season != "winter",]
head(meanvolumes)

# Taxonomic, PFT information for all species
sev.species <- read.csv("sevilleta_species_list.csv", header=TRUE, strip.white=TRUE)
colnames(sev.species)[1] <- "SpeciesCode"
head(sev.species)


# Merge npp and species data, dates first
grassland.core.volume <- merge(meanvolumes, sev.species, "SpeciesCode", all.x=T)
head(grassland.core.volume)

# Make a column of average-season dates which will help make prettier figures
grassland.core.volume$fakedate <- NA
grassland.core.volume$fakedate[grassland.core.volume$season=="spring"] <- "4/1"
grassland.core.volume$fakedate[grassland.core.volume$season=="fall"] <- "10/1"
grassland.core.volume$fakedate <- as.Date(paste(grassland.core.volume$fakedate, grassland.core.volume$year), format="%m/%d %Y") 

head(grassland.core.volume)


####Plant functional types Since we don't actually care about species-specific numbers, we can condense all of these data into plant functional groups. I have code for traditional groups (creosote, non-creosote C3 plants, C4, and CAM), but am masking it. Including plots for our new PFTs
### Create our own PFT's
##### We should come up with new PFT's!
# Possible categories: LATR, "woody" perennial forbs, annual juicy forbs, perennial juicy forbs, annual grasses, perennial grasses, OR spring-blooming grasses, monsoon-blooming grasses
grassland.core.volume$PFT <- as.character(NA)
grassland.core.volume$PFT[grassland.core.volume$g_f == "g"] <- "C4: grass"
grassland.core.volume$PFT[grassland.core.volume$g_f == "g" & grassland.core.volume$path == "C3"] <- "C3: grass"
grassland.core.volume$PFT[grassland.core.volume$g_f == "f"] <- "C3: forb"
grassland.core.volume$PFT[grassland.core.volume$g_f == "f" & grassland.core.volume$path == "C4"] <- "C4: forb"
grassland.core.volume$PFT[grassland.core.volume$g_f == "s" & grassland.core.volume$path == "C3"] <- "C3: shrub/subshrub"
grassland.core.volume$PFT[grassland.core.volume$path == "CAM"] <- "CAM"
grassland.core.volume$PFT[grassland.core.volume$SpeciesCode == "ATCA2"] <- "C4: forb"
grassland.core.volume$PFT[grassland.core.volume$SpeciesCode == "ASMIM"] <- "C3: forb"
grassland.core.volume$PFT[grassland.core.volume$SpeciesCode == "ESVIV"] <- "CAM"
grassland.core.volume$PFT[grassland.core.volume$SpeciesCode == "PRGLT"] <- "C3: shrub/subshrub"
grassland.core.volume$PFT[grassland.core.volume$SpeciesCode == "STEM"] <- "C3: shrub/subshrub"
grassland.core.volume$PFT[grassland.core.volume$SpeciesCode == "RANE"] <- "C3: forb"
grassland.core.volume$PFT <- as.factor(grassland.core.volume$PFT)
grassland.core.volume[is.na(grassland.core.volume$PFT),]
# Fix photo_path gaps and remove unknowns
unique(grassland.core.volume$SpeciesCode[is.na(grassland.core.volume$PFT)])
grassland.core.volume$SpeciesCode[!is.na(grassland.core.volume$SpeciesCode) &
                                    grassland.core.volume$SpeciesCode == "NONE"] 
head(grassland.core.volume)

# Summarize plant biomass by Plant functional type
grassland.core.volume <- unique(grassland.core.volume)
grassland.core.volume$fakedate <- as.factor(as.character(grassland.core.volume$fakedate))
gPFTseasonsummary <- ddply(unique(grassland.core.volume[,c("year", "season", "fakedate", "PFT", "mean.cover", "mean.volume")]), 
                          c("year", "season", "fakedate", "PFT"), function(x)
                            data.frame(mean.coverPFT = sum(x$mean.cover, na.rm=TRUE), 
                                       mean.volumePFT = sum(x$mean.volume, na.rm=TRUE)))

# Summary of C3 grass, C4 shrub/subshrub, and CAM contribution
gPFTseasonsummary <- gPFTseasonsummary[!is.na(gPFTseasonsummary$PFT),]
kable(gPFTseasonsummary[gPFTseasonsummary$PFT=="C3: grass" | 
                         gPFTseasonsummary$PFT=="C4: shrub/subshrub" | 
                         gPFTseasonsummary$PFT=="CAM",
                       c("fakedate", "year", "PFT", "mean.coverPFT", "mean.volumePFT")],
      format="pandoc")

# Fix dates'
gPFTseasonsummary$fakedate <- as.Date(gPFTseasonsummary$fakedate, format="%Y-%m-%d")

##Plots Now let's make some plots!  
##*by our new PFTs**
gcoverPFT.plot <- ggplot(gPFTseasonsummary, aes(x = fakedate, y = mean.coverPFT, group = PFT, colour=PFT, fill=PFT)) +
  theme_bw() + 
  geom_col(width=150) +
  theme(axis.title.x=element_blank(), 
        legend.key.size=unit(0.8, "lines"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.title=element_text(size=11),
        axis.title.y=element_text(size=11)) +
  scale_x_date(date_labels = "%y", date_breaks = "1 year",
               limits = c(as.Date("1999-01-01"), as.Date("2016-10-01"))) +
  xlab("Time") + ylab(expression(atop("Cover", 
                                      "("*cm^2*"/"*m^2*")")))

gcoverPFT.plot

#####Notes:  Creosote (LATR) biomass has been calculated in several different ways throughout time. 
###No method seems entirely consistent, hence the variation in all creosote biomass metrics. 
##I don't know how to correct for these changes. We might want to exclude some groups.

# Met data
met.data <- read.csv("Met_all.csv", header=TRUE, strip.white=TRUE)
# only use station #49 data
met.data <- met.data[met.data$Sta==49,]
# only keep data and precip data
head(met.data)
met.data <- unique(met.data[,c(2, 3, 4, 5, 7, 11)])
head(met.data)
met.data$season <- as.factor(met.data$Month)
met.data$season <- revalue(met.data$season, c("1"="1","2"="1","3"= "1","4"="1","5"="1","6"="1",
                                              "7"="2","8"="2","9"="2","10"="2","11"="1" ,"12"="1"))
colnames(met.data) <- c("mean.date", "month", "day","year", "Avg_Temp", "Precip", "season")
met.data[met.data==-999] <- NA
head(met.data)


# Make a column of average-season dates which will help make prettier figures
# Format date column 
met.data$mean.date <- as.Date(met.data$mean.date, format="%m/%d/%Y")

#making fake date to wrap years
unique(month(met.data$mean.date[met.data$season==1])) 
met.data$fakedate <- NA
met.data$fakedate[met.data$season=="1"] <- "4/1"
met.data$fakedate[met.data$season=="2"] <- "10/1"
met.data$fakedate <- as.Date(paste(met.data$fakedate, met.data$year), format="%m/%d %Y")
met.data$fakedate[met.data$month==11 | met.data$month==12] <- 
  met.data$fakedate[met.data$month==11 | met.data$month==12] + years(1)
unique(month(met.data$fakedate[month(met.data$mean.date)==11])) #should be 4/April
unique(met.data$fakedate[month(met.data$mean.date)==11 &
                           year(met.data$mean.date)==2000]) #should be April 2001


# Calculate monthly precip
monthlysums <- unique(ddply(met.data, c("year", "month"), function(x)
  data.frame(month.precip = sum(x$Precip, na.rm=TRUE),
             month.temp = mean(x$Avg_Temp, na.rm=TRUE))))
head(monthlysums)
monthlysums$temp.date <- as.Date(paste(1, monthlysums$month, monthlysums$year), 
                                 format="%d %m %Y")
#Calculate seasonal precip
seasonsums <- unique(ddply(met.data, c("fakedate", "season"), function(x)
  data.frame(season.precip = sum(x$Precip, na.rm=TRUE),
             season.temp = mean(x$Avg_Temp, na.rm=TRUE))))
head(seasonsums)

###may use this to change from 1 and 2 to winter and monsoon
seasonsums$season <- revalue(seasonsums$season, c("1" = "winter", "2" = "monsoon")) 
seasonprecip.plot <- ggplot(seasonsums, aes(x = fakedate, y = season.precip, colour=season, fill=season)) +
  geom_col(width=100) + theme_bw() +
  theme(axis.title.x=element_blank(), 
        legend.key.size=unit(0.8, "lines"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.title=element_text(size=11),
        axis.title.y=element_text(size=11)) +
  scale_x_date(date_labels = "%y", date_breaks = "1 year",
               limits = c(as.Date("1999-01-01"), as.Date("2016-10-01"))) +
  xlab("Time") + ylab(expression(atop("Precip","(mm)")))
seasonprecip.plot

### Try to summarize the trapping data, try is the operative word

sev.trap <- read.csv("sev008_rodentpopns_20161027.csv", header=TRUE, strip.white=TRUE)
sev.trap <- sev.trap[sev.trap$location=="5pgrass" | sev.trap$location=="5plarrea",]

# Calculate the number of nights and webs trapped per season
numnight <- ddply(sev.trap, c("year", "location", "season"), function(x)
  data.frame(numnight = length(unique(paste(x$night, x$web)))))
head(numnight)

# Merge numnight column
sev.trap <- unique(merge(sev.trap, numnight, c("year", "location", "season"), all=T))

###calculate the number of trap nights
sev.trap$trapnight <- sev.trap$numnight * 148
head(sev.trap)

##select the grassland site
sev.trap.grass<- sev.trap[sev.trap$location=="5pgrass",]
sev.trap.grass <- subset(sev.trap.grass, year>"1989") 
head(sev.trap.grass)

###summarising by season
grodent.long <- data.frame(summarise(group_by(sev.trap.grass, year, location, season, species, numnight), abundance = length(species)))

##calculate mice per trapnight
grodent.long$miceperwebs <- grodent.long$abundance/grodent.long$numnight
head(grodent.long)

grodent.long$season <- as.factor(grodent.long$season)

#rename seasons to create two bins
grodent.long$season <- revalue(grodent.long$season, c(
  "1" = "spring", 
  "2" = "fall",
  "3" = "fall"))
head(grodent.long)

##create a column of average-season dates which will make it line up with veg and met panels
# Make a column of average-season dates which will help make prettier figures
grodent.long$fakedate <- NA
grodent.long$fakedate[grodent.long$season=="spring"] <- "4/1"
grodent.long$fakedate[grodent.long$season=="fall"] <- "10/1"
grodent.long$fakedate <- as.Date(paste(grodent.long$fakedate, grodent.long$year), format="%m/%d %Y") 

head(grodent.long)
unique(grodent.long$species)
# Combine mice into feeding guilds

grodent.long$guild <- as.character(grodent.long$species)
grodent.long$guild <- revalue(grodent.long$guild, c(
  "chin" = "Other Heteromyid", 
  "dime" = "Dipodomys merriami", 
  "dior"= "Dipodomys Ordii", 
  "disp" = "Dipodomys spectabilis", 
  "pgfv" = "Perognathus sp.",
  "pgfl" = "Perognathus sp.",
  "neal" = "Neotoma sp.", 
  "nemi" = "Neotoma sp.",
  "onar"= "Onychomys sp.", 
  "onle"= "Onychomys sp.", 
  "onsp"= "Onychomys sp.", 
  "pele"= "Peromyscus sp.", 
  "pema"= "Peromyscus sp.", 
  "pedi"= "Peromyscus sp.", 
  "pmma"= "Peromyscus sp.", 
  "pmle"= "Peromyscus sp.", 
  "pmtr"= "Peromyscus sp.", 
  "pmdi"= "Peromyscus sp.", 
  "pmbo"= "Peromyscus sp.",
  "remg"= "Reithrodontomys sp.", 
  "remn"="Reithrodontomys sp.", 
  "resp"="Reithrodontomys sp.",
  "sihi"="Other Cricetid",
  "spsp"= "Other Scurid"))

# Summarize mice by foraging guild
gguildsummary <- ddply(unique(grodent.long[,c("year", "season", "miceperwebs", "fakedate", "guild")]), 
                      c("year", "season", "fakedate", "guild"), function(x)
                        data.frame(sum.guild = sum(x$miceperwebs, na.rm=TRUE)))
head(gguildsummary)
grodent.long <- subset(grodent.long, fakedate>="1999-01-01")
grodent.long$fakedate <- as.Date(grodent.long$fakedate, format="%Y-%m-%d")

gsevmouse.plot <- ggplot(grodent.long, aes(x = fakedate, y = miceperwebs, group=guild, colour=guild, fill=guild)) +
  geom_col(width=150) + theme_bw() +
  theme(axis.title.x=element_blank(), 
        legend.key.size=unit(0.8, "lines"),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),    
        legend.title=element_text(size=11),
        axis.title.y=element_text(size=11)) +
  scale_x_date(date_labels = "%y", date_breaks = "1 year",
               limits = c(as.Date("1999-01-01"), as.Date("2016-10-01"))) +
  xlab("Time") + ylab(expression(atop("Small Mammals", 
                                      "(trapped per bout)")))
gsevmouse.plot

gfinalthreeplots <- plot_grid(seasonprecip.plot, gcoverPFT.plot, gsevmouse.plot,
                             align = 'v', ncol=1, scale=0.98,
                             rel_heights = c(1,1.1,1.2),
                             labels=c('A', 'B', 'C'), hjust=-0.2) 
gfinalthreeplots

print(gfinalthreeplots)
