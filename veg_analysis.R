# Initial version

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
###Read in files### read in files
creosote_raw<-read.csv ("sev182_nppcorewebbiomass_20150816.csv", header=TRUE, strip.white=TRUE)
creosote_raw
head(creosote_raw)

###pull out the data for just the creosote core site
creosote<-creosote_raw[creosote_raw$site=="C",]
colnames(creosote)[7] <- "SpeciesCode"
head(creosote)
#All plant cover/volume data
Ccover <- read.csv ("sev129_nppcorequadrat_20161214.csv", header=TRUE, strip.white = TRUE)
colnames(Ccover)[9] <- "SpeciesCode"
Ccover<- Ccover[Ccover$site=="C",]
Ccover<-Ccover[Ccover$height >0 & Ccover$cover>0,]
head(Ccover)
# Taxonomic, PFT information for all species
sev.species <- read.csv("Complete Sevilleta Species List.csv", header=TRUE, strip.white=TRUE)
colnames(sev.species)[1] <- "SpeciesCode"
head(sev.species)

##### Look at the formats of these files


##Calculations  Now we need to calculate the different metrics of plant production.
##First we'll calculate "mean.volume," the average volumetric space (cm^3) that each species occupies per quadrat per season.

# Calculate the number of quads per site per season
quads <- ddply(Ccover, c("year", "season", "date", "site"), function(x)
  data.frame(quadnum = length(unique(x$quad))))
# Merge quadnum column
Ccover <- unique(merge(Ccover, quads, c("year", "season", "date", "site"), all=T))
head(Ccover)

# Calculate all the average volumes /species /quad in the harvest dataset
Ccover$volume <- Ccover$cover * Ccover$height
sumvolumes <- ddply(unique(Ccover[,c("year", "season", "date", "site", "web", "quad", "quadnum", "SpeciesCode", "volume")]), c("year", "season", "date", "site", "quad", "quadnum", "SpeciesCode"), function(x)
  data.frame(quad.volume = sum(x$volume, na.rm=T)))
head(sumvolumes)
# Now calculate the average volume across all quads /species
meanvolumes <- ddply(unique(sumvolumes[,c("year", "season", "date", "site", "SpeciesCode", "quad", "quadnum", "quad.volume")]), c("year", "season", "date", "site", "SpeciesCode"), function(x)
  data.frame(mean.volume = sum(x$quad.volume, na.rm=T)/x$quadnum))
meanvolumes$season <- as.factor(meanvolumes$season)


##Now we can calculate the the average dry weight (weight; determined using linear regression) and net primary production (npp) for each species per season.

# Calculate the number of quads per site per season
quads <- ddply(creosote, c("year", "site"), function(x)
  data.frame(quadnum = length(unique(x$quad))))
# Merge quadnum column
creosote <- unique(merge(creosote, quads, c("year", "site"), all=T))

# Find the species average of all NPP quadrats
average <- unique(ddply(creosote, c("year", "site", "SpeciesCode"), function(x)
  data.frame(springnpp = sum(x$snpp, na.rm=TRUE)/x$quadnum,
             fallnpp = sum(x$fnpp, na.rm=TRUE)/x$quadnum,
             annualnpp = sum(x$anpp, na.rm=TRUE)/x$quadnum,
             spwt = sum(x$swt, na.rm=TRUE)/x$quadnum,
             fwt = sum(x$fwt, na.rm=TRUE)/x$quadnum)))
# Look at the results for a few common species
head(average)
head(meanvolumes)

##Now we can use the package **reshape** to melt the biomass dataframe so that it can viewed as a time series.

# Melt the npp dataframe so that spring and fall biomass are in one column
melt.ave.one <- melt(average[,c("year", "site", "SpeciesCode", "annualnpp", "springnpp", "fallnpp")], id.vars = c("year", "site", "SpeciesCode", "annualnpp"), variable.name = "season", value.name = "seasonalnpp")
# Revalue season column
melt.ave.one$season <- revalue(melt.ave.one$season, c(
  "springnpp" = "2",
  "fallnpp" = "3"))
# Melt the npp dataframe so that all the weights are in one column
melt.ave.two <- melt(average[,c("year", "site", "SpeciesCode", "spwt", "fwt")], id.vars = c("year", "site", "SpeciesCode"), variable.name = "season", value.name = "season.weight")
# Revalue season column
melt.ave.two$season <- revalue(melt.ave.two$season, c(
  "spwt" = "2",
  "fwt" = "3"))
melt.ave <- unique(merge(melt.ave.one, melt.ave.two, c("year", "site", "SpeciesCode", "season"), all=T))
head(melt.ave)
head(meanvolumes)
# Merge npp and species data, dates first
all.sev.biomass <- merge(melt.ave, meanvolumes[,c("year", "season", "date")], c("year", "season"), all=T)
head(all.sev.biomass)
all.sev.biomass <- merge(all.sev.biomass, meanvolumes, c("year", "season", "date", "site", "SpeciesCode"), all=T)
all.sev.biomass <- merge(all.sev.biomass, sev.species, "SpeciesCode", all.x=T)

head(all.sev.biomass)
# Make a column of average-season dates which will help make prettier figures
unique(month(all.sev.biomass$mean.date[all.sev.biomass$season==1]))
all.sev.biomass$fakedate <- NA
all.sev.biomass$fakedate[all.sev.biomass$season=="1"] <- 1
all.sev.biomass$fakedate[all.sev.biomass$season=="2"] <- 366*(1/3)
all.sev.biomass$fakedate[all.sev.biomass$season=="3"] <- 366*(2/3)
all.sev.biomass$fakedate <- as.POSIXlt(strptime(paste(all.sev.biomass$fakedate, all.sev.biomass$year), format="%j %Y"))

head(all.sev.biomass)


####Plant functional types Since we don't actually care about species-specific numbers, we can condense all of these data into plant functional groups. I have code for traditional groups (creosote, non-creosote C3 plants, C4, and CAM), but am masking it. Including plots for our new PFTs
### Create our own PFT's
##### We should come up with new PFT's!
# Possible categories: LATR, "woody" perennial forbs, annual juicy forbs, perennial juicy forbs, annual grasses, perennial grasses, OR spring-blooming grasses, monsoon-blooming grasses
all.sev.biomass$PFT <- as.character(NA)
all.sev.biomass$PFT[all.sev.biomass$g_f == "g"] <- "C4: grass"
all.sev.biomass$PFT[all.sev.biomass$g_f == "g" & all.sev.biomass$path == "C3"] <- "C3: grass"
all.sev.biomass$PFT[all.sev.biomass$g_f == "f"] <- "C3: forb"
all.sev.biomass$PFT[all.sev.biomass$g_f == "f" & all.sev.biomass$path == "C4"] <- "C4: forb"
all.sev.biomass$PFT[all.sev.biomass$g_f == "s" & all.sev.biomass$path == "C3"] <- "C3: shrub/subshrub"
all.sev.biomass$PFT[all.sev.biomass$path == "CAM"] <- "CAM"
all.sev.biomass$PFT[all.sev.biomass$SpeciesCode == "LATR2"] <- "C3: shrub/subshrub"
all.sev.biomass$PFT[all.sev.biomass$SpeciesCode == "ASMIM"] <- "C3: forb"
all.sev.biomass$PFT[all.sev.biomass$SpeciesCode == "MOSQ"] <- "C4: grass"
all.sev.biomass$PFT[all.sev.biomass$SpeciesCode == "OECA1"] <- "C3: forb"
all.sev.biomass$PFT[all.sev.biomass$SpeciesCode == "SATR12"] <- "C4: shrub/subshrub"
all.sev.biomass$PFT[all.sev.biomass$SpeciesCode == "SATR1"] <- "C4: shrub/subshrub"
all.sev.biomass$PFT <- as.factor(all.sev.biomass$PFT)
all.sev.biomass[is.na(all.sev.biomass$PFT),]
# Fix photo_path gaps and remove unknowns
unique(all.sev.biomass$SpeciesCode[is.na(all.sev.biomass$PlotGroup)])
all.sev.biomass$PlotGroup[!is.na(all.sev.biomass$SpeciesCode) &
                            all.sev.biomass$SpeciesCode == "MUSQ3"] <- "C4"
all.sev.biomass$PlotGroup[!is.na(all.sev.biomass$SpeciesCode) &
                            all.sev.biomass$SpeciesCode == "MOSQ"] <- "C4"
all.sev.biomass$PlotGroup[!is.na(all.sev.biomass$SpeciesCode) &
                            all.sev.biomass$SpeciesCode == "OECA1"] <- "C3sub"
all.sev.biomass$PlotGroup[!is.na(all.sev.biomass$SpeciesCode) &
                            all.sev.biomass$SpeciesCode == "SATR1"] <- "C4"
all.sev.biomass$PlotGroup[!is.na(all.sev.biomass$SpeciesCode) &
                            all.sev.biomass$SpeciesCode == "ASMIM"] <- "C3sub"
all.sev.biomass$PlotGroup[!is.na(all.sev.biomass$SpeciesCode) &
                            all.sev.biomass$SpeciesCode == "CAWR"] <- "C3sub"
all.sev.biomass <- all.sev.biomass[!is.na(all.sev.biomass$SpeciesCode) &
                                     all.sev.biomass$SpeciesCode != "SPCFC" &
                                     all.sev.biomass$SpeciesCode != "STEM",]
head(all.sev.biomass)

# Summarize plant biomass by PlotGroup
all.sev.biomass <- unique(all.sev.biomass)
all.sev.biomass$fakedate <- as.factor(as.character(all.sev.biomass$fakedate))
PGseasonsummary <- ddply(unique(all.sev.biomass[,c("year", "season", "fakedate", "PlotGroup", "seasonalnpp", "mean.volume", "season.weight")]), c("year", "season", "fakedate", "PlotGroup"), function(x)
  data.frame(seasonalnppPG = sum(x$seasonalnpp, na.rm=TRUE),
             mean.volumePG = sum(x$mean.volume, na.rm=TRUE),
             season.weightPG = sum(x$season.weight, na.rm=TRUE)))
PFTseasonsummary <- ddply(unique(all.sev.biomass[,c("year", "season", "fakedate", "PFT", "seasonalnpp", "mean.volume", "season.weight")]), c("year", "season", "fakedate", "PFT"), function(x)
  data.frame(seasonalnppPFT = sum(x$seasonalnpp, na.rm=TRUE),
             mean.volumePFT = sum(x$mean.volume, na.rm=TRUE),
             season.weightPFT = sum(x$season.weight, na.rm=TRUE)))

PGannualsummary <- ddply(unique(all.sev.biomass[,c("year", "PlotGroup", "annualnpp")]), c("year", "PlotGroup"), function(x)
  data.frame(annualnppPG = sum(x$annualnpp, na.rm=TRUE)))
PFTannualsummary <- ddply(unique(all.sev.biomass[,c("year", "PFT", "annualnpp")]), c("year", "PFT"), function(x)
  data.frame(annualnppPFT = sum(x$annualnpp, na.rm=TRUE)))

PGsummary <- merge(PGseasonsummary, PGannualsummary, c("year", "PlotGroup"), all=T)
PFTsummary <- merge(PFTseasonsummary, PFTannualsummary, c("year", "PFT"), all=T)

# Now merge the met.data onto these tables
PGsummary$fakedate <- as.Date(PGsummary$fakedate)
PFTsummary$fakedate <- as.Date(PFTsummary$fakedate)

PGsummary <- unique(merge(PGsummary, unique(met.data[met.data$fakedate >= min(PGsummary$fakedate), c("fakedate", "season.precip")]), by=c("fakedate"), all=T))
PFTsummary <- unique(merge(PFTsummary, unique(met.data[met.data$fakedate >= min(PFTsummary$fakedate), c("fakedate", "season.precip")]), by=c("fakedate"), all=T))
PGsummary <- PGsummary[with(PGsummary, order(fakedate, PlotGroup)),]
PFTsummary <- PFTsummary[with(PFTsummary, order(fakedate, PFT)),]

# Summary of C3 grass, C4 shrub/subshrub, and CAM contribution
PFTsummary <- PFTsummary[!is.na(PFTsummary$PFT),]
kable(PFTsummary[PFTsummary$PFT=="C3: grass" |
                   PFTsummary$PFT=="C4: shrub/subshrub" |
                   PFTsummary$PFT=="CAM",
                 c("fakedate", "year", "season.precip", "PFT", "seasonalnppPFT", "annualnppPFT")],
      format="pandoc")
# Now exclude these groups
PFTsummary <- PFTsummary[PFTsummary$PFT!="C3: grass" &
                           PFTsummary$PFT!="C4: shrub/subshrub" &
                           PFTsummary$PFT!="CAM",]
##Plots Now let's make some plots! These plots are all based on measurements of plant height and cover (Volume). Using species-specific regressions created by the Sev team (mostly Doug Moore), these measurements were converted into dry weights (Seasonal Weight). From these weights, seasonal increases in biomass (Seasonal NPP) were calculated. The sum of yearly seasonal biomass increases is presented as Annual NPP. I think only one or two of these metrics should be selected to create a coherent time series of plant production at the Sevilleta.

#####Notes:  Creosote (LATR) biomass has been calculated in several different ways throughout time. No method seems entirely consistent, hence the variation in all creosote biomass metrics. I don't know how to correct for these changes. We might want to exclude some groups.

## by PFTs**
seasonalvolume.plot <- ggplot(PFTsummary[PFTsummary$season!=1,], aes(x = fakedate, y = mean.volumePFT, group = PFT, fill = PFT)) +
  theme_bw() + scale_fill_brewer(palette = "Set1") +
  geom_bar(stat="identity", width=110) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  xlab("Time") + ylab("Volume \n(cm^3/m^2)")

seasonalweight.plot <- ggplot(PFTsummary[PFTsummary$season!=1,], aes(x = fakedate, y = season.weightPFT, group = PFT, fill = PFT)) +
  theme_bw() + scale_fill_brewer(palette = "Set1") +
  geom_bar(stat="identity", width=110) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  xlab("Time") + ylab("Seasonal Weight \n(g/m^2)")

seasonalbiomass.plot <- ggplot(PFTsummary[PFTsummary$season!=1,], aes(x = fakedate, y = seasonalnppPFT, group = PFT, fill = PFT)) +
  theme_bw() + scale_fill_brewer(palette = "Set1") +
  geom_bar(stat="identity", width=110) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  xlab("Time") + ylab("Seasonal NPP \n(g/m^2)")

annualbiomass.plot <- ggplot(PFTsummary[PFTsummary$season==3,], aes(x = fakedate-days(60), y = annualnppPFT, group = PFT, fill = PFT)) +
  theme_bw() + scale_fill_brewer(palette = "Set1") +
  geom_bar(stat="identity", width=240) +
  theme(legend.position = c(0.95, 0.65), legend.key.size = unit(0.7, "lines")) +
  labs(fill = "PFT") +
  xlab("Time") + ylab("Annual NPP \n(g/m^2)")

fourbiomassplots <- plot_grid(seasonalvolume.plot, seasonalweight.plot, seasonalbiomass.plot, annualbiomass.plot, align = 'v', ncol=1, rel_heights = c(1,1,1,1.2))
fourbiomassplots
##Let's calculate a few different metrics of precip. I think monthly sum bins and seasonal sum bins would be most informative. A rolling sum might also be useful.

# Met data
met.data <- read.csv("Met_all.csv", header=TRUE, strip.white=TRUE)
# only use station #49 data
met.data <- met.data[met.data$Sta==49,]
# only keep data and precip data
met.data <- unique(met.data[,c(1,2,4,10)])
met.data$season <- as.factor(met.data$Month)
met.data$season <- revalue(met.data$season, c(
  "1" = "1",
  "2" = "2", "3" = "2", "4" = "2", "5" = "2",
  "6" = "3", "7" = "3", "8" = "3", "9" = "3",
  "10" = "1", "11" = "1", "12" = "1"))
colnames(met.data) <- c("mean.date", "month", "year", "Precip", "season")
met.data[met.data==-999] <- NA

# Calculate monthly and seasonal precip sums
precipsums <- unique(ddply(met.data, c("year", "month"), function(x)
  data.frame(month.precip = sum(x$Precip, na.rm=TRUE))))
# Remember, I want the winter season to include ONDJ
season.precip <- rollapply(c(NA,NA,NA,precipsums$month.precip), 4, sum, by=4, align="right", fill=NA)
precipsums$season.precip <- c(NA, rep(season.precip[!is.na(season.precip)], each=4))
met.data <- merge(met.data, precipsums, by=c("year", "month"), all=T)

# Format date column and get rid of data before 1998
met.data$mean.date <- as.Date(as.POSIXlt(strptime(met.data$mean.date, format="%m/%d/%Y")))
met.data <- subset(met.data, year>=1998)

# Make a column of average-season dates which will help make prettier figures
unique(month(met.data$mean.date[met.data$season==1]))
met.data$fakedate <- NA
met.data$fakedate[met.data$season=="1"] <- 1
met.data$fakedate[met.data$season=="2"] <- 366*(1/3)
met.data$fakedate[met.data$season=="3"] <- 366*(2/3)
met.data$fakedate <- as.Date(strptime(paste(met.data$fakedate, met.data$year), format="%j %Y"))
met.data$fakedate[met.data$month==10 | met.data$month==11 | met.data$month==12] <- met.data$fakedate[met.data$month==10 | met.data$month==11 | met.data$month==12] + years(1)

# Sev trapping counts
# all sev rodent data
sev.trap <- read.csv("rodentdata2016_rodentdataall.csv", header=TRUE, strip.white=TRUE)
sev.trap$season <- revalue(as.factor(sev.trap$season), c(
  "1" = "2",
  "2" = "2p5",
  "3" = "3"))
sev.trap <- sev.trap[sev.trap$location=="5pgrass" | sev.trap$location=="5plarrea",]
head(sev.trap)

# trap nights
trap.nights <- read.csv("rodentdata2016_weightedaverages.csv", header=TRUE, strip.white=TRUE)
trap.nights <- trap.nights[,c("Year", "Season", "Black.Grama", "TrapNights")]
colnames(trap.nights) <- c("year", "season", "location", "trapnights")
trap.nights$season <- revalue(trap.nights$season, c(
  "Fall" = "3",
  "Fall*" = "3",
  "Spring" = "2",
  "Summer" = "2p5"))
trap.nights$location <- revalue(trap.nights$location, c(
  "5ptsgrass" = "5pgrass",
  "5ptslarrea" = "5plarrea"))
sev.trap <- merge(sev.trap, trap.nights, c("year", "season", "location"), all=T)

##Calculate the total mice captured per season per trapnight
# Subset
micesums <- sev.trap[sev.trap$species.1 != "s",]
#micesums <- micesums[micesums$recap == "n",]

# Revalue a few species
micesums$species <- as.character(micesums$species)
micesums$species <- revalue(micesums$species, c(
  "pgfl" = "pgfv",
  "nemi" = "neal", #actually nesp
  "pmbo" = "pmsp", #should I include all of these?
  "pmdi" = "pmsp",
  "pmma" = "pmsp",
  "pmsp" = "pmsp",
  "pmer" = "pmsp",
  "pmle" = "pmsp"))
micesums$species <- as.factor(micesums$species)
head(micesums)
# Calculate seasonal trap sums per species
micesums <- unique(ddply(micesums, c("year", "season", "location", "species", "trapnights"), function(x)
  data.frame(total.cap = length(x$trap))))
micesums$cap.trap <- micesums$total.cap / micesums$trapnights
# Calculate total capture per trapnight at the 5pt sites
ave.mouse.cap <- unique(ddply(micesums, c("year", "season", "species"), function(x)
  data.frame(ave.cap.trap = mean(x$cap.trap, na.rm=T),
             sum.cap.trap = sum(x$cap.trap, na.rm=T))))
micesums <- merge(micesums, ave.mouse.cap, c("year", "season", "species"), all=T)

# Combine mice into feeding guilds
ave.mouse.cap$guild <- as.character(ave.mouse.cap$species)
heteromyids <- c("chin", "dime", "dior", "dipo", "disp", "pgfv")
cricetids <- c("neal", "pmsp", "pmtr", "remg", "remn", "resp", "onar", "onle", "onsp", "sihi")
levels = c("Heteromyids", "Cricetids")


##Look at the precip record over this time.I think it's cool to compare these numbers to the biomass. A few good seasons can bump up the biomass in subsequent seasons as much a big rainy season.

precip.plot <- ggplot(PGsummary, aes(x = fakedate, y = season.precip)) +
theme_bw() +
geom_line() +
geom_point(size=2, aes(colour=season.precip, fill=season.precip)) +
geom_rect(aes(fill = season.precip, ymin=-75, ymax=0,
xmin=fakedate-days(61), xmax=fakedate+days(61))) +
scale_fill_gradient2(low = '#67001f', high = '#2166ac', mid='#f7f7f7',
midpoint = mean(met.data$season.precip, na.rm=T)) +
  scale_colour_gradient2(low = '#67001f', high = '#2166ac', mid='#f7f7f7',
                         midpoint = mean(met.data$season.precip, na.rm=T)) +
  theme(legend.position = "bottom") +
  xlab("Time") + ylab("Seasonal Precip \n(mm)")
precip.plot


#*Plot percent contribution each group**
 PFTweightpercent.plot <- ggplot(PFTsummary[!is.na(PFTsummary$PFT),], aes(x = fakedate, y = season.weightPFT, group = PFT, fill = PFT)) +
  theme_bw() + scale_fill_brewer(palette = "Set1") +
  geom_bar(stat="identity", position="fill", width=110) +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  xlab("Time") + ylab("Seasonal Weight \n(g/m^2)")

PFTnpppercent.plot <- ggplot(PFTsummary[!is.na(PFTsummary$PFT),], aes(x = fakedate, y = seasonalnppPFT, group = PFT, fill = PFT)) +
  theme_bw() + scale_fill_brewer(palette = "Set1") +
  geom_bar(stat="identity", position="fill", width=110) +
  theme(legend.position = c(0.95, 0.65), legend.key.size = unit(0.7, "lines")) +
  labs(fill = "PFT") +
  xlab("Time") + ylab("Seasonal NPP \n(g/m^2)")

annualpercent.plot <- ggplot(PFTsummary[!is.na(PFTsummary$PFT) & PFTsummary$season==3,], aes(x = fakedate-days(60), y = annualnppPFT, group = PFT, fill = PFT)) +
  theme_bw() + scale_fill_brewer(palette = "Set1") +
  geom_bar(stat="identity", position="fill", width=240) +
  theme(legend.position = c(0.95, 0.65), legend.key.size = unit(0.7, "lines")) +
  labs(fill = "PFT") +
  xlab("Time") + ylab("Annual NPP \n(g/m^2)")

percentplots <- plot_grid(PFTweightpercent.plot, PFTnpppercent.plot, annualpercent.plot, align = 'v', ncol=1, rel_heights = c(1,1,1.2))
percentplots

# Correlations with precip
cor.plotPG <- ggplot(PGsummary, aes(x = season.precip, y = seasonalnppPG, group = PlotGroup, colour=PlotGroup)) + theme_bw() +
  scale_colour_brewer(palette = "Set2") +
  geom_point(shape=1) + stat_smooth(method="lm")

cor.plotPFT <- ggplot(PFTsummary, aes(x = season.precip, y = seasonalnppPFT, group = PFT, colour=PFT)) + theme_bw() +
  scale_colour_brewer(palette = "Set1") +
  geom_point(shape=2) + stat_smooth(method="lm")

grid.arrange(cor.plotPG, cor.plotPFT)


#### End goal figure One of our ideas was to make stacked bar graphs of standing plant biomass (grouped by plant functional group) and highlight the proportion of the weight that was new growth (net primary production). To overlay these layers, I've decided it's easiest to use geom-bar() and overlay geom-rect(). Because geom-rect() needs x,y coordinates, let's make a data.frame of ymin and ymax!

ycoordsPG <- data.frame(PGsummary[!is.na(PGsummary$PlotGroup) & PGsummary$season != "1", c("fakedate", "PlotGroup", "season.weightPG", "seasonalnppPG")])
ycoordsPG$ymin <- NA
ycoordsPG$ymin[ycoordsPG$PlotGroup == "C3sub"] <- 0
ycoordsPG$ymin[ycoordsPG$PlotGroup == "C4"] <-
ycoordsPG$season.weightPG[ycoordsPG$PlotGroup == "C3sub"]
ycoordsPG$ymin[ycoordsPG$PlotGroup == "CAM"] <-
ycoordsPG$season.weightPG[ycoordsPG$PlotGroup == "C3sub"] +
ycoordsPG$season.weightPG[ycoordsPG$PlotGroup == "C4"]
ycoordsPG$ymin[ycoordsPG$PlotGroup == "LATR"] <-
ycoordsPG$season.weightPG[ycoordsPG$PlotGroup == "C3sub"] +
ycoordsPG$season.weightPG[ycoordsPG$PlotGroup == "C4"] +
ycoordsPG$season.weightPG[ycoordsPG$PlotGroup == "CAM"]
ycoordsPG$ymax <- NA
ycoordsPG$ymax <- ycoordsPG$ymin + ycoordsPG$seasonalnppPG

##Note:* Right now, there is just a black box around the NPP values. Adding a texture in ggplot is stupidly hard, but I could do it. A prettier solution may be to use a lighter version of the same colour to represent NPP. When we decide on final groups, we can change those colours.
##These are two of the components (precip and biomass) we might want in the final Figure 1. They have been set to the same time scales, but I'm keeping them as separate figures for now. Even looking at two of these figures side-by-side seems busy to me. Maybe the rodent-plant-precip figures should remain as 3 separate panels?
##Now that I'm looking at these plots, I realize I don't really care about NPP. It's true that it may represent yummy fresh growth, but standing biomass and NPP are both just proxies for what we really care about: seed production. We hope that a lot of biomass or new biomass represents a good seed year, but we don't know. So I think we should simplify and just include standing biomass.

finalPFT.plot <- ggplot(PFTsummary[PFTsummary$season!=1,], aes(x = fakedate, y = season.weightPFT, group = PFT, fill = PFT, colour=PFT)) +
theme_bw() +
geom_bar(stat="identity", width=100) +
theme(axis.title.x=element_blank(),
legend.key.size=unit(0.8, "lines"),
legend.background = element_rect(fill = "transparent", colour = "transparent"),
legend.title=element_text(size=11),
axis.title.y=element_text(size=11)) +
scale_fill_manual(values=c("#c2a5cf", "#7b3294", "#a6dba0", "#008837"), name = "Plant Functional\nGroup") +
scale_colour_manual(values=c("#c2a5cf", "#7b3294", "#a6dba0", "#008837"), name = "Plant Functional\nGroup") +
scale_x_date(date_labels = "%Y", date_breaks = "2 year",
limits = c(as.Date("2004-05-01"), as.Date("2015-09-01"))) +
xlab("Time") + ylab(expression(atop("Standing Biomass",
"(g/"*m^2*")")))

finalPFTNPP.plot <- ggplot(PFTsummary[PFTsummary$season!=1,], aes(x = fakedate, y = seasonalnppPFT, group = PFT, fill = PFT, colour=PFT)) +
theme_bw() +
geom_bar(stat="identity", width=100) +
theme(axis.title.x=element_blank(), legend.key.size=unit(0.8, "lines"),
legend.background = element_rect(fill = "transparent", colour = "transparent"),
legend.title=element_text(size=11),
axis.title.y=element_text(size=11)) +
scale_fill_manual(values=c("#c2a5cf", "#7b3294", "#a6dba0", "#008837"), name = "Plant Functional\nGroup") +
scale_colour_manual(values=c("#c2a5cf", "#7b3294", "#a6dba0", "#008837"), name = "Plant Functional\nGroup") +
scale_x_date(date_labels = "%Y", date_breaks = "2 year",
limits = c(as.Date("2004-05-01"), as.Date("2015-09-01"))) +
xlab("Time") + ylab(expression(atop("Seasonal NPP",
"(g/"*m^2*")")))

precip.plot2 <- ggplot(PGsummary, aes(x = fakedate, y = season.precip, fill=season.precip, colour=season.precip)) +
theme_bw() +
geom_bar(stat="identity", width=100) +
scale_fill_gradient2(low = '#67001f', high = '#4393c3', mid='#f7f7f7',
midpoint = mean(met.data$season.precip, na.rm=T),
name="Precipitation") +
  scale_colour_gradient2(low = '#67001f', high = '#4393c3', mid='#f7f7f7',
                         midpoint = mean(met.data$season.precip, na.rm=T),
                         name="Precipitation") +
  theme(axis.title.x=element_blank(), legend.key.size=unit(0.6, "lines"),
        legend.title=element_text(size=11),
        axis.title.y=element_text(size=11)) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 year",
               limits = c(as.Date("2004-05-01"), as.Date("2015-09-01"))) +
  xlab("Time") + ylab("Seasonal\nPrecipitation\n(mm)")

sevmouseplot <- ggplot(ave.mouse.cap, aes(x = fakedate, y = sum.cap.trap, group=guild, colour=guild)) +
  theme_bw() +
  #  geom_line(alpha=0.5) +
  stat_summary(fun.y = mean, geom="line", alpha=0.8, size=0.8) +
  geom_point(data=ave.mouse.cap[ave.mouse.cap$guild=="pgfv",],
             aes(y=sum.cap.trap, colour=guild)) +
  scale_colour_manual(name="Species", guide="legend",
                      values=c("#e31a1c", "#fd8d3c", "#fecc5c"),
                      labels=c(expression(italic("Perognathus flavus")),
                               "Other Heteromyids", "Cricetids")) +
  theme(legend.key.size=unit(0.8, "lines"),
        legend.title=element_text(size=11),
        axis.title.y=element_text(size=11),
        legend.text.align = 0) +
  ylim(0, 0.32) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 year",
               limits = c(as.Date("2004-05-01"), as.Date("2015-09-01"))) +
  xlab("Time") + ylab("Rodent\nabundance\n(ind./trap night)")

finalthreeplots <- plot_grid(precip.plot2, finalPFT.plot, finalPFTNPP.plot, sevmouseplot,
                             align = 'v', ncol=1, scale=0.98,
                             rel_heights = c(1,1.1,1.1,1.4),
                             labels=c('A', 'B', 'C', 'D'), hjust=-0.2)


##Write out PFTsummary table and final figures
print(finalthreeplots)
ggsave(file="/Users/alesia/Desktop/NobleData/Figure1plots_26Jan2017.png", width=9, height=8, units="in", dpi=300)

write.csv(PFTsummary, file="/Users/alesia/Desktop/NobleData/Fig1.biomassDATA_16Jan2017.csv", row.names = F)

# Summary of species in each category
#write.csv(unique(all.sev.biomass[, c("SpeciesCode", "family", "genus", "species", "common.name", "path", "a_p", "g_f", "PFT")]), file="/Users/alesia/Desktop/NobleData/PFT_groups9Jan2017.csv", row.names = F)

# Summary of precip data
write.csv(unique(met.data[, c("season", "fakedate", "season.precip")]), file="/Users/alesia/Desktop/NobleData/Precip16Jan2017.csv", row.names = F)
