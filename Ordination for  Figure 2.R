library(ggplot2)
library(lubridate)
library(plyr)
library(reshape2)
library(gridExtra)
library(cowplot)
library(devtools)
library(dplyr)
library(tidyr)
library(vegan)
library(codyn)
library(calibrate)

###Figure 2 - Ordination################################################

sev.trap <- read.csv("sev008_rodentpopns_20161027.csv", header=TRUE, strip.white=TRUE)
sev.trap <- sev.trap[sev.trap$location=="5pgrass" | sev.trap$location=="5plarrea",]

# Calculate the number of nights and webs trapped per season
numnight <- ddply(sev.trap, c("year", "location", "season"), function(x)
  data.frame(numnight = length(unique(paste(x$night, x$web)))))
head(numnight)

# Merge numnight column
sev.trap <- unique(merge(sev.trap, numnight, c("year", "location", "season"), all=T))

###summarising by season
sev.trap <- data.frame(summarise(group_by(sev.trap, year, location, season, numnight, species), abundance = length(species)))

##calculate mice per trapnight
sev.trap$miceperwebs <- round((sev.trap$abundance/sev.trap$numnight)*10,0)
sev.trap$season<- as.factor(sev.trap$season)
head(sev.trap)

#rename seasons to create two bins
#sev.trap$season <- revalue(sev.trap$season, c(
#  "1" = "spring", 
# "2" = "summer",
#  "3" = "fall"))
#head(sev.trap)

# Combine mice into feeding guilds

sev.trap$guild <- as.character(sev.trap$species)
heteromyids <- c("chin", "pgsp","dime", "dior", "dipo", "disp", "pgfl", "pgfv")
cricetids <- c("neal", "nemi","nesp", "onar", "onle", "onsp", "pebo", "pedi", "peer", 
              "pele", "pema", "petr", "pesp", "remg", "remn", "resp", "sihi", "pmer", 
             "pmle", "pmsp", "pmtr", "pmdi", "pmbo", "pmma")
scurids <- c("amin", "amle", "eudo", "euqu", "spsp", "spva")
leporids<- c("syau")

sev.trap$guild[sev.trap$guild %in% heteromyids] <- "Heteromyids"
sev.trap$guild[sev.trap$guild %in% cricetids] <- "Cricetids"
sev.trap$guild[sev.trap$guild %in% scurids] <- "Scurids"
sev.trap$guild[sev.trap$guild %in% leporids] <- "Leporids"
sev.trap$guild <- as.factor(sev.trap$guild)
sev.trap$guild <- factor(sev.trap$guild,
                              levels = c("Heteromyids", "Cricetids", "Scurids", "Leporids"))
head(sev.trap)
###creating new location column
sev.trap = within(sev.trap, {
  site = 0
  site = site + 1*(location == "5plarrea")
  site = site + 2*(location == "5pgrass")
  site = factor(site, levels=1:2, labels=c('C', 'G'))
})

sev.trap <- cbind(site=sev.trap[,length(sev.trap)], sev.trap[,c(1,3:(length(sev.trap)-1))])
head(sev.trap)
sev.trap$numnight<-NULL
sev.trap$abundance<-NULL
head(sev.trap)
###select the heteromyids
#ev.het<- sev.trap[sev.trap$guild=="Heteromyids",]
#ead(sev.het)
#tr(sev.het)
###converting to wide format
sev.trap.wide<-spread(sev.trap, species, miceperwebs)
sev.trap.wide[is.na(sev.trap.wide)]<-0 
head(sev.trap.wide)
  
  ###Ordination
############
###heteromyids
rodent.comb.full <- subset(sev.trap.wide, guild == "Heteromyids")
rodent.spp.full <- rodent.comb.full[,c(5:32)]
rodent.env.full <- rodent.comb.full[,c(1:3)]

###ordination
ord.het <- metaMDS(rodent.spp.full, distance = "bray", trace = TRUE)
ord.het.scores <- scores(ord.het, display = c("sites","species"))
summary(ord.het.scores)

#adding grouping variables to scores data.frame
ord.het.full <- cbind(rodent.env.full, ord.het.scores)
head(ord.het.full)

###cricetids
rodent.comb <- subset(sev.trap.wide, guild == "Cricetids")
rodent.spp <- rodent.comb[,c(5:32)]
rodent.env <- rodent.comb[,c(1:3)]

###ordination
ord.cri <- metaMDS(rodent.spp, distance = "bray", trace = TRUE)
ord.cri.scores <- scores(ord.cri, display = c("sites", "species"))
summary(ord.cri.scores)

#adding grouping variables to scores data.frame
ord.cri.full <- cbind(rodent.env, ord.cri.scores)
head(ord.cri.full)


######
###plot
par(mfrow = c(2, 1))

#heteromyids
par(mar=c(4, 4.5, .5, .5), xpd=TRUE, las=1)
plot(ord.het.full[,4:5], type = "n", xlim = c(-2, 2), ylim = c(-2, 2), cex.axis=1, cex.lab=1)

points(subset(ord.het.full, site == "C")[,4:5], cex = 1.1, pch=16, col="#990000")
points(subset(ord.het.full, site == "G")[,4:5], cex = 1.1, pch=16, col="#99CC66")

leg.text <- 	c("shrubland", "grassland")
leg.col <- c("#990000", "#99CC66")
legend("topright", cex = .8, leg.text, col=leg.col, pch=16, title= "heteromyid community")


#cricetids
par(mar=c(4, 4.5, .5, .5), xpd=TRUE, las=1)
plot(ord.cri.full[,4:5], type = "n", xlim = c(-0.5,0.5), ylim = c(-0.5, 0.5), cex.axis=1, cex.lab=1)

points(subset(ord.cri.full, site == "C")[,4:5], cex = 1.1, pch=16, col="#990000")
points(subset(ord.cri.full, site == "G")[,4:5], cex = 1.1, pch=16, col="#99CC66")

legend("topright", cex = .8, leg.text, col=leg.col, pch=16, title= "cricetid community")


