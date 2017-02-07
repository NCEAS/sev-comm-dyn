require(dplyr)
require(tidyr)
require(vegan)
require(codyn)

###Rodent data
###############
###1989-2015
SEV008 <- read.csv("SEV008.csv")

SEV008 <- subset(SEV008, location == "5pgrass" | location == "5plarrea")
SEV008 <- subset(SEV008, species != "spsp")
SEV008 <- subset(SEV008, recap != "y")


###summarising by web
#sum across traps and web
rodent.long <- data.frame(summarise(group_by(SEV008, year, location, season, web, species), abundance = length(species)))


###creating density column
#density= 4.9087 ha
rodent.long$density <- round(rodent.long$abundance/4.9087, 2)



###creating rodent feeding strategy column
hetero <- c("dime", "dior", "dipo", "disp", "pgfl", "pgfv")
`%!in%` <- Negate(`%in%`) 

rodent.long = within(  rodent.long, {
  feed_type = 0
  feed_type = feed_type + 1*(species %in% hetero)
  feed_type = feed_type + 2*(species %!in% hetero)
  
  feed_type = factor(feed_type, levels=1:2, labels=c('heteromyid', 'cricetid'))
})



###creating new location column
rodent.long = within(  rodent.long, {
  site = 0
  site = site + 1*(location == "5plarrea")
  site = site + 2*(location == "5pgrass")
  
  site = factor(site, levels=1:2, labels=c('C', 'G'))
})

rodent.long <- cbind(site=rodent.long[,length(rodent.long)], rodent.long[,c(1,3:(length(rodent.long)-1))])

head(rodent.long)


###converting to wide format
rodent.wide <- spread(rodent.long, species, density)
rodent.wide[is.na(rodent.wide)] <- 0

head(rodent.wide)



###Ordination
############

###heteromyids
rodent.comb.full <- subset(rodent.wide, feed_type == "heteromyid")
rodent.spp.full <- rodent.comb.full[,c(7:33)]
rodent.env.full <- rodent.comb.full[,c(1:4)]

###ordination
ord.het.full <- metaMDS(rodent.spp.full, distance = "bray", trace = TRUE)
ord.het.scores.full <- scores(ord.het.full, display = c("sites", "species"))
summary(ord.het.scores.full)

#adding grouping variables to scores data.frame
ord.het.full <- cbind(rodent.env, ord.het.scores)
head(ord.het.full)



###cricetids
rodent.comb <- subset(rodent.wide, feed_type == "cricetid")
rodent.spp <- rodent.comb[,6:32]
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
par(mar=c(4, 5, 2, 2), xpd=TRUE, las=1)
plot(ord.het.full[,4:5], type = "n", xlim = c(-2, 2), ylim = c(-2, 2), cex.axis=1.5, cex.lab=1.5)
 
points(subset(ord.het.full, site == "C")[,4:5], cex = 1.2, pch=16, col="#990000")
points(subset(ord.het.full, site == "G")[,4:5], cex = 1.2, pch=16, col="#99CC66")

textxy(ord.het.full$NMDS1, ord.het.full$NMDS2, ord.het.full$year)

leg.text <- 	c("creosote", "black grama")
leg.col <- c("#990000", "#99CC66")
legend("topright", cex = 1, leg.text, col=leg.col, pch=16, title= "heteromyid community")


#cricetids
par(mar=c(4, 5, 2, 2), xpd=TRUE, las=1)
plot(ord.cri.full[,4:5], type = "n", xlim = c(-15, 0), ylim = c(-10, 10), cex.axis=1.5, cex.lab=1.5)
 
points(subset(ord.cri.full, site == "C")[,4:5], cex = 1.2, pch=16, col="#990000")
points(subset(ord.cri.full, site == "G")[,4:5], cex = 1.2, pch=16, col="#99CC66")

legend("topright", cex = 1, leg.text, col=leg.col, pch=16, title= "cricetid community")




###SIMPER
#########

###between sites
data <- rodent.wide
col.nums <- 6:32

sim.site <- with(data, simper(data[,col.nums], site))
  	sim.site <- summary(sim.site,  ordered = TRUE, digits = max(3))



###between time periods
site.unq <- as.vector(unique(SevRodent$site))
site.n <- length(unique(SevRodent$site))

#simper.df <- data.frame(cbind(x1 = character(0),
#															x1 = character(0),
#															x1 = character(0),
#															x1 = character(0),
#															x1 = character(0),
#															level = character(0),
#															site = character(0)))

simper.df <- vector("list", 2)


for (iii in 1:site.n){
	site <- subset(data, site == site.unq[iii])
	year.n <- length(unique(site$year))
	n <- (year.n/3)
		if (n != round(n)){
			n <- as.numeric(substring(n, 1, 1))
			Level <- vector("list", length(3))
			n.stp <- round(seq(n, year.n, length.out=3))
			n.str <- c(1, n.stp[3]-n.stp[2], n.stp[3]-n.stp[1])
			for (ii in 1:3){
				year.unq <- (unique(site$year))
				yr.sub <- year.unq[n.str[ii]]:year.unq[n.stp[ii]]
				sub <- subset(site, year %in% yr.sub)
				sub.nrow <- nrow(sub)
				
				Level[[ii]] <- data.frame(level=c(rep(ii, sub.nrow)))
			}
		level <- do.call(rbind, lapply(Level, data.frame, stringsAsFactors=FALSE))	
		}else if (n == round(n)){
			Level <- vector("list", length(3))
			n.cnt <- c(1, n+1, (n*2)+1)
			for (ii in 1:3){
				year.unq <- (unique(site$year))
				yr.sub <- year.unq[n.cnt[ii]]:year.unq[n*ii]
				sub <- subset(site, year %in% yr.sub)
				sub.nrow <- nrow(sub)
				
				Level[[ii]] <- data.frame(level=c(rep(ii, sub.nrow)))
			}
		level <- do.call(rbind, lapply(Level, data.frame, stringsAsFactors=FALSE))	
		}
	dat.sim.list <- vector("list", 3)
	level.names <- c("levels 1 and 2", "levels 1 and 3", "levels 2 and 3")
	for (i in 1:3){
		sim <- with(level, simper(site[,col.nums], level))
  	sim <- summary(sim,  ordered = TRUE, digits = max(3))
		species <- data.frame(sim[[i]][1:5,c(1,6)])
		species$level <- level.names[i]
		
		dat.sim.list[[i]] <- species
	}
	df <- do.call(rbind, lapply(dat.sim.list, data.frame, stringsAsFactors=FALSE))
	df$site <- site.unq[iii]
	
	simper.df <-  rbind(simper.df, df)
}



###Rank Clocks
##############
simp.spp <- rownames(data.frame(sim.site[[1]][1:5,]))

rodent.long.simp <- subset(rodent.long, species %in% simp.spp)

rodent.rank <- aggregate(density ~ species*year*site,
	data = rodent.long.simp,
	FUN = mean)

rodent.rank <- within(rodent.rank, site <- factor(site, levels = c("C", "G")))

rodent.rank = within(  rodent.rank, {
  location = 0
  location = location + 1*(site == "C")
  location = location + 2*(site == "G")
  
  location = factor(location, levels=1:2, labels=c('Creosote', 'Black grama'))
})

head(rodent.rank)


###plotting
summary(rodent.rank)

ggplot(rodent.rank, aes(year, density, color = species)) + xlab("Year") + ylab("Density") +
	geom_line(size = 1.5) + theme_bw() + facet_wrap(~location) + theme(legend.position="bottom", text = element_text(size=25), strip.background=element_blank(), strip.text=element_text(size=25))

ggplot(rodent.rank, aes(year, density, color = species)) + xlab("Year") + ylab("Density") +
	geom_line(size = 1.5) + coord_polar() + theme_bw() + facet_wrap(~location) + theme(legend.position="bottom", text = element_text(size=25), strip.background=element_blank(), strip.text=element_text(size=25))




######################
###Temporal Diversity
#####################
head(rodent.long)

div.full.long <- rodent.long

div.full.long <- div.full.long
levels(div.full.long$site)[levels(div.full.long$site)=="C"] <- "Creosote"
levels(div.full.long$site)[levels(div.full.long$site)=="G"]   <- "Black grama"
	
	
###Richness
###########
rich <- data.frame(summarise(group_by(div.full.long, site, feed_type, year), richness=length(unique(species))))

rich.het <- subset(rich, feed_type == "heteromyid")
rich.cit <- subset(rich, feed_type == "cricetid")

###Turnover
##########
div.long <- aggregate(density ~ species*year*site*feed_type,
	data = div.full.long,
	FUN = mean)

div.long$group_var <- paste(div.long$site, div.long$feed_type, sep="-")

head(div.long)
	
turn.ttl <- turnover(df=div.long, abundance.var="density", replicate.var="group_var")

turn.ttl <- within(turn.ttl, group_var <- factor(group_var, levels = c("Creosote-heteromyid", "Creosote-cricetid", "Black grama-heteromyid", "Black grama-cricetid")))

turn.app <- turnover(df=div.long, abundance.var="density", replicate.var="group_var", metric="appearance")
turn.dis <- turnover(df=div.long, abundance.var="density", replicate.var="group_var", metric="disappearance")

turn <- merge(turn.app, turn.dis, by = c("year", "group_var"))

group_var <- colsplit(turn$group_var, "-", names=c("site", "feed_type"))
turn <- cbind(turn, group_var)
turn$disappearance <- turn$disappearance * -1

turn.het <- subset(turn, feed_type == "heteromyid")
turn.cit <- subset(turn, feed_type == "cricetid")

turn.het <- within(turn.het, site <- factor(site, levels = c("Creosote", "Black grama")))
turn.cit <- within(turn.cit, site <- factor(site, levels = c("Creosote", "Black grama")))


###Mean Rank Shift
##################
rodent.mrank <- mean_rank_shift(df=div.long, abundance.var="density", replicate.var="group_var")
rodent.mrank$year<- as.numeric(substr(rodent.mrank$year_pair, 6, 9))

group_var <- colsplit(rodent.mrank$group_var, "-", names=c("site", "feed_type"))
rodent.mrank <- cbind(rodent.mrank, group_var)

rodent.mrank


rodent.mrank.het <- subset(rodent.mrank, feed_type == "heteromyid")
rodent.mrank.cit <- subset(rodent.mrank, feed_type == "cricetid")


rodent.mrank.het <- within(rodent.mrank.het, site <- factor(site, levels = c("Creosote", "Black grama")))
rodent.mrank.cit <- within(rodent.mrank.cit, site <- factor(site, levels = c("Creosote", "Black grama")))

rodent.mrank.cit[is.na(rodent.mrank.cit)] <- 0


###Rate Change
##############
rate <- rate_change(df=div.long, abundance.var="density", replicate.var="group_var")
intervals <- rate_change_interval(df=div.long, abundance.var="density", replicate.var="group_var")


group_var <- colsplit(intervals$group_var, "-", names=c("site", "feed_type"))
intervals <- cbind(intervals, group_var)


intervals.het <- subset(intervals, feed_type == "heteromyid")
intervals.cit <- subset(intervals, feed_type == "cricetid")

intervals.het <- within(intervals.het, site <- factor(site, levels = c("Creosote", "Black grama")))
intervals.cit <- within(intervals.cit, site <- factor(site, levels = c("Creosote", "Black grama")))


###Plotting
###########
rich.het$year <- as.numeric(rich.het$year)
rich.cit$year <- as.numeric(rich.cit$year)
turn.het$year <- as.numeric(turn.het$year)
turn.cit$year <- as.numeric(turn.cit$year)
rodent.mrank.het$year <- as.numeric(rodent.mrank.het$year)
rodent.mrank.cit$year <- as.numeric(rodent.mrank.cit$year)


richness.het <- ggplot(rich.het, aes(year, richness, color = site)) + expand_limits(y=c(0,8)) + scale_x_continuous(breaks=c(1990, 2000, 2010)) + xlab("Year") + ylab(expression(paste('Richness [no/ m'^2,']'))) + geom_line(size = 1.5, color="black") + theme_bw() + facet_wrap(~site) + theme(legend.position="none", text = element_text(size=20), strip.background=element_blank(), strip.text=element_text(size=20) 
		) 

richness.cit <- ggplot(rich.cit, aes(year, richness, color = site)) + expand_limits(y=c(0,8)) + scale_x_continuous(breaks=c(1990, 2000, 2010)) + xlab("Year") + ylab(expression(paste('Richness [no/ m'^2,']'))) + geom_line(size = 1.5, color="black") + theme_bw() + facet_wrap(~site) + theme(legend.position="none", text = element_text(size=20), strip.background=element_blank(), strip.text=element_text(size=20), axis.title.y=element_blank()
		) 
	


turnover.het <- ggplot(gather(turn.het, metric, value, appearance:disappearance), aes(year, value, color = metric)) + expand_limits(y=c(-1,1)) + scale_x_continuous(breaks=c(1990, 2000, 2010)) + xlab("Year") + ylab("Turnover") + geom_line(size = 1.5) + theme_bw() + facet_wrap(~site) + theme(legend.position="none", text = element_text(size=20), strip.background=element_blank(), strip.text=element_blank()
		)

turnover.cit <- ggplot(gather(turn.cit, metric, value, appearance:disappearance), aes(year, value, color = metric)) + expand_limits(y=c(-1,1)) + scale_x_continuous(breaks=c(1990, 2000, 2010)) + xlab("Year") + ylab("Turnover") + geom_line(size = 1.5) + theme_bw() + facet_wrap(~site) + theme(legend.position="none", text = element_text(size=20), strip.background=element_blank(), strip.text=element_blank(), axis.title.y=element_blank()
		)



MRS.het <- ggplot(rodent.mrank.het, aes(year, MRS, color = site)) + expand_limits(y=c(0,2)) + scale_x_continuous(breaks=c(1990, 2000, 2010)) + xlab("Year") + ylab("Mean rank shift") + geom_line(size = 1.5, color="black") + theme_bw() + facet_wrap(~site) + theme(legend.position="none", text = element_text(size=20), strip.background=element_blank(), strip.text=element_blank())

MRS.cit <- ggplot(rodent.mrank.cit, aes(year, MRS, color = site)) + expand_limits(y=c(0,2)) + scale_x_continuous(breaks=c(1990, 2000, 2010)) + xlab("Year") + ylab("Mean rank shift") + geom_line(size = 1.5, color="black") + theme_bw() + facet_wrap(~site) + theme(legend.position="none", text = element_text(size=20), strip.background=element_blank(), strip.text=element_blank(), axis.title.y=element_blank())



rate_change.het <- ggplot(intervals.het, aes(interval, distance, color = site)) + expand_limits(y=c(0,8)) + xlab("Time interval") + ylab("Euclidean \ndistance") + geom_point(size=5, color="black") + geom_smooth(method = "lm", se=FALSE, color="#56B4E9", size=2) + theme_bw() + facet_wrap(~site) + theme(legend.position="none", text = element_text(size=20), strip.background=element_blank(), strip.text=element_blank()
		)


rate_change.cit <- ggplot(intervals.cit, aes(interval, distance, color = site)) + expand_limits(y=c(0,8)) + xlab("Time interval") + geom_point(size=5, color="black") + geom_smooth(method = "lm", se=FALSE, color="#56B4E9", size=2) + theme_bw() + facet_wrap(~site) + theme(legend.position="none", text = element_text(size=20), strip.background=element_blank(), strip.text=element_blank(), axis.title.y=element_blank()
		)


#multiplot(richness, turnover2, MRS, rate_change, cols=1)

topgrobs <- arrangeGrob(richness.het,richness.cit, turnover.het,turnover.cit, MRS.het,MRS.cit, rate_change.het,rate_change.cit, nrow=4)
grid.arrange(topgrobs, ncol=1, top=textGrob("Heteromyid                                     Cricetid", gp=gpar(fontsize=30,font=2)))





#######################
###Community Stability
######################
div.long.stb <- aggregate(density ~ species*year*site*web*feed_type,
	data = rodent.long,
	FUN = mean)

div.long.stb$group_var <- paste(div.long.stb$site, div.long.stb$feed_type, div.long.stb$web, sep="-")

head(div.long.stb)


###Community Stability
comm_stb <- community_stability(df=div.long.stb, abundance.var="density", replicate.var="group_var")
comm_stb$ID <- substring(comm_stb$group_var, 1, 5)

gv <- c(comm_stb[,1])

###Variance Ratio
var <- variance_ratio(df=div.long.stb, abundance.var="density", replicate.var="group_var", bootnumber=100, average.replicates=FALSE)


###Synchrony "Loreau"
syn <- synchrony(df=div.long.stb, abundance.var="density", replicate.var="group_var", metric= "Loreau")

syn[is.na(syn)] <- 0

syn <- subset(syn, group_var %in% gv)
syn$ID <- substring(syn$group_var, 1, 5)


 
###plotting
require(plotrix)
names <- c("C-C-1", "C-C-3", "C-C-5", "C-H-1", "C-H-3", "C-H-5", "G-C-1", "G-C-2", "G-C-3", "G-C-4", "G-C-5", "G-H-1", "G-H-2", "G-H-3", "G-H-4", "G-H-5") 

arrange(comm_stb, group_var)
arrange(syn, group_var)

stb <- cbind(comm_stb, syn=syn[,2])

cc <- subset(stb, ID == "C-cri")
gc <- subset(stb, ID == "G-cri")
ch <- subset(stb, ID == "C-het")
gh <- subset(stb, ID == "G-het")

lm(cc$stability ~ cc$syn)
lm(gc$stability ~ gc$syn)
lm(ch$stability ~ ch$syn)
lm(gh$stability ~ gh$syn)

###
par(mfrow = c(1, 1))
plot(stb$syn, stb$stability, type="n", pch=20, cex=2, 
	xlim=c(0,1.5), ylim=c(1,2.5),
	xlab="Synchrony", ylab="Community stability")

points(subset(stb, ID == "C-cri")[,c(4,2)], cex = 1.2, pch=16, col="dodgerblue1")
points(subset(stb, ID == "G-cri")[,c(4,2)], cex = 1.2, pch=16, col="dodgerblue4")
points(subset(stb, ID == "C-het")[,c(4,2)], cex = 1.2, pch=16, col="firebrick1")
points(subset(stb, ID == "G-het")[,c(4,2)], cex = 1.2, pch=16, col="firebrick4")

#textxy(syn$syn, comm_stb$stability, labs=names, cex = 1, cx = 1, dcol = "black", m = c(0, 0))

ablineclip(a=1.9, b=-3, x1=0, x2=1, lwd=3.0, col="dodgerblue1")
ablineclip(a=2, b=-1.8, x1=0, x2=1, lwd=3.0, col="dodgerblue4")
ablineclip(a=-1.3, b=4.7, x1=0, x2=1, lwd=3.0, col="firebrick1")
ablineclip(a=3, b=-2.8, x1=0, x2=1, lwd=3.0, col="firebrick4")


#ablineclip(a=1.6, b=-0.1, x1=0, x2=1, lwd=3.0)


leg.text <- 	c("cricetid creosote", "cricetid black grama", "heteromyid creosote", "heteromyid black grama")
leg.col <- c("dodgerblue1", "dodgerblue4", "firebrick1", "firebrick4")
legend("topright", cex = 1.0, leg.text, col=leg.col, pch=16, bty = "n")




