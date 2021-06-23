#packages
library(plyr)
library(ggplot2)
library(readxl)



tdf <- read_excel("/Users/au529793/Projects/GIT/INFC-functional-diversity/DATA/TraitDataFrame.xlsx", 
                             sheet = "AllTraits", col_types = c("text", 
                                                                "text", 
                                                                "numeric", 
                                                                "numeric", 
                                                                "numeric", 
                                                                "numeric", 
                                                                "numeric", 
                                                                "text", 
                                                                "text"), na = "na")

tdf <- na.omit(tdf)

tdf$VesselDiameter[tdf$VesselDiameter == '0-20'] <- '20'
tdf$VesselDiameter[tdf$VesselDiameter == '20-50'] <- '50'
tdf$VesselDiameter[tdf$VesselDiameter == '50-100'] <- '100'
tdf$VesselDiameter[tdf$VesselDiameter == '100-200'] <- '200'

tdf$VesselDensity[tdf$VesselDensity == '0-100'] <- '100'
tdf$VesselDensity[tdf$VesselDensity == '100-200'] <- '200'
tdf$VesselDensity[tdf$VesselDensity == '200-300'] <- '300'
tdf$VesselDensity[tdf$VesselDensity == '500-600'] <- '600'
tdf$VesselDensity[tdf$VesselDensity == '1500-2000'] <- '1000'
tdf$VesselDensity[tdf$VesselDensity == '2000-2500'] <- '1000'
tdf$VesselDensity[tdf$VesselDensity == '3500-4000'] <- '1000'

tdf$VesselDiameter <- as.numeric(as.character(tdf$VesselDiameter))
tdf$VesselDensity <- as.numeric(as.character(tdf$VesselDensity))



plot(tdf$VesselDensity, tdf$VesselDiameter)

par(mfrow=c(2,5), mar=c(4,4,1,1), oma=c(3,1,3,0))
boxplot(log(tdf$SeedMass) ~ tdf$VesselDensity)
boxplot(tdf$SLA ~ tdf$VesselDensity)
boxplot(tdf$StemDensity ~ tdf$VesselDensity)
boxplot(tdf$Height ~ tdf$VesselDensity)
boxplot(tdf$XylemVulnerability ~ tdf$VesselDensity)

boxplot(log(tdf$SeedMass) ~ tdf$VesselDiameter)
boxplot(tdf$SLA ~ tdf$VesselDiameter)
boxplot(tdf$StemDensity ~ tdf$VesselDiameter)
boxplot(tdf$Height ~ tdf$VesselDiameter)
boxplot(tdf$XylemVulnerability ~ tdf$VesselDiameter)

par(mfrow=c(1,1), oma=c(0,4,4,1))
r <- psych::corr.test(tdf[,-c(1:2)])$r
p <- psych::corr.test(tdf[,-c(1:2)])$p
corrplot::corrplot(r, p.mat=p, type='upper', 
                   diag=F, method='ellipse', outline=T,
                   addCoef.col=1, tl.col=1, tl.cex=1,
                   number.font = 1,
                   mar=c(4,4,4,4),
                   tl.srt=90, insig = 'label_sig', pch.cex = 0.75)

plot(tdf[,-c(1,2)])


