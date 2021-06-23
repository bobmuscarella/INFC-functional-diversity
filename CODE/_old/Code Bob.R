#packages
library(plyr)
library(ggplot2)
library(readxl)



TraitDataFrame <- read_excel("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Collaboration/raw_data/TraitDataFrame.xlsx", 
                             sheet = "AllTraits", col_types = c("skip", 
                                                                "skip", "skip", "skip", "skip", "skip", 
                                                                "skip", "text", "text"), na = "na")


TraitDataFrame <- na.omit(TraitDataFrame)

TraitDataFrame$VesselDiameter[TraitDataFrame$VesselDiameter == '0-20'] <- '20'
TraitDataFrame$VesselDiameter[TraitDataFrame$VesselDiameter == '20-50'] <- '50'
TraitDataFrame$VesselDiameter[TraitDataFrame$VesselDiameter == '50-100'] <- '100'
TraitDataFrame$VesselDiameter[TraitDataFrame$VesselDiameter == '100-200'] <- '200'

TraitDataFrame$VesselDensity[TraitDataFrame$VesselDensity == '0-100'] <- '100'
TraitDataFrame$VesselDensity[TraitDataFrame$VesselDensity == '100-200'] <- '200'
TraitDataFrame$VesselDensity[TraitDataFrame$VesselDensity == '200-300'] <- '300'
TraitDataFrame$VesselDensity[TraitDataFrame$VesselDensity == '500-600'] <- '600'
TraitDataFrame$VesselDensity[TraitDataFrame$VesselDensity == '1500-2000'] <- '1000'
TraitDataFrame$VesselDensity[TraitDataFrame$VesselDensity == '2000-2500'] <- '1000'
TraitDataFrame$VesselDensity[TraitDataFrame$VesselDensity == '3500-4000'] <- '1000'


table(TraitDataFrame)


TraitDataFrame$VesselDiameter <- as.numeric(as.character(TraitDataFrame$VesselDiameter))
TraitDataFrame$VesselDensity <- as.numeric(as.character(TraitDataFrame$VesselDensity))
                                           

# Get the frequency counts
dfc <- ddply(TraitDataFrame, c("VesselDiameter", "VesselDensity"), "nrow", .drop = FALSE)

ggplot(data = dfc, aes(x = VesselDiameter, y = VesselDensity, size = factor(nrow))) + 
  geom_point() + 
  scale_size_discrete(range = c(1, 10))


### plotting all the data points (Could add a boxplot)

ggplot(data = TraitDataFrame, aes(x = VesselDiameter, y = VesselDensity, color = VesselDiameter))+
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm')

