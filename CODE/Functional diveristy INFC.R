### Creating complete dataset

### Load needed packages
library(raster)
library(readr)
library(tidyverse)
library(FD)
library(readxl)
library(data.table)
library(ggplot2)
library(ggpubr)
library(dplyr)

##################################################
###########Mean annual precipation (mm)###########
##################################################

### create Raster layer object
January <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_01.tif")
February <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_02.tif")
March <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_03.tif")
April <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_04.tif")
May <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_05.tif")
June <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_06.tif")
July <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_07.tif")
August <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_08.tif")
September <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_09.tif")
October <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_10.tif")
November <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_11.tif")
December <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Precipation data/wc2.1_30s_prec_12.tif")

#### create coordinate vector
Plotdata <- read_delim("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/INFC/INFC-functional-diversity-master/DATA/RAW/infc05_quantiF3/t1_05_quantiF3.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
Coordinates <- data.frame(Plotdata$LON_ND_W84, Plotdata$LAT_ND_W84)
Coordinates1 <- data.frame(Plotdata$LON_ND_W84, Plotdata$LAT_ND_W84)
CoordinatesB <- data.frame(Plotdata$idpunto, Plotdata$LAT_ND_W84, Plotdata$LON_ND_W84)


### extract Precipation
Coordinates$PrecipationJanuary <- raster::extract(January, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                  fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$PrecipationFebruary <- raster::extract(February, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                   fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$PrecipationMarch <- raster::extract(March, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$PrecipationApril <- raster::extract(April, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$PrecipationMay <- raster::extract(May, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                              fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$PrecipationJune <- raster::extract(June, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                               fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$PrecipationJuly <- raster::extract(July, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                               fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$PrecipationAugustus <- raster::extract(August, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                   fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$PrecipationSeptember <- raster::extract(September, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                    fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$PrecipationOctober <- raster::extract(October, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                  fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$PrecipationNovember <- raster::extract(November, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                   fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$PrecipationDecember <- raster::extract(December, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                   fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)

### match the coordinate vector with the IDpunto of the INFC dataset + calculate Mean annual precipation
Coordinates$x <- paste(Coordinates$Plotdata.LAT_ND_W84, Coordinates$Plotdata.LON_ND_W84)
CoordinatesB$x <- paste(Coordinates1$Plotdata.LAT_ND_W84, Coordinates1$Plotdata.LON_ND_W84)
Precipation <- merge(Coordinates, CoordinatesB, by.x = "x", by.y = "x")
Precipation <- select(Precipation, Plotdata.idpunto, Plotdata.LON_ND_W84.x, Plotdata.LAT_ND_W84.x, PrecipationJanuary, PrecipationFebruary, PrecipationMarch, PrecipationApril, PrecipationMay, PrecipationJune, PrecipationJuly, PrecipationAugustus, PrecipationSeptember, PrecipationOctober, PrecipationNovember, PrecipationDecember)
Precipation$MeanAnnualPrecipation <- rowSums(Precipation[,4:15])
summary(Precipation)


##################################################
###########Mean annual temperature (°C)###########
##################################################

### create Raster layer object
JanuaryT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_01.tif")
FebruaryT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_02.tif")
MarchT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_03.tif")
AprilT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_04.tif")
MayT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_05.tif")
JuneT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_06.tif")
JulyT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_07.tif")
AugustT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_08.tif")
SeptemberT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_09.tif")
OctoberT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_10.tif")
NovemberT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_11.tif")
DecemberT <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Temperature data/wc2.1_30s_tavg_12.tif")


#### create coordinate vector
Coordinates <- data.frame(Plotdata$LON_ND_W84, Plotdata$LAT_ND_W84)
Coordinates1 <- data.frame(Plotdata$LON_ND_W84, Plotdata$LAT_ND_W84)
CoordinatesB <- data.frame(Plotdata$idpunto, Plotdata$LAT_ND_W84, Plotdata$LON_ND_W84)

### extract Precipation
Coordinates$TemperatureJanuary <- raster::extract(JanuaryT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                  fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$TemperatureFebruary <- raster::extract(FebruaryT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                   fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$TemperatureMarch <- raster::extract(MarchT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$TemperatureApril <- raster::extract(AprilT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$TemperatureMay <- raster::extract(MayT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                              fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$TemperatureJune <- raster::extract(JuneT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                               fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$TemperatureJuly <- raster::extract(JulyT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                               fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$TemperatureAugustus <- raster::extract(AugustT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                   fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$TemperatureSeptember <- raster::extract(SeptemberT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                    fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$TemperatureOctober <- raster::extract(OctoberT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                  fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$TemperatureNovember <- raster::extract(NovemberT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                   fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
Coordinates$TemperatureDecember <- raster::extract(DecemberT, Coordinates1, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                   fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)

### match the coordinate vector with the other dataset + calculate Mean annual temperature
Coordinates$x <- paste(Coordinates$Plotdata.LAT_ND_W84, Coordinates$Plotdata.LON_ND_W84)
CoordinatesB$x <- paste(Coordinates1$Plotdata.LAT_ND_W84, Coordinates1$Plotdata.LON_ND_W84)
Temperature <- merge(Coordinates, CoordinatesB, by.x = "x", by.y = "x")
Temperature <- select(Temperature, Plotdata.idpunto, Plotdata.LON_ND_W84.x, Plotdata.LAT_ND_W84.x, TemperatureJanuary, TemperatureFebruary, TemperatureMarch, TemperatureApril, TemperatureMay, TemperatureJune, TemperatureJuly, TemperatureAugustus, TemperatureSeptember, TemperatureOctober, TemperatureNovember, TemperatureDecember)
Temperature$MeanAnnualTemperature <- (rowSums(Temperature[,4:15])/12)
summary(Temperature$MeanAnnualTemperature)



######################################
########### Soil variables ###########
######################################

### create Raster layer object
Soil <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Soil data/GISfiles/wise30sec_fin/w001000.adf")
Soil

#### create coordinate vector
Coordinates <- data.frame(Plotdata$LON_ND_W84, Plotdata$LAT_ND_W84)
CoordinatesB <- data.frame(Plotdata$idpunto, Plotdata$LAT_ND_W84, Plotdata$LON_ND_W84)

### extract Soil Data
Coordinates$Soil <- raster::extract(Soil, Coordinates, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                    fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)

### Access attributes from a raster layer to through levels fucntion
### link: https://gis.stackexchange.com/questions/258960/access-to-attribute-in-rasterlayer-in-r

IDSoilData <- data.frame(raster::levels(Soil))

### match ID and NEWSUID that is used in the soil data

SoilCharacterstics <- read.csv("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Soil data/Interchangeable_format/HW30s_FULL.txt")

IDItalianPlots <- data.frame(table(Coordinates$Soil))
names(IDItalianPlots)[1] <- "ID"
IDItalianPlots$NEWSUID <-  IDSoilData$NEWSUID[match(IDItalianPlots$ID, IDSoilData$ID)]
IDItalianPlots <- IDItalianPlots[-c(1), -c(2)]

### Extract soil characteristics
# link: https://stackoverflow.com/questions/29979851/r-compare-two-data-frames-of-different-length-for-same-values-in-two-columns

SubsetSoilData <- merge(SoilCharacterstics, IDItalianPlots, by = c("NEWSUID"), all = FALSE)
SubsetSoilData <- SubsetSoilData[, c(1,8,22,23,24,25,28,29,52)]

### Looks like soil data points that have no values get a negative datapoint

SubsetSoilData[SubsetSoilData < 0] <- NA
FinalItalianSoil <- na.omit(SubsetSoilData)
ORGC <- aggregate(FinalItalianSoil$ORGC, list(FinalItalianSoil$NEWSUID), mean)
names(ORGC)[1] <- "NEWSUID"
names(ORGC)[2] <- "ORGC"
### WD30009854 is an outlier 

WC <- aggregate(FinalItalianSoil$TAWC, list(FinalItalianSoil$NEWSUID), mean)
names(WC)[1] <- "NEWSUID"
names(WC)[2] <- "WC"

CNRatio <- aggregate(FinalItalianSoil$CNrt, list(FinalItalianSoil$NEWSUID), mean)
names(CNRatio)[1] <- "NEWSUID"
names(CNRatio)[2] <- "CNRatio"

IDItalianPlots$ORGC <- ORGC$ORGC[match(IDItalianPlots$NEWSUID, ORGC$NEWSUID)]
IDItalianPlots$WC <- WC$WC[match(IDItalianPlots$NEWSUID, WC$NEWSUID)]
IDItalianPlots$CNRatio <- CNRatio$CNRatio[match(IDItalianPlots$NEWSUID, CNRatio$NEWSUID)]

### match soil data with plot coordinates
Coordinates$ORGC <- IDItalianPlots$ORGC[match(Coordinates$Soil, IDItalianPlots$ID)]
Coordinates$WC <- IDItalianPlots$WC[match(Coordinates$Soil, IDItalianPlots$ID)]
Coordinates$CNRatio <- IDItalianPlots$CNRatio[match(Coordinates$Soil, IDItalianPlots$ID)]

### Merge datasets
Coordinates$x <- paste(Coordinates$Plotdata.LAT_ND_W84, Coordinates$Plotdata.LON_ND_W84)
CoordinatesB$x <- paste(CoordinatesB$Plotdata.LAT_ND_W84, CoordinatesB$Plotdata.LON_ND_W84)
Soil <- merge(Coordinates, CoordinatesB, by.x = "x", by.y = "x")
Soil <- select(Soil, Plotdata.LAT_ND_W84.x, Plotdata.LON_ND_W84.x, ORGC, WC, CNRatio)  


##############################################
########### Climate classification ###########
##############################################

#### Set working directory
setwd("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Climate classification")
getwd()

### Load rasterlayer
load("ClimateB.Rdata")
ClimateB

#### create coordinate vector
Plotdata <- read_delim("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/INFC/INFC-functional-diversity-master/DATA/RAW/infc05_quantiF3/t1_05_quantiF3.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
Coordinates <- data.frame(Plotdata$LON_ND_W84, Plotdata$LAT_ND_W84)
CoordinatesB <- data.frame(Plotdata$idpunto, Plotdata$LAT_ND_W84, Plotdata$LON_ND_W84)

### extract Climate classification
Coordinates$ClimateClassification <- raster::extract(ClimateB, Coordinates, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                                                     fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)

### Merge datasets
Coordinates$x <- paste(Coordinates$Plotdata.LAT_ND_W84, Coordinates$Plotdata.LON_ND_W84)
CoordinatesB$x <- paste(CoordinatesB$Plotdata.LAT_ND_W84, CoordinatesB$Plotdata.LON_ND_W84)
ClimateClassification <- merge(Coordinates, CoordinatesB, by.x = "x", by.y = "x")
ClimateClassification<- select(ClimateClassification, Plotdata.idpunto, Plotdata.LAT_ND_W84.x, Plotdata.LON_ND_W84.x, ClimateClassification) 


#########################################
########### Plot productivity ###########
#########################################

PlotProductivity<- read_delim("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/INFC/INFC-functional-diversity-master/DATA/RAW/infc05_quantiF3/t1_05_quantiF3.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

PlotProductivity <- select(PlotProductivity, idpunto, LAT_ND_W84, LON_ND_W84, W4apv_ha, ICVapv_ha)


##################################
########### Trait data ###########
##################################

### Load data
WOODIV_Trait_data <- read.csv("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Traitdata/WOODIV_DB_release_v1/TRAITS/WOODIV_Trait_data.csv")
WOODIV_Species_code <- read.csv("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Traitdata/WOODIV_DB_release_v1/SPECIES/WOODIV_Species_code.csv")

### match species code
WOODIV_Trait_data$SpeciesCode <- WOODIV_Species_code$genus_species_subspecies[match(WOODIV_Trait_data$spcode, WOODIV_Species_code$spcode)]

### Split dataset up in the 4 traits
table(WOODIV_Trait_data$Traits)
Height <-  subset(WOODIV_Trait_data, Traits == "Height")
SeedMass <-subset(WOODIV_Trait_data, Traits == "SeedMass")
SLA <-subset(WOODIV_Trait_data, Traits == "SLA")
StemSpecDens <-subset(WOODIV_Trait_data, Traits == "StemSpecDens")

# Calculate mean
Heightmean <- aggregate(x= Height$value, by = list(Height$SpeciesCode), FUN = mean)
SeedMassmean <- aggregate(x= SeedMass$value, by = list(SeedMass$SpeciesCode), FUN = mean)
SLAmean <- aggregate(x= SLA$value, by = list(SLA$SpeciesCode), FUN = mean)
StemSpecDensmean <- aggregate(x= StemSpecDens$value, by = list(StemSpecDens$SpeciesCode), FUN = mean)

### Change column heading
names(Heightmean)[1] <- "Species"
names(Heightmean)[2] <- "Value"
names(SeedMassmean)[1] <- "Species"
names(SeedMassmean)[2] <- "Value"
names(SLAmean)[1] <- "Species"
names(SLAmean)[2] <- "Value"
names(StemSpecDensmean)[1] <- "Species"
names(StemSpecDensmean)[2] <- "Value"

### Match trait information together with species codes of INFC
library(readxl)
Species <-  read_excel("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Species code (trait frame).xlsx")

Species$Height <- Heightmean$Value[match(Species$`Species name`, Heightmean$Species)]
Species$SLA <- SLAmean$Value[match(Species$`Species name`, SLAmean$Species)]
Species$Seedmass <- SeedMassmean$Value[match(Species$`Species name`, SeedMassmean$Species)]
Species$StemDensity <- StemSpecDensmean$Value[match(Species$`Species name`, StemSpecDensmean$Species)]

### Add extra Stemdensity and SLA values to finnish the dataset

Species$StemDensity[Species$`Species name` == "Abies_cephalonica"] <- 0.595006
Species$SLA[Species$`Species name` == "Abies_cephalonica"] <- 5.820722

Species$StemDensity[Species$`Species name` == "Pinus_mugo"] <- 0.49

Species$StemDensity[Species$`Species name` == "Carpinus_orientalis"] <- 0.7

Species$StemDensity[Species$`Species name` == "Quercus_frainetto"] <- 0.78

Species$StemDensity[Species$`Species name` == "Quercus_trojana"] <- 0.549


### Take out unwanted columns + NA values
Species <- Species[, -c(2,3)]
Species <- na.omit(Species)


####################################################
########### Functional diversity indices ###########
####################################################

### Compute abundance matrix with species as columns and rows as sites

AliveTrees <- read_delim("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/INFC/INFC-functional-diversity-master/DATA/RAW/infc05_apv/t2_05_apv.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)


### Check if Species codes overlap
Match <- match(AliveTrees$SPcod, Species$`Species Code`, nomatch = NA)
summary(Match)
AliveTrees$Match <- match(AliveTrees$SPcod, Species$`Species Code`, nomatch = 0)
AliveTreesSubset <- subset(AliveTrees, Match > 0)

### Create abundance matrix
AmountSpecies <- table(AliveTreesSubset$idpunto, AliveTreesSubset$SPcod)
SpeciesAmount <- as.data.frame.matrix(AmountSpecies) ## change table to dataframe
colnames(SpeciesAmount) <- as.character(Species$`Species Code`)
SpeciesAmount <- as.matrix(SpeciesAmount)

###  The number of species (columns) in a must match the number of species (rows) in x
Species2 <- Species
Species <- Species[, -c(1)]
rownames(Species) <- Species2$`Species Code`
colnames(SpeciesAmount) <- as.character(Species2$`Species Code`)


###  Functional indices
DBFDWeighted <- FD::dbFD(Species, SpeciesAmount, w.abun = TRUE)
DBFDUnweighted <- FD::dbFD(Species, SpeciesAmount, w.abun = FALSE)
Frich <- as.data.frame(DBFDUnweighted$FRic)
FdispU <- as.data.frame(DBFDUnweighted$FDis)
FdispW <- as.data.frame(DBFDWeighted$FDis)
RaoU<- as.data.frame(DBFDUnweighted$RaoQ)
RaoW <- as.data.frame(DBFDWeighted$RaoQ)
CWMU <- as.data.frame(DBFDUnweighted$CWM)
CWMW <- as.data.frame(DBFDWeighted$CWM)

setDT(Frich, keep.rownames = TRUE)
names(Frich)[1] <- "IDPlot"
setDT(FdispU, keep.rownames = TRUE)
names(FdispU)[1] <- "IDPlot"
setDT(FdispW, keep.rownames = TRUE)
names(FdispW)[1] <- "IDPlot"
setDT(RaoU, keep.rownames = TRUE)
names(RaoU)[1] <- "IDPlot"
setDT(RaoW, keep.rownames = TRUE)
names(RaoW)[1] <- "IDPlot"
setDT(CWMU, keep.rownames = TRUE)
names(CWMU)[1] <- "IDPlot"
setDT(CWMW, keep.rownames = TRUE)
names(CWMW)[1] <- "IDPlot"



##############################################################
########### Final dataframe with all the variables ###########
##############################################################

### FunctionalDiversity data
CombinedDataset <- Frich
names(CombinedDataset)[2] <- "Frich"
CombinedDataset$FdispU <- FdispU$`DBFDUnweighted$FDis`[match(CombinedDataset$IDPlot, FdispU$IDPlot)]
CombinedDataset$FdispW <- FdispW$`DBFDWeighted$FDis`[match(CombinedDataset$IDPlot, FdispW$IDPlot)]
CombinedDataset$RaoU <- RaoU$`DBFDUnweighted$RaoQ`[match(CombinedDataset$IDPlot, RaoU$IDPlot)]
CombinedDataset$RaoW <- RaoW$`DBFDWeighted$RaoQ`[match(CombinedDataset$IDPlot, RaoW$IDPlot)]
CombinedDataset$CWMW_StemDensity <- CWMW$StemDensity[match(CombinedDataset$IDPlot, CWMW$IDPlot)]
CombinedDataset$CWMW_Seedmass <- CWMW$Seedmass[match(CombinedDataset$IDPlot, CWMW$IDPlot)]
CombinedDataset$CWMW_SLA <- CWMW$SLA[match(CombinedDataset$IDPlot, CWMW$IDPlot)]
CombinedDataset$CWMW_Height <- CWMW$Height[match(CombinedDataset$IDPlot, CWMW$IDPlot)]

CombinedDataset$CWMW_StemDensity <- as.numeric(as.character(CombinedDataset$CWMW_StemDensity))
CombinedDataset$CWMW_Seedmass <- as.numeric(as.character(CombinedDataset$CWMW_Seedmass))
CombinedDataset$CWMW_SLA <- as.numeric(as.character(CombinedDataset$CWMW_SLA))
CombinedDataset$CWMW_Height <- as.numeric(as.character(CombinedDataset$CWMW_Height))

### Environmental data
CombinedDataset$MeanAnnualPrecipation <- Precipation$MeanAnnualPrecipation[match(CombinedDataset$IDPlot, Precipation$Plotdata.idpunto)]
CombinedDataset$MeanAnnualTemperature <- Temperature$MeanAnnualTemperature[match(CombinedDataset$IDPlot, Temperature$Plotdata.idpunto)]

### Soil data
CoordinatesB <- data.frame(Plotdata$idpunto, Plotdata$LAT_ND_W84, Plotdata$LON_ND_W84)
Soil$x <- paste(Soil$Plotdata.LAT_ND_W84, Soil$Plotdata.LON_ND_W84)
CoordinatesB$x <- paste(CoordinatesB$Plotdata.LAT_ND_W84, CoordinatesB$Plotdata.LON_ND_W84)
SoilB <- merge(Soil, CoordinatesB, by.x = "x", by.y = "x")
SoilB <- select(SoilB, Plotdata.idpunto, ORGC, WC, CNRatio)

CombinedDataset$ORGC <- SoilB$ORGC[match(CombinedDataset$IDPlot, SoilB$Plotdata.idpunto)]
CombinedDataset$WC <- SoilB$WC[match(CombinedDataset$IDPlot, SoilB$Plotdata.idpunto)]
CombinedDataset$CNRatio <- SoilB$CNRatio[match(CombinedDataset$IDPlot, SoilB$Plotdata.idpunto)]

### Climate classification
CombinedDataset$ClimateClassification <- ClimateClassification$ClimateClassification[match(CombinedDataset$IDPlot, ClimateClassification$Plotdata.idpunto)]

### Plot productivity
CombinedDataset$AnnualIncrement_hectare <- PlotProductivity$ICVapv_ha[match(CombinedDataset$IDPlot, PlotProductivity$idpunto)] ### 565 plots have NA values
CombinedDataset$DryWeightAboveGroundBiomass_hectare <- PlotProductivity$W4apv_ha[match(CombinedDataset$IDPlot, PlotProductivity$idpunto)]

##### Delete the unwanted INFC forest plot codes

CombinedDataset$Forestcode <- Plotdata$codcfor[match(CombinedDataset$IDPlot, Plotdata$idpunto)]
CombinedDataset<- filter(CombinedDataset, Forestcode < 17)

summary(CombinedDataset)
# Precipation -> 20 NA
# Temperature -> 20 NA
# Climate -> 19 NA and not every value is 0 or 1
# Increment -> 565 NA
# ORGC/WC/CNRatio -> 43 NA
# delete Functional richness column due to NA values
CombinedDataset <- CombinedDataset[, -c(2)]
Combinedataset_NA <- na.omit(CombinedDataset)
## Total loose 601 plots
### Take out ID plot 124816 (Outlier)
Combinedataset_NA <- subset(Combinedataset_NA, ORGC < 175)


