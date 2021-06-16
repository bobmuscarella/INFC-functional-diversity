##%######################################################%##
#                                                          #
####                  Load R packages                   ####
#                                                          #
##%######################################################%##
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c('raster','sp','readxl', 'ncdf4', 'readr', 'writexl','dplyr', 'data,table')
ipak(packages)

##%######################################################%##
#                                                          #
####               Create working folders               ####
#                                                          #
##%######################################################%##
setwd("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Collaboration")
folder_names <-
  c("raw_data", "output_data", "docs", "scripts", "output_plot")
for (i in folder_names) {
  ifelse(!dir.exists(i),
         dir.create(i),
         print("Folder exists already"))
}

############################################################
#                                                          #
#                      Load dataset                        #
#                                                          #
############################################################
quantiF3 = read_delim(
  "C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/INFC/INFC-functional-diversity-master/DATA/RAW/infc05_quantiF3/t1_05_quantiF3.csv",
  ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
setProj<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
############################################################
#                                                          #
#               make spatialpointdataframe               #
#                                                          #
############################################################
xy.dt<- quantiF3[,3:2]
quanti.sp<- SpatialPointsDataFrame(coords = xy.dt, 
                                   data = quantiF3,
                                   proj4string = CRS(setProj))

##%######################################################%##
#                                                          #
####             1/ ADMINISTRATIVE BORDERS              ####
#                                                          #
##%######################################################%##
Italy0<-raster::getData('GADM', country='ITA', level=0, 
                        path = './raw_data')
ita_boundary <- spTransform(Italy0, CRS(setProj))
extent(ita_boundary)
rm(Italy0)
plot(ita_boundary)
plot(quanti.sp, add=T, col='red', pch=16, cex=.3)

##%######################################################%##
#                                                          #
####                     Load TOPO                      ####
#                                                          #
##%######################################################%##
## Get SRTM data (90m)
ita_alt<-raster::getData('alt',country="ITA", 
                         path = './raw_data')
projection(ita_alt)
# Make Aspect and Slope variables
ita_slp <- terrain(ita_alt, opt=c('slope'), unit='degrees')
ita_asp <- terrain(ita_alt, opt=c('aspect'), unit='degrees')

##%######################################################%##
#                                                          #
####                     extraction                     ####
#                                                          #
##%######################################################%##
quanti.sp@data$elev<-raster::extract(ita_alt, quanti.sp)
quanti.sp@data$slp<-raster::extract(ita_slp, quanti.sp)
quanti.sp@data$asp<-raster::extract(ita_asp, quanti.sp)

############################################################
#                                                          #
#            Load Monthly VPD + Soil Mositure              #
#                                                          #
############################################################
path <- paste0("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Collaboration/raw_data/TerraClimate/") 
list.nc <- list.files(path,pattern=".nc")

system.time(
  for (i in 1:length(list.nc)){
    tmp.stack <- stack(paste0(path,list.nc[i]))
    quanti.sp@data<-cbind(quanti.sp@data,raster::extract(
      tmp.stack,quanti.sp))
  }
)

# Calculate MeanAnnualVPD
quanti.sp@data$MeanAnnualVPD <- rowMeans(quanti.sp@data[, 66:77])
quanti.sp@data$MeanAnnualSoilMoisture <- rowMeans(quanti.sp@data[,54:55])
rm(tmp.stack,path);gc()

##%######################################################%##
#                                                          #
####              Climate classification                ####
#                                                          #
##%######################################################%##
mb <- raster::raster("/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Climate classification/mb/hdr.adf")
summary(mb)
mb
plot(mb)
# Being incompatible projections
quanti.sp <- spTransform(quanti.sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))
quanti.sp@data<-cbind(quanti.sp@data,ClimateClassification=raster::extract(
  mb,quanti.sp))
quanti.sp <- spTransform(quanti.sp, CRS(setProj)) # reproject to the original proj

unique(quanti.sp@data$ClimateClassification)
# Macrobioclimate
# 0: temperate
# 1: mediterranean
quanti.sp@data$Macrobioclimate <-
  ifelse(
    quanti.sp@data$ClimateClassification == 0, "temperate",
    ifelse(quanti.sp@data$ClimateClassification == 1, "mediterranean", NA)
  )
##%######################################################%##
#                                                          #
####     Extract climate + productivity data            ####
#                                                          #
##%######################################################%##

#df<-quanti.sp@data
#write_xlsx(df,"C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Collaboration/output_data/PlotData.xlsx")

##%######################################################%##
#                                                          #
####                  Load Trait data                   ####
#                                                          #
##%######################################################%##

NoVesselTraits <- read_excel("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/Collaboration/raw_data/TraitDataFrame.xlsx", 
                             sheet = "AllTraits", col_types = c("numeric", 
                                                                "text", "numeric", "numeric", "numeric", "numeric", 
                                                                "numeric", "skip", "skip"), na = "na")
SubsetNoVesselTraits <- na.omit(NoVesselTraits)

NoVesselTraits_NA <- read_excel("raw_data/TraitDataFrame.xlsx", 
                             sheet = "5 traits + 1NA", col_types = c("numeric", 
                                                                     "text", "numeric", "numeric", "numeric", "numeric", 
                                                                     "numeric"), na = "na")

##%######################################################%##
#                                                          #
####              Load Alive trees data                 ####
#                                                          #
##%######################################################%##
AliveTrees1 <- read_delim("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/INFC/INFC-functional-diversity-master/DATA/RAW/infc05_apv/t2_05_apv.csv", 
                         ";", 
                         escape_double = FALSE, 
                         trim_ws = TRUE)

AliveTrees2 <- read_delim("C:/Users/roell/OneDrive/Bureaublad/INFC-functional-diversity/INFC/INFC-functional-diversity-master/DATA/RAW/infc05_apv/t2_05_apv.csv", 
                         ";", 
                         escape_double = FALSE, 
                         trim_ws = TRUE)
### Add forest codes
AliveTrees1$Forestcode <- quantiF3$codcfor[match(AliveTrees1$idpunto, quantiF3$idpunto)]
AliveTrees2$Forestcode <- quantiF3$codcfor[match(AliveTrees2$idpunto, quantiF3$idpunto)]

## Filter Unwanted Forest codes out
AliveTrees1<- filter(AliveTrees1, Forestcode < 17)
AliveTrees2<- filter(AliveTrees2, Forestcode < 17)

# Match SPcod of the trait dataset + take out unmatching SPcod
AliveTrees1$Match <- match(AliveTrees1$SPcod, SubsetNoVesselTraits$`Species Code`, nomatch = 0)
AliveTrees2$Match <- match(AliveTrees2$SPcod, NoVesselTraits_NA$`Species Code`, nomatch = 0)
AliveTreesSubset1 <- subset(AliveTrees1, Match > 0) 
AliveTreesSubset2 <- subset(AliveTrees2, Match > 0)

##%######################################################%##
#                                                          #
####           Species abundance matrix                 ####
#                                                          #
##%######################################################%##

### Abundance matrix 1
AmountSpecies1 <- table(AliveTreesSubset1$idpunto, AliveTreesSubset1$SPcod)
SpeciesAmount1 <- as.data.frame.matrix(AmountSpecies1) 
SpeciesAmount1 <- as.matrix(SpeciesAmount1)

### Abundance matrix 2
AmountSpecies2 <- table(AliveTreesSubset2$idpunto, AliveTreesSubset2$SPcod)
SpeciesAmount2 <- as.data.frame.matrix(AmountSpecies2) 
SpeciesAmount2 <- as.matrix(SpeciesAmount2)

##%######################################################%##
#                                                          #
####   Matching names abundance matrix and trait data   ####
#                                                          #
##%######################################################%##

#### Trait 1 
SubsetNoVesselTraits2 <- SubsetNoVesselTraits
SubsetNoVesselTraits <- SubsetNoVesselTraits[,-c(1,2)]
rownames(SubsetNoVesselTraits) <- SubsetNoVesselTraits2$`Species Code`
colnames(SpeciesAmount1) <- as.character(SubsetNoVesselTraits2$`Species Code`)

#### Trait 2
NoVesselTraits_NA2 <- NoVesselTraits_NA 
NoVesselTraits_NA <- NoVesselTraits_NA[,-c(1,2)]
rownames(NoVesselTraits_NA) <- NoVesselTraits_NA2$`Species Code`
colnames(SpeciesAmount2) <- as.character(NoVesselTraits_NA2$`Species Code`)

##%######################################################%##
#                                                          #
####                Functional indices                  ####
#                                                          #
##%######################################################%##

### Trait 1
DBFDWeighted1 <- FD::dbFD(SubsetNoVesselTraits, SpeciesAmount1, w.abun = TRUE)
FdispW1 <- as.data.frame(DBFDWeighted1$FDis)
CWM1 <- as.data.frame(DBFDWeighted1$CWM)

setDT(FdispW1, keep.rownames = TRUE)
names(FdispW1)[1] <- "IDPlot"
names(FdispW1)[2] <- "Fdisp"
setDT(CWM1, keep.rownames = TRUE)
names(CWM1)[1] <- "IDPlot"

### Trait2
DBFDWeighted2 <- FD::dbFD(NoVesselTraits_NA, SpeciesAmount2, w.abun = TRUE, corr = "cailliez")
FdispW2 <- as.data.frame(DBFDWeighted2$FDis)
CWM2 <- as.data.frame(DBFDWeighted2$CWM)

setDT(FdispW2, keep.rownames = TRUE)
names(FdispW2)[1] <- "IDPlot"
names(FdispW2)[2] <- "Fdisp"
setDT(CWM2, keep.rownames = TRUE)
names(CWM2)[1] <- "IDPlot"

##%######################################################%##
#                                                          #
####      Correlation  between trait data frames        ####
#                                                          #
##%######################################################%##

FdispW2$Match <- match(FdispW2$IDPlot, FdispW1$IDPlot, nomatch = 0)
FdispWSubset <- subset(FdispW2, Match > 0) 
cor.test(FdispWSubset$Fdisp,FdispW1$Fdisp)

###################################################################################################################
#---------- At the end of the extraction you can easily extract the dataframe from the spatialpointdaatframe ---###
df<-quanti.sp@data
class(df)
head(df)
