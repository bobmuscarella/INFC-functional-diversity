# load libraries
library("factoextra")
library("ggpubr")

# Load data
dataset <- df

##%######################################################%##
#                                                          #
####        Correlation between variables               ####
#                                                          #
##%######################################################%##

### Aboveground biomass and increments
dataset$ICCapv_ha_log <-
  scale(log(dataset$ICCapv_ha + (1 - min(dataset$ICCapv_ha, na.rm = T))), center = TRUE)
hist(dataset$ICCapv_ha_log)

dataset$Capv_ha_log <-
  scale(log(dataset$Capv_ha + (1 - min(dataset$Capv_ha, na.rm = T))), center = TRUE)
hist(dataset$Capv_ha_log)

cor.test(dataset$Capv_ha_log, dataset$ICCapv_ha_log)


#############################
#### Research question 1 ####
#############################

### Option 1: Create one environmental gradient and plot versus CWM/Fdis
VPD <- dataset$vpd
Soil_Moisture <- dataset$soilmoisture
ord <- prcomp( ~ VPD  +
                 Soil_Moisture,
               center = TRUE, scale = TRUE)
summary(ord)


### visualize the variables used in the PCA
A1 <-
  fviz_pca_var(
    ord,
    col.var = "contrib",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE # Avoid text overlapping)
  )

###  visualize results for individuals
B1 <- fviz_pca_ind(ord,
                   label = "none" # hide individual labels
                   )

### Put the plots together
PCAplot1 <- ggarrange(A1, B1, labels = c("A", "B"), ncol = 2, nrow = 1)
PCAplot1

### Coordinates for individuals and plotting it against soil moisture
res.ind <- get_pca_ind(ord)
Coord <- res.ind$coord
head(Coord)
# get dimensions
Dim1 <- Coord[,1]
Dim2 <- Coord[,2]

dataset$Dim1 <- Dim1
dataset$Dim2 <- Dim2

## Plotting relationships out
SLA.mod <- lm(cwm_SLA_log ~ Dim2, data=dataset)
summary(SLA.mod)
plot(cwm_SLA_log~Dim2, data=dataset, col='red', pch='.')
abline(SLA.mod)

Wooddensity.mod <- lm(cwm_StemDensity ~ Dim2, data = dataset)
summary(Wooddensity.mod)
plot(cwm_StemDensity~Dim2, data=dataset, col='red', pch='.')
abline(Wooddensity.mod)

Height.mod <- lm(cwm_Height_log ~ Dim2, data=dataset)
summary(Height.mod)
plot(cwm_Height_log~Dim2, data=dataset, col='red', pch='.')
abline(Height.mod)

SeedMass.mod <- lm(cwm_SeedMass_log ~ Dim2, data=dataset)
summary(SeedMass.mod)
plot(cwm_SeedMass_log~Dim2, data=dataset, col='red', pch='.')
abline(SeedMass.mod)

XylemVulnerability.mod <- lm(cwm_XylemVulnerability ~ Dim2, data=dataset)
summary(XylemVulnerability.mod)
plot(cwm_XylemVulnerability~Dim2, data=dataset, col='red', pch='.')
abline(XylemVulnerability.mod)

FunctionalDispersion.mod <- lm(FDis ~ Dim2, data=dataset) # Warning !! FDis in not in the dataframe; do you mean FDis_All ??
summary(FunctionalDispersion.mod)


# Visualize the the relationship between soil moisture and vdp
ggplot(data = dataset, aes(x = vpd, y = soilmoisture)) + geom_point(color='red') 

# Visualize dimension 1, to see where low soil moisture/vpd plots occur
pacman::p_load(ggplot2, sf, rnaturalearth, rnaturalearthdata, sp, rgeos)
theme_set(theme_bw()) ## sets background theme
Italy <- ne_countries(country = 'italy',scale = "medium", returnclass = "sf")
ggplot(data = Italy) + geom_sf() + geom_point(data = dataset, aes(LON_ND_W84, LAT_ND_W84, color = Dim1)) + labs(y = "", x = "") + scale_color_gradientn(colours = rainbow(5))


# AR: I stopped here with the coding..

### Option 2A: PCA species traits + CWM species position PCA 

# Read trait data
trait <- as.data.frame(readxl::read_excel("DATA/TraitDataFrame.xlsx", 
                                          sheet = "AllTraits", 
                                          col_types = c("text", 
                                                        "text", 
                                                        "numeric", 
                                                        "numeric", 
                                                        "numeric", 
                                                        "numeric", 
                                                        "numeric",
                                                        "numeric",
                                                        "numeric"), na = "na"))
# Remove vessel traits & species not in plots
trait <- trait[, -c(8:9)]
rownames(trait) <- trait$`Species Code`

# Log-transform some variables
trait$SeedMass_log <- log(trait$SeedMass)
trait$Height_log <- log(trait$Height)
trait$SLA_log <- log(trait$SLA)

#TAKE OUT NA VALUES FOR PCA
Trait_PCA <- na.omit(trait)

StemDensity <- Trait_PCA$StemDensity
SLA <- Trait_PCA$SLA_log
Height <- Trait_PCA$Height_log
SeedMass <- Trait_PCA$SeedMass_log
XylemVulnerability <- Trait_PCA$XylemVulnerability

ordB <- prcomp( ~ StemDensity  +
                  SLA +
                  Height +
                  SeedMass +
                  XylemVulnerability,
                center = TRUE, scale = TRUE)
summary(ordB)

### visualize the variables used in the PCA
A2 <- fviz_pca_var(ordB, col.var="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE # Avoid text overlapping)
)

###  visualize results for plots
B2 <- fviz_pca_ind(ordB,
                   label = "none", # hide individual labels
)


### Put the plots together
PCAplot2 <- ggarrange(A2, B2, labels = c("A", "B"), ncol = 2, nrow = 1)
PCAplot2

### Contribution of variables
tres.varB <- get_pca_var(ordB)

contrib2 <- tres.varB$contrib
contrib2

### Coordinates for individuals and plotting it against soil moisture
res.indB <- get_pca_ind(ordB)
CoordB <- res.indB$coord

Dim1 <- CoordB[,-c(2,3,4,5)]
Dim2 <- CoordB[,-c(1,3,4,5)]


Trait_PCA$Dim1 <- Dim1
Trait_PCA$Dim2 <- Dim2

############# Calculate CWM of PCA position 

# Read tree data
tree <- read.table("DATA/RAW/infc05_apv/t2_05_apv.csv", sep=";", header=T)
tree <- tree[tree$idpunto %in% data$idpunto,]

# Make a community abundance matrix
tree_comm <- as.matrix(as.data.frame.matrix(table(tree$idpunto, tree$SPcod)))

cwm_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(Trait_PCA)]
cwm_tree_comm <- cwm_tree_comm[rowSums(cwm_tree_comm) > 0,]
cwm <- FD::functcomp(Trait_PCA[,c(11:12)], cwm_tree_comm) ### cwm_tree_comm is missing one species but no clue why, any idea?
colnames(cwm) <- paste0("cwm_", colnames(cwm))
dataB <- cbind(data, cwm[match(data$idpunto, rownames(cwm)),])

###Take out Na values 
dataB <- dataB[!is.na(Plot$cwm_Dim1),]
dataB <- dataB[!is.na(Plot$cwm_Dim2),]

### Plot the CWM versus climate variables

VPD1 <- lm(dataB$cwm_Dim1 ~ dataB$vpd)
summary(VPD1)

ggplot(data = dataB, aes(x = vpd, y = cwm_Dim1)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

SoilMoisture1 <- lm(dataB$cwm_Dim1 ~ dataB$soilmoisture)
summary(SoilMoisture1)

ggplot(data = dataB, aes(x = soilmoisture, y = cwm_Dim1)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

VPD2 <- lm(dataB$cwm_Dim2 ~ dataB$vpd)
summary(VPD2)

ggplot(data = dataB, aes(x = vpd, y = cwm_Dim2)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

SoilMoisture2 <- lm(dataB$cwm_Dim2 ~ dataB$soilmoisture)
summary(SoilMoisture2)

ggplot(data = dataB, aes(x = soilmoisture, y = cwm_Dim2)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)


### Option 2B: FDisp + conifer proportion versus climate    

FDis1 <- lm(data$FDis ~ data$vpd)
summary(FDis1)

ggplot(data = data, aes(x = vpd, y = FDis)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

Fdis2 <- lm(data$FDis ~ data$soilmoisture)
summary(Fdis2)

ggplot(data = data, aes(x = soilmoisture, y = FDis)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

### create conifer column
Conifer <- as.data.frame(readxl::read_excel("DATA/conifer.xlsx",col_types = c("numeric", "skip", "text")))

tree$conifer <- Conifer$`Conifer (T/F)`[match(tree$SPcod, Conifer$`Species Code`)]
TRUE. <- subset(tree, conifer == "T")
FALSE. <- subset(tree, conifer == "F")

TRUE.$number <- 1
FALSE.$number <- 1

Truetotal <- aggregate(TRUE.$number, by=list(Category=TRUE.$idpunto), FUN=sum)
Falsetotal <- aggregate(FALSE.$number, by=list(Category=FALSE.$idpunto), FUN=sum)

### add to plot data and create proportion
data$NoConiferAmount <- Falsetotal$x[match(data$idpunto, Falsetotal$Category)]
data$ConiferAmount <- Truetotal$x[match(data$idpunto, Truetotal$Category)]

data$NoConiferAmount[is.na(data$NoConiferAmount)] <- 0
data$ConiferAmount[is.na(data$ConiferAmount)] <- 0

data$ProportionConifer <- data$ConiferAmount/(data$ConiferAmount+data$NoConiferAmount)

###plotting

Conifer1 <- lm(data$ProportionConifer ~ data$vpd)
summary(Conifer1)

ggplot(data = data, aes(x = vpd, y = ProportionConifer)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

FdisB <- lm(data$ProportionConifer ~ data$soilmoisture)
summary(FdisB)

ggplot(data = data, aes(x = soilmoisture, y = ProportionConifer)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)


### Check is species richness is affected by climate like shown by Marco and Angelo
library(data.table)

Richness_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(trait)]
Richness_tree_comm <- Richness_tree_comm[rowSums(Richness_tree_comm) > 0,]
Richness_tree_comm[Richness_tree_comm>0] <-1

Richness <- rowSums(Richness_tree_comm)
SpeciesRichness <- as.data.frame(Richness)
setDT(SpeciesRichness, keep.rownames = TRUE)
names(SpeciesRichness)[1] <- "IDPlot"

data$SpeciesRichness <- SpeciesRichness$Richness[match(data$idpunto, SpeciesRichness$IDPlot)]

RichnessA <- lm(data$SpeciesRichness ~ data$vpd)
summary(RichnessA)

ggplot(data = data, aes(x = vpd, y = SpeciesRichness)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

RichnessB <- lm(data$SpeciesRichness ~ data$soilmoisture)
summary(RichnessB)

ggplot(data = data, aes(x = soilmoisture, y = SpeciesRichness)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

