# Load data
data <- read.csv("DATA/Data_for_analysis.csv")
head(data)

### VARIABLES OF INTEREST
### Annual increment
# data$ICCapv_ha # Current annual volume increment of living trees
# data$ICWapv_ha # Dry weight correspondent to the current annual volume increment of living trees
# data$ICVapv_ha # Organic carbon stock of current annual volume increment of living trees

### Stock (sum?)
# data$Capv_ha # Organic carbon stock of total above-ground biomass of living trees
# data$Capm_ha # Organic carbon stock of the total above-ground biomass of standing dead trees

### Climate
# data$codcfor # Forest type from INFI data
# data$climate_classification # Climate class (1=Mediterranean, 0=temperate)
# data$vpd # Mean annual vapor pressure deficit
# data$soilmoisture # Mean annual soil moisture

### Functional diversity / composition
# data$FDis # Functional dispersion
# data$SpRich # Species richness in the plot
# data$cwm_SeedMass_log # community-weighted mean of log seed mass
# data$cwm_Height_log # community-weighted mean of log height
# data$cwm_SLA_log # community-weighted mean of log SLA
# data$cwm_StemDensity # community-weighted mean of wood density
# data$cwm_XylemVulnerability # community-weighted mean of p50

### QUESTIONS
# How are functional composition (CWM) and diversity (FDisp) related to climate?
# Do plots with higher *response variable* have higher/lower CWM values?
# Do plots with higher *response variable* have higher functional diversity?
# Does this relationship differ between Mediterranean and temperate climates?
# Do other climate variables mediate the relationship between FD and *response*?

#############################
#### Research question 1 ####
#############################

### Option 1: Create one environmental gradient and plot versus CWM/Fdis
VPD <- data$vpd
Soil_Moisture <- data$soilmoisture
ord <- prcomp( ~ VPD  +
                 Soil_Moisture,
               center = TRUE, scale = TRUE)
summary(ord)

library("factoextra")
library("ggpubr")
### visualize the variables used in the PCA
A1 <- fviz_pca_var(ord, col.var="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE # Avoid text overlapping)
)

###  visualize results for individuals
B1 <- fviz_pca_ind(ord,
                   label = "none", # hide individual labels
)

### Put the plots together
PCAplot1 <- ggarrange(A1, B1, labels = c("A", "B"), ncol = 2, nrow = 1)
PCAplot1

### Coordinates for individuals and plotting it against soil moisture
res.ind <- get_pca_ind(ord)
Coord <- res.ind$coord

Dim1 <- Coord[,-c(2)]
Dim2 <- Coord[,-c(1)]

data$Dim1 <- Dim1
data$Dim2 <- Dim2

## Plotting relationships out

SLA <- lm(data$cwm_SLA_log ~ data$Dim2)
summary(SLA)

ggplot(data = data, aes(x = Dim2, y = cwm_SLA_log)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

Wooddensity <- lm(data$cwm_StemDensity ~ plot$Dim2)
summary(Wooddensity)

ggplot(data = data, aes(x = Dim2, y = cwm_StemDensity)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

Height <- lm(data$cwm_Height_log ~ data$Dim2)
summary(Height)

ggplot(data = data, aes(x = Dim2, y = cwm_Height_log)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

SeedMass <- lm(data$cwm_SeedMass_log ~ data$Dim2)
summary(SeedMass)

ggplot(data = data, aes(x = Dim2, y = cwm_SeedMass_log)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

XylemVulnerability <- lm(data$cwm_XylemVulnerability ~ data$Dim2)
summary(XylemVulnerability)

ggplot(data = data, aes(x = Dim2, y = cwm_XylemVulnerability)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

FunctionalDispersion <- lm(data$FDis ~ data$Dim2)
summary(FunctionalDispersion)

ggplot(data = data, aes(x = Dim2, y = FDis)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

# Visualize the the relationship between soil moisture and vdp

ggplot(data = data, aes(x = vpd, y = soilmoisture)) + geom_point(color='red') 

# Visualize dimension 1, to see where low soil moisture/vpd plots occur
pacman::p_load(ggplot2, sf, rnaturalearth, rnaturalearthdata, sp, rgeos)
theme_set(theme_bw()) ## sets background theme
Italy <- ne_countries(country = 'italy',scale = "medium", returnclass = "sf")

ggplot(data = Italy) + geom_sf() + geom_point(data = data, aes(LON_ND_W84, LAT_ND_W84, color = Dim1)) + labs(y = "", x = "") + scale_color_gradientn(colours = rainbow(5))



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






