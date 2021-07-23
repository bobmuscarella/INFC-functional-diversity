library(raster)
library(readxl)

# Read plot data
plot <- read.csv("DATA/RAW/infc05_quantiF3/t1_05_quantiF3.csv", sep=";")
plot <- plot[plot$codcfor < 17,] # Filter unwanted Forest codes out

# SHOULD WE REMOVE PLOTS WITH ANY RECENT EXPLOITATION?
# sum(plot$Vut_ha>0, na.rm=T)
plot <- plot[plot$Vut_ha == 0 | is.na(plot$Vut_ha),]

# Remove unused columns in plot data
plot <- plot[,colnames(plot) %in% c("idpunto","codcfor",
                                   "LAT_ND_W84","LON_ND_W84",
                                   "ICCapv_ha","ICWapv_ha","ICVapv_ha",
                                   "Capv_ha","Capm_ha")]

### ADD ENVIRONMENTAL DATA
plot$vpd <- raster::extract(raster("DATA/TerraClimate19812010_vpd_italy.nc"), 
                            plot[,c("LON_ND_W84","LAT_ND_W84")])

plot$soilmoisture <- raster::extract(raster("DATA/TerraClimate19812010_soil_italy.nc"), 
                                     plot[,c("LON_ND_W84","LAT_ND_W84")])

# Topographic 
italy_elev <- raster::getData('alt',country="ITA", path = "DATA")
slp <- terrain(italy_elev, opt=c('slope'), unit='degrees')
asp <- terrain(italy_elev, opt=c('aspect'), unit='degrees')
plot$slope <- raster::extract(slp, plot[,c("LON_ND_W84","LAT_ND_W84")])
plot$aspect <- raster::extract(asp, plot[,c("LON_ND_W84","LAT_ND_W84")])

# Climate classification
cc <- projectRaster(raster("DATA/climateclassification/hdr.adf"), italy_elev)
plot$climate_classification <- raster::extract(cc, plot[,c("LON_ND_W84","LAT_ND_W84")])


### FOR PLOTS THAT HAVE NA VALUES OF CLIMATE, ASSIGN VALUE FROM NEAREST, NON-NA GRID CELL
# devtools::install_github("Pakillo/rSDM")
library(rSDM)
pts <- SpatialPoints(plot[is.na(plot$slope),c("LON_ND_W84","LAT_ND_W84")], 
                     proj4string=crs(slp))
pts_new <- points2nearestcell(locs=pts, ras=slp, showmap=F, showchanges=F)
plot$slope[is.na(plot$slope)] <- raster::extract(slp, pts_new)

pts <- SpatialPoints(plot[is.na(plot$aspect),c("LON_ND_W84","LAT_ND_W84")], 
                     proj4string=crs(asp))
pts_new <- points2nearestcell(locs=pts, ras=asp, showmap=F, showchanges=F)
plot$aspect[is.na(plot$aspect)] <- raster::extract(asp, pts_new)

pts <- SpatialPoints(plot[is.na(plot$climate_classification),c("LON_ND_W84","LAT_ND_W84")],
                     proj4string=crs(cc))
pts_new <- points2nearestcell(locs=pts, ras=cc, showmap=F, showchanges=F)
plot$climate_classification[is.na(plot$climate_classification)] <- raster::extract(cc, pts_new)

# Read tree data
tree <- read.table("DATA/RAW/infc05_apv/t2_05_apv.csv", sep=";", header=T)
tree <- tree[tree$idpunto %in% plot$idpunto,]

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
trait <- trait[trait$`Species Code` %in% tree$SPcod, -c(8:9)]
rownames(trait) <- trait$`Species Code`

# Log-transform some variables
trait$SeedMass_log <- log(trait$SeedMass)
trait$Height_log <- log(trait$Height)
trait$SLA_log <- log(trait$SLA)

# Subset to 5 traits and only 1 NA per row for Fdisp
fd_trait <- trait[rowSums(is.na(trait[,c(3:7)])) < 2 , -c(1:4,6)]

# Make a community abundance matrix
tree_comm <- as.matrix(as.data.frame.matrix(table(tree$idpunto, tree$SPcod)))

# Compute and add functional dispersion
fd_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(fd_trait)]
fd_tree_comm <- fd_tree_comm[rowSums(fd_tree_comm) > 0,]
dbfd <- FD::dbFD(fd_trait, fd_tree_comm, w.abun=T, corr = "cailliez", 
                 calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
plot$FDis <- dbfd$FDis[match(plot$idpunto, names(dbfd$FDis))]
plot$SpRich <- dbfd$FDis[match(plot$idpunto, names(dbfd$nbsp))]

# Compute and add community-mean traits
cwm_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(trait)]
cwm_tree_comm <- cwm_tree_comm[rowSums(cwm_tree_comm) > 0,]
cwm <- FD::functcomp(trait[,c("SeedMass_log",
                              "Height_log",
                              "SLA_log",
                              "StemDensity",
                              "XylemVulnerability")], cwm_tree_comm)
colnames(cwm) <- paste0("cwm_", colnames(cwm))
plot <- cbind(plot, cwm[match(plot$idpunto, rownames(cwm)),])

head(plot)


<<<<<<< HEAD:CODE/x.CompileData.R




# VARIABLES OF INTEREST

# Annual increment
plot$ICCapv_ha # Current annual volume increment of living trees
plot$ICWapv_ha # Dry weight correspondent to the current annual volume increment of living trees
plot$ICVapv_ha # Organic carbon stock correspondent to the current annual volume increment of living trees

# Stock (sum?)
plot$Capv_ha # Organic carbon stock of total above-ground biomass of living trees
plot$Capm_ha # Organic carbon stock of the total above-ground biomass of standing dead trees

# Climate
Forest type
Climate classification
VPD

# How are functional composition (CWM) and diversity (FDisp) related to climate?
# Do plots with higher *response variable* have higher/lower CWM values?
# Do plots with higher *response variable* have higher functional diversity?
# Does this relationship differ between Mediterranean and temperate climates?
# Do other climate variables mediate the relationship between FD and *response*?


#############################
#### Research question 1 ####
#############################

### Option 1: Create one environmental gradient and plot versus CWM/Fdis
VPD <- plot$vpd
Soil_Moisture <- plot$soilmoisture
ord <- prcomp( ~ VPD  +
                 Soil_Moisture,
               center = TRUE, scale = TRUE)
summary(ord)

library("factoextra")
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

plot$Dim1 <- Dim1
plot$Dim2 <- Dim2

## Plotting relationships out

SLA <- lm(plot$cwm_SLA ~ plot$Dim2)
summary(SLA)

ggplot(data = plot, aes(x = Dim2, y = cwm_SLA)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

Wooddensity <- lm(plot$cwm_StemDensity ~ plot$Dim2)
summary(Wooddensity)

ggplot(data = plot, aes(x = Dim2, y = cwm_StemDensity)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

Height <- lm(plot$cwm_Height ~ plot$Dim2)
summary(Height)

ggplot(data = plot, aes(x = Dim2, y = cwm_Height)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

SeedMass <- lm(plot$cwm_SeedMass ~ plot$Dim2)
summary(SeedMass)

ggplot(data = plot, aes(x = Dim2, y = cwm_SeedMass)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

XylemVulnerability <- lm(plot$cwm_XylemVulnerability ~ plot$Dim2)
summary(XylemVulnerability)

ggplot(data = plot, aes(x = Dim2, y = cwm_XylemVulnerability)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

FunctionalDispersion <- lm(plot$FDis ~ plot$Dim2)
summary(FunctionalDispersion)

ggplot(data = plot, aes(x = Dim2, y = FDis)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

# Visualize the the relationship between soil moisture and vdp

ggplot(data = plot, aes(x = vpd, y = soilmoisture)) + geom_point(color='red') 

# Visualize dimension 1, to see where low soil moisture/vpd plots occur
pacman::p_load(ggplot2, sf, rnaturalearth, rnaturalearthdata, sp, rgeos)
theme_set(theme_bw()) ## sets background theme
Italy <- ne_countries(country = 'italy',scale = "medium", returnclass = "sf")

ggplot(data = Italy) + geom_sf() + geom_point(data = plot, aes(LON_ND_W84, LAT_ND_W84, color = Dim1)) + labs(y = "", x = "") + scale_color_gradientn(colours = rainbow(5))



### Option 2A: PCA species traits + CWM species position PCA 

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

cwm_tree_commB <- tree_comm[,colnames(tree_comm) %in% rownames(Trait_PCA)]
cwm_tree_commB <- cwm_tree_commB[rowSums(cwm_tree_commB) > 0,]
cwmB <- FD::functcomp(Trait_PCA[,c(11:12)], cwm_tree_commB)
colnames(cwmB) <- paste0("cwm_", colnames(cwmB))
PlotB <- cbind(plot, cwmB[match(plot$idpunto, rownames(cwmB)),])

###Take out Na values 
PlotB <- PlotB[!is.na(PlotB$cwm_Dim1),]
PlotB <- PlotB[!is.na(PlotB$cwm_Dim2),]

### Plot the CWM versus climate variables

VPD1 <- lm(PlotB$cwm_Dim1 ~ PlotB$vpd)
summary(VPD1)

ggplot(data = PlotB, aes(x = vpd, y = cwm_Dim1)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

SoilMoisture1 <- lm(PlotB$cwm_Dim1 ~ PlotB$soilmoisture)
summary(SoilMoisture1)

ggplot(data = PlotB, aes(x = soilmoisture, y = cwm_Dim1)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

VPD2 <- lm(PlotB$cwm_Dim2 ~ PlotB$vpd)
summary(VPD2)

ggplot(data = PlotB, aes(x = vpd, y = cwm_Dim2)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

SoilMoisture2 <- lm(PlotB$cwm_Dim2 ~ PlotB$soilmoisture)
summary(SoilMoisture2)

ggplot(data = PlotB, aes(x = soilmoisture, y = cwm_Dim2)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)


### Option 2B: FDisp + conifer proportion versus climate    #

FDis1 <- lm(plot$FDis ~ plot$vpd)
summary(FDis1)

ggplot(data = plot, aes(x = vpd, y = FDis)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

Fdis2 <- lm(plot$FDis ~ plot$soilmoisture)
summary(Fdis2)

ggplot(data = plot, aes(x = soilmoisture, y = FDis)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

### create conifer column
Conifer <- read_excel("C:/Users/roell/OneDrive/Bureaublad/Conifer.xlsx", 
                      col_types = c("numeric", "skip", "text"))

tree$conifer <- Conifer$`Conifer (T/F)`[match(tree$SPcod, Conifer$`Species Code`)]
TRUE. <- subset(tree, conifer == "T")
FALSE. <- subset(tree, conifer == "F")

TRUE.$number <- 1
FALSE.$number <- 1

Truetotal <- aggregate(TRUE.$number, by=list(Category=TRUE.$idpunto), FUN=sum)
Falsetotal <- aggregate(FALSE.$number, by=list(Category=FALSE.$idpunto), FUN=sum)

### add to plot data and create proportion
plot$NoConiferAmount <- Falsetotal$x[match(plot$idpunto, Falsetotal$Category)]
plot$ConiferAmount <- Truetotal$x[match(plot$idpunto, Truetotal$Category)]

plot$NoConiferAmount[is.na(plot$NoConiferAmount)] <- 0
plot$ConiferAmount[is.na(plot$ConiferAmount)] <- 0

plot$ProportionConifer <- plot$ConiferAmount/(plot$ConiferAmount+plot$NoConiferAmount)

###plotting

Conifer1 <- lm(plot$ProportionConifer ~ plot$vpd)
summary(Conifer1)

ggplot(data = plot, aes(x = vpd, y = ProportionConifer)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

FdisB <- lm(plot$ProportionConifer ~ plot$soilmoisture)
summary(FdisB)

ggplot(data = plot, aes(x = soilmoisture, y = ProportionConifer)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)


### Check is species richness is affected by climate like shown by Marco and Angelo

Richness_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(trait)]
Richness_tree_comm <- Richness_tree_comm[rowSums(Richness_tree_comm) > 0,]
Richness_tree_comm[Richness_tree_comm>0] <-1

Richness <- rowSums(Richness_tree_comm)
SpeciesRichness <- as.data.frame(Richness)
setDT(SpeciesRichness, keep.rownames = TRUE)
names(SpeciesRichness)[1] <- "IDPlot"

plot$SpeciesRichness <- SpeciesRichness$Richness[match(plot$idpunto, SpeciesRichness$IDPlot)]

RichnessA <- lm(plot$SpeciesRichness ~ plot$vpd)
summary(RichnessA)

ggplot(data = plot, aes(x = vpd, y = SpeciesRichness)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)

RichnessB <- lm(plot$SpeciesRichness ~ plot$soilmoisture)
summary(RichnessB)

ggplot(data = plot, aes(x = soilmoisture, y = SpeciesRichness)) + geom_point(color='red') + geom_smooth(method = "lm", se = TRUE)


=======
write.csv(plot, "DATA/Data_for_analysis.csv", row.names=F)
>>>>>>> ec1db9416573693eeced8419620e4924a4be5069:CODE/0.CompileData.R

