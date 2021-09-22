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

# Compute and add functional dispersion all traits
fd_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(fd_trait)]
fd_tree_comm <- fd_tree_comm[rowSums(fd_tree_comm) > 0,]
dbfd <- FD::dbFD(fd_trait, fd_tree_comm, w.abun=T, corr = "cailliez", 
                 calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
plot$FDis_All <- dbfd$FDis[match(plot$idpunto, names(dbfd$FDis))]
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

# Compute and add functional dispersion for each trait
# Need to split up trait data in seperate traits to exclude NA´s
SeedMass_log <- as.data.frame(trait$SeedMass_log)
rownames(SeedMass_log) <- trait$`Species Code`
SeedMass_log <- na.omit(SeedMass_log)

FDis_SeedMass_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(SeedMass_log)]
FDis_SeedMass_tree_comm <- FDis_SeedMass_tree_comm[rowSums(FDis_SeedMass_tree_comm) > 0,]
FDis_SeedMass <- FD::dbFD(SeedMass_log, FDis_SeedMass_tree_comm, w.abun=T, corr = "cailliez", 
                          calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
plot$FDis_SeedMass <- FDis_Seedmass$FDis[match(plot$idpunto, names(FDis_Seedmass$FDis))]

Height_log <- as.data.frame(trait$Height_log)
rownames(Height_log) <- trait$`Species Code`
Height_log <- na.omit(Height_log)

FDis_Height_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(Height_log)]
FDis_Height_tree_comm <- FDis_Height_tree_comm[rowSums(FDis_Height_tree_comm) > 0,]
FDis_Height <- FD::dbFD(Height_log, FDis_Height_tree_comm, w.abun=T, corr = "cailliez", 
                          calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
plot$FDis_Height <- FDis_Height$FDis[match(plot$idpunto, names(FDis_Height$FDis))]

SLA_log <- as.data.frame(trait$SLA_log)
rownames(SLA_log) <- trait$`Species Code`
SLA_log <- na.omit(SLA_log)

FDis_SLA_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(SLA_log)]
FDis_SLA_tree_comm <- FDis_SLA_tree_comm[rowSums(FDis_SLA_tree_comm) > 0,]
FDis_SLA <- FD::dbFD(SLA_log, FDis_SLA_tree_comm, w.abun=T, corr = "cailliez", 
                          calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
plot$FDis_SLA <- FDis_SLA$FDis[match(plot$idpunto, names(FDis_SLA$FDis))]

StemDensity <- as.data.frame(trait$StemDensity)
rownames(StemDensity) <- trait$`Species Code`
StemDensity <- na.omit(StemDensity)

FDis_StemDensity_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(StemDensity)]
FDis_StemDensity_tree_comm <- FDis_StemDensity_tree_comm[rowSums(FDis_StemDensity_tree_comm) > 0,]
FDis_StemDensity <- FD::dbFD(StemDensity, FDis_StemDensity_tree_comm, w.abun=T, corr = "cailliez", 
                          calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
plot$FDis_StemDensity <- FDis_StemDensity$FDis[match(plot$idpunto, names(FDis_StemDensity$FDis))]

XylemVulnerability <- as.data.frame(trait$XylemVulnerability)
rownames(XylemVulnerability) <- trait$`Species Code`
XylemVulnerability <- na.omit(XylemVulnerability)

FDis_XylemVulnerability_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(XylemVulnerability)]
FDis_XylemVulnerability_tree_comm <- FDis_XylemVulnerability_tree_comm[rowSums(FDis_XylemVulnerability_tree_comm) > 0,]
FDis_XylemVulnerability <- FD::dbFD(XylemVulnerability, FDis_XylemVulnerability_tree_comm, w.abun=T, corr = "cailliez", 
                          calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
plot$FDis_XylemVulnerability <- FDis_XylemVulnerability$FDis[match(plot$idpunto, names(FDis_XylemVulnerability$FDis))]

#Compute and add community-mean PCA postion
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

PCAplot1 <- ggarrange(A2, B2, labels = c("A", "B"), ncol = 2, nrow = 1)
PCAplot1

### Contribution of variables
tres.varB <- get_pca_var(ordB)
contrib1 <- tres.varB$contrib
contrib1

### Coordinates for individuals 
res.indB <- get_pca_ind(ordB)
CoordB <- res.indB$coord
Dim1 <- CoordB[,-c(2,3,4,5)]
Dim2 <- CoordB[,-c(1,3,4,5)]

Trait_PCA$Dim1 <- Dim1
Trait_PCA$Dim2 <- Dim2

############# Calculate CWM of PCA position 
cwm_PCA_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(Trait_PCA)]
cwm_PCA_tree_comm <- cwm_PCA_tree_comm[rowSums(cwm_PCA_tree_comm) > 0,]
cwm_PCA <- FD::functcomp(Trait_PCA[,c(11:12)], cwm_PCA_tree_comm) 
colnames(cwm_PCA) <- paste0("cwm_", colnames(cwm_PCA))
plot <- cbind(plot, cwm_PCA[match(data$idpunto, rownames(cwm)),])


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

=======
write.csv(plot, "DATA/Data_for_analysis.csv", row.names=F)
>>>>>>> ec1db9416573693eeced8419620e4924a4be5069:CODE/0.CompileData.R

