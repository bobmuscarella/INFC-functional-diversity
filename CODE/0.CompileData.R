##%######################################################%##
#                                                          #
####                  Load R packages                   ####
#                                                          #
##%######################################################%##
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar())

folder_names <- c( "output_data", 'output_plot','output_tab')
#Check if the folders  exist in the current directory, if not creates it
for (i in folder_names){
  ifelse(!dir.exists(i), dir.create(i), print("Folder exists already"))
}

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c('raster','sp','readxl', 'FD', 'rSDM', 'data.table','ncdf4')
ipak(packages)


##%######################################################%##
#                                                          #
####                  Read plot data                    ####
#                                                          #
##%######################################################%##

plot <- read.csv("DATA/RAW/infc05_quantiF3/t1_05_quantiF3.csv", sep=";")
plot <- plot[plot$codcfor < 17,] # Filter unwanted Forest codes out

# Remove plots with recent explotation
plot <- plot[plot$Vut_ha == 0 | is.na(plot$Vut_ha),]

# Remove unused columns in plot data
plot <- plot[,colnames(plot) %in% c("idpunto","codcfor",
                                   "LAT_ND_W84","LON_ND_W84",
                                   "ICCapv_ha","ICWapv_ha","ICVapv_ha",
                                   "Capv_ha","Capm_ha")]

# Here our working projection
setProj<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Making spatialpointdataframe before extraction allow controlling for possible CRS misleding
head(plot)
xy.dt<- plot[,3:2]
quanti.sp<- SpatialPointsDataFrame(coords = xy.dt, 
                                   data = plot,
                                   proj4string = CRS(setProj))
plot(quanti.sp, pch='.')
rm(xy.dt)

##%######################################################%##
#                                                          #
####               ADD ENVIRONMENTAL DATA               ####
#                                                          #
##%######################################################%##
#----- LOAD VPD and check projection coherence -----#
vpd.r<-raster::raster("DATA/TerraClimate19812010_vpd_italy.nc")
raster::crs(vpd.r)
quanti.sp@data$vpd <-raster::extract(vpd.r, quanti.sp)
#rm(vpd.r)

#---- Topographic  -----#
italy_elev <- raster::getData('alt',country="ITA", path = "DATA")
projection(italy_elev)
italy_slp <- terrain(italy_elev, opt=c('slope'), unit='degrees')
italy_asp <- terrain(italy_elev, opt=c('aspect'), unit='degrees')
quanti.sp@data$elev<-raster::extract(italy_elev, quanti.sp)
quanti.sp@data$slope<-raster::extract(italy_slp, quanti.sp)
quanti.sp@data$aspect<-raster::extract(italy_asp, quanti.sp)

#---- Climate classification -----#
## WARNING !!: Is NOT a good idea re-project RASTER data as it involves pixel deformation. Instead we can easily re-project our spatialpointdataframe (vector) before extraction.
cc<- raster::raster("DATA/climateclassification/hdr.adf")
raster::crs(cc)
# So, being incompatible projections
quanti.sp_proj <- spTransform(quanti.sp, raster::crs(cc))
projection(quanti.sp_proj)
quanti.sp_proj@data$climate_classification<-raster::extract(cc, quanti.sp_proj)
# Reproject to the original proj
quanti.sp <- spTransform(quanti.sp_proj, CRS(setProj)) # reproject to the original proj
head(quanti.sp@data)
rm(quanti.sp_proj)

##%######################################################%##
#                                                          #
####        Assign values to NA grid cells              ####
#                                                          #
##%######################################################%##

### FOR PLOTS THAT HAVE NA VALUES OF CLIMATE, ASSIGN VALUE FROM NEAREST, NON-NA GRID CELL
# devtools::install_github("Pakillo/rSDM")
pts = quanti.sp[is.na((quanti.sp$slope)),]
pts <- points2nearestcell(locs=pts, ras=italy_slp, showmap=F, showchanges=F)
quanti.sp$slope[is.na(quanti.sp$slope)] <- raster::extract(italy_slp, pts)

pts = quanti.sp[is.na((quanti.sp$aspect)),]
pts <- points2nearestcell(locs=pts, ras=italy_asp, showmap=F, showchanges=F)
quanti.sp$aspect[is.na(quanti.sp$aspect)] <- raster::extract(italy_asp, pts)

# So, being incompatible projections
quanti.sp_proj <- spTransform(quanti.sp, raster::crs(cc))
pts = quanti.sp_proj[is.na((quanti.sp_proj$climate_classification)),]
pts <- points2nearestcell(locs=pts, ras=cc, showmap=F, showchanges=F)
quanti.sp_proj$climate_classification[is.na(quanti.sp_proj$climate_classification)] <- raster::extract(cc, pts)
# Reproject to the original proj
quanti.sp <- spTransform(quanti.sp_proj, CRS(setProj)) # reproject to the original proj

rm(italy_asp, italy_elev, italy_slp, quanti.sp_proj,cc)

##%######################################################%##
#                                                          #
####               ADD Functional Indices               ####
#                                                          #
##%######################################################%##

# Back to dataframe
df<- quanti.sp@data  # I called the whole dataframe


# Read tree data
tree <- read.table("DATA/RAW/infc05_apv/t2_05_apv.csv", sep=";", header=T)
tree <- tree[tree$idpunto %in% df$idpunto,]

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
head(trait)

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
df$FDis_All <- dbfd$FDis[match(df$idpunto, names(dbfd$FDis))]

# Compute and add species richness
SpeciesRichness <- as.data.frame (rowSums(tree_comm))
setDT(SpeciesRichness, keep.rownames = "idpunto")
SpeciesRichness$idpunto <- as.double(SpeciesRichness$idpunto)
names(SpeciesRichness)[2]<-paste("SpRich")
df <- merge(SpeciesRichness,df,by="idpunto")
                                     
# Compute and add community-mean traits
cwm_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(trait)]
cwm_tree_comm <- cwm_tree_comm[rowSums(cwm_tree_comm) > 0,]
cwm <- FD::functcomp(trait[,c("SeedMass_log",
                              "Height_log",
                              "SLA_log",
                              "StemDensity",
                              "XylemVulnerability")], cwm_tree_comm)
colnames(cwm) <- paste0("cwm_", colnames(cwm))
df <- cbind(df, cwm[match(df$idpunto, rownames(cwm)),])

# Compute and add functional dispersion for each trait
# Need to split up trait data in seperate traits to exclude NA´s
SeedMass_log <- as.data.frame(trait$SeedMass_log)
rownames(SeedMass_log) <- trait$`Species Code`
SeedMass_log <- na.omit(SeedMass_log)

FDis_SeedMass_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(SeedMass_log)]
FDis_SeedMass_tree_comm <- FDis_SeedMass_tree_comm[rowSums(FDis_SeedMass_tree_comm) > 0,]
FDis_SeedMass <- FD::dbFD(SeedMass_log, FDis_SeedMass_tree_comm, w.abun=T, corr = "cailliez", 
                          calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
df$FDis_SeedMass <- FDis_SeedMass$FDis[match(df$idpunto, names(FDis_SeedMass$FDis))]

Height_log <- as.data.frame(trait$Height_log)
rownames(Height_log) <- trait$`Species Code`
Height_log <- na.omit(Height_log)

FDis_Height_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(Height_log)]
FDis_Height_tree_comm <- FDis_Height_tree_comm[rowSums(FDis_Height_tree_comm) > 0,]
FDis_Height <- FD::dbFD(Height_log, FDis_Height_tree_comm, w.abun=T, corr = "cailliez", 
                          calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
df$FDis_Height <- FDis_Height$FDis[match(df$idpunto, names(FDis_Height$FDis))]

SLA_log <- as.data.frame(trait$SLA_log)
rownames(SLA_log) <- trait$`Species Code`
SLA_log <- na.omit(SLA_log)

FDis_SLA_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(SLA_log)]
FDis_SLA_tree_comm <- FDis_SLA_tree_comm[rowSums(FDis_SLA_tree_comm) > 0,]
FDis_SLA <- FD::dbFD(SLA_log, FDis_SLA_tree_comm, w.abun=T, corr = "cailliez", 
                          calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
df$FDis_SLA <- FDis_SLA$FDis[match(df$idpunto, names(FDis_SLA$FDis))]

StemDensity <- as.data.frame(trait$StemDensity)
rownames(StemDensity) <- trait$`Species Code`
StemDensity <- na.omit(StemDensity)

FDis_StemDensity_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(StemDensity)]
FDis_StemDensity_tree_comm <- FDis_StemDensity_tree_comm[rowSums(FDis_StemDensity_tree_comm) > 0,]
FDis_StemDensity <- FD::dbFD(StemDensity, FDis_StemDensity_tree_comm, w.abun=T, corr = "cailliez", 
                          calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
df$FDis_StemDensity <- FDis_StemDensity$FDis[match(df$idpunto, names(FDis_StemDensity$FDis))]

XylemVulnerability <- as.data.frame(trait$XylemVulnerability)
rownames(XylemVulnerability) <- trait$`Species Code`
XylemVulnerability <- na.omit(XylemVulnerability)

FDis_XylemVulnerability_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(XylemVulnerability)]
FDis_XylemVulnerability_tree_comm <- FDis_XylemVulnerability_tree_comm[rowSums(FDis_XylemVulnerability_tree_comm) > 0,]
FDis_XylemVulnerability <- FD::dbFD(XylemVulnerability, FDis_XylemVulnerability_tree_comm, w.abun=T, corr = "cailliez", 
                          calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
df$FDis_XylemVulnerability <- FDis_XylemVulnerability$FDis[match(df$idpunto, names(FDis_XylemVulnerability$FDis))]

#Compute and add community-mean PCA postion
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
library(factoextra)
A2 <- fviz_pca_var(ordB, col.var="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE # Avoid text overlapping)
)

###  visualize results for plots
B2 <- fviz_pca_ind(ordB,
                   label = "none", # hide individual labels
)

PCAplot1 <- ggpubr::ggarrange(A2, B2, labels = c("A", "B"), ncol = 2, nrow = 1)
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
df <- cbind(df, cwm_PCA[match(df$idpunto, rownames(cwm)),])
par(resetPar())



#--------------------- End of script -----------------------#



<<<<<<< HEAD:CODE/x.CompileData.R


=======
write.csv(df, "output_data/Data_for_analysis.csv", row.names=F)
>>>>>>> ec1db9416573693eeced8419620e4924a4be5069:CODE/0.CompileData.R

