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


write.csv(plot, "DATA/Data_for_analysis.csv", row.names=F)

