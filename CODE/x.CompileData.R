library(raster)
library(readxl)

# Read plot data
plot <- read.csv("DATA/RAW/infc05_quantiF3/t1_05_quantiF3.csv", sep=";")
plot <- plot[plot$codcfor < 17,] # Filter unwanted Forest codes out

# REMOVE PLOTS WITH SOME EXPLOITATION?
# sum(plot$Vut_ha>0, na.rm=T)
plot <- plot[plot$Vut_ha == 0 | is.na(plot$Vut_ha),]

# Remove unused columns in plot data
plot <- plot[,colnames(plot) %in% c("idpunto","codcfor",
                                   "LAT_ND_W84","LON_ND_W84",
                                   "ICCapv_ha","ICWapv_ha","ICVapv_ha",
                                   "Capv_ha","Capm_ha")]

### Add environmental data
plot$vpd <- raster::extract(raster("DATA/TerraClimate19812010_vpd_italy.nc"), 
                            plot[,c("LON_ND_W84","LAT_ND_W84")])

plot$soilmoisture <- raster::extract(raster("DATA/TerraClimate19812010_soil_italy.nc"), 
                                     plot[,c("LON_ND_W84","LAT_ND_W84")])

# Topographic 
italy_elev <- raster::getData('alt',country="ITA", path = "DATA")
plot$slope <- raster::extract(terrain(italy_elev, opt=c('slope'), unit='degrees'), 
                           plot[,c("LON_ND_W84","LAT_ND_W84")])
plot$aspect <- raster::extract(terrain(italy_elev, opt=c('aspect'), unit='degrees'), 
                               plot[,c("LON_ND_W84","LAT_ND_W84")])

# Load + extract climate classification raster layer
load("DATA/ClimateB.Rdata")
plot$climate_classification <- raster::extract(ClimateB, plot[,c("LON_ND_W84","LAT_ND_W84")])

# Read tree data
tree <- read.table("DATA/RAW/infc05_apv/t2_05_apv.csv", sep=";", header=T)
tree <- tree[tree$idpunto %in% plot$idpunto,]

# Read trait data
trait <- as.data.frame(read_excel("DATA/TraitDataFrame.xlsx", 
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

# Functional indices
fd_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(fd_trait)]
fd_tree_comm <- fd_tree_comm[rowSums(fd_tree_comm) > 0,]
dbfd <- FD::dbFD(fd_trait, fd_tree_comm, w.abun=T, corr = "cailliez", 
                 calc.CWM=F, calc.FRic=F, calc.FGR=F, calc.FDiv=F)
dbfd <- do.call(cbind, dbfd)
plot <- cbind(plot, dbfd[match(plot$idpunto, rownames(dbfd)),])

# Community-mean traits
cwm_tree_comm <- tree_comm[,colnames(tree_comm) %in% rownames(trait)]
cwm_tree_comm <- cwm_tree_comm[rowSums(cwm_tree_comm) > 0,]
cwm <- FD::functcomp(trait[,c(3:7)], cwm_tree_comm)
colnames(cwm) <- paste0("cwm_", colnames(cwm))
plot <- cbind(plot, cwm[match(plot$idpunto, rownames(cwm)),])

head(plot)






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




