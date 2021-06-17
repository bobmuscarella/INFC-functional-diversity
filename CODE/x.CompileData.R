library(raster)
library(readxl)

# Read plot data
plot <- read.csv("DATA/RAW/infc05_quantiF3/t1_05_quantiF3.csv", sep=";")
plot <- plot[plot$codcfor < 17,] # Filter unwanted Forest codes out

# Add environmental data
plot$vpd <- extract(raster("DATA/TerraClimate19812010_vpd.nc"), 
                    plot[,c("LON_ND_W84","LAT_ND_W84")])

plot$soilmoisture <- extract(raster("DATA/TerraClimate19812010_soil.nc"), 
                    plot[,c("LON_ND_W84","LAT_ND_W84")])

# Load + extract climate classification raster layer
load("DATA/ClimateB.Rdata")
plot$climate_classification <- raster::extract(ClimateB, plot[,c("LON_ND_W84","LAT_ND_W84")])

# Read tree data
tree <- read.table("DATA/RAW/infc05_apv/t2_05_apv.csv", sep=";", header=T)
tree <- tree[tree$idpunto %in% plot$idpunto,]

# Read trait data
trait <- as.data.frame(read_excel("DATA/TraitDataFrame.xlsx", 
                                  sheet = "5 traits + 1NA", 
                                  col_types = c("text", 
                                                "text", 
                                                "numeric", 
                                                "numeric", 
                                                "numeric", 
                                                "numeric", 
                                                "numeric"), na = "na"))

# Match SPcod of the trait dataset to tree data
tree <- tree[tree$SPcod %in% trait$`Species Code`,]

# Make a community abundance matrix
tree_comm <- as.matrix(as.data.frame.matrix(table(tree$idpunto, tree$SPcod)))
tree_comm_pa <- 1 * (tree_comm > 0)
  
# Confirm match of names in abundance matrix and trait data
trait$`Species Code`==colnames(tree_comm)
rownames(trait) <- trait$`Species Code`
trait <- trait[,-c(1,2)]

# Log-transform some variables
trait$SeedMass_log <- log10(trait$SeedMass)
trait$Height_log <- log10(trait$Height)
trait$SLA_log <- log10(trait$SLA)

# Functional indices
dbfd <- FD::dbFD(trait[,!colnames(trait) %in% c("SeedMass","Height","SLA")], 
                 tree_comm, w.abun=TRUE, corr = "cailliez")

dbfd <- do.call(cbind, dbfd)
plot <- cbind(plot, dbfd[match(plot$idpunto, rownames(dbfd)),])


### PLOTTING 
# 
# library(maps)
# map(xlim=c(5,20), ylim=c(35,48))
# axis(1); axis(2)
# points(plot[,c("LON_ND_W84","LAT_ND_W84")], col=cut(plot$FDis,5))
# 
# plot(plot$vpd, plot$CWM.XylemVulnerability, 
#      col=plot$climate_classification+1, cex=0.5)






