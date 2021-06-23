#### READ DATA ####
path <- "/Users/au529793/Projects/Random DATA/Italian_Forest_Inventory/DATA/RAW/"
tree <- read.csv(paste0(path, "infc05_apv/t2_05_apv.csv"), sep=";")
plot <- read.csv(paste0(path, "infc05_quantiF3/t1_05_quantiF3.csv"), sep=";")
env <- read.csv(paste0(path, "infc05_quantiF3+/t1_05_quantiF3+.csv"), sep=";")

#### Filter out plantations, unstocked (burned) plots, and plots that had some exploitation in the last 12 months
drop_plots <- plot$idpunto[plot$codcfor>17 | plot$Cut_ha>0]
plot <- droplevels(plot[!plot$idpunto %in% drop_plots,])
tree <- droplevels(tree[!tree$idpunto %in% drop_plots,])
env <- droplevels(env[!env$idpunto %in% drop_plots,])

head(tree)
head(plot)
head(env)

#### Compute total increment per plot
tot_incr <- tapply(tree$W4apv, tree$idpunto, sum)
plot$tot_incr <- tot_incr[match(plot$idpunto, names(tot_incr))]

map('italy')

points(plot$LON_ND_W84, plot$LAT_ND_W84, pch=21, bg=rgb(1,0,0,0.25), cex=plot$tot_incr/10)

plot(plot$LAT_ND_W84, plot$tot_incr, log='', pch=16, col=rgb(0,0,0,0.2))
abline(lm((plot$tot_incr) ~ plot$LAT_ND_W84), col=2)

# Current carbon stock
plot$ICVapv_ha

# Carbon stock increment
plot$ICCapv_ha

plot(plot$ICVapv_ha, plot$ICCapv_ha, log='xy')
abline(0,1)


### Get records for a focal tree species
foctree <- tree[tree$SPcod==10,]

foctree_abund <- table(foctree$idpunto)

plot$foctree_abund <- foctree_abund[match(plot$idpunto, names(foctree_abund))]
plot$foctree_abund[is.na(plot$foctree_abund)] <- 0




head(plot)

library(raster)
map <- raster("/Users/au529793/Projects/Postdoc/Global_Palm_Abundance/DATA/GIS/CHELSA/Current/Annual PPT mm_yr/CHELSA_bio10_12.tif")


italy <- map('italy')

map_italy <- crop(map, extent(range(italy$x, na.rm=T), range(italy$y, na.rm=T)))
plot(map, add=T)

plot(map_italy)
points(plot$LON_ND_W84, plot$LAT_ND_W84, pch=21, bg=rgb(1,0,0,0.5), cex=plot$foctree_abund/25)
points(plot$LON_ND_W84, plot$LAT_ND_W84, pch=3, col=rgb(0,0,1,0.5), cex=0.1)


plot$map <- extract(map_italy, plot[,c('LON_ND_W84', 'LAT_ND_W84')])
boxplot(plot$map ~ plot$foctree_abund>0, col=1:2)
plot(plot$map, plot$foctree_abund)


