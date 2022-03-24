##%######################################################%##
#                                                          #
####                        modA                        ####
#                                                          #
##%######################################################%##
png(
  "output_plot/Fig_1.jpg",
  width = 10,
  height = 5,
  units = 'in',
  res = 300
)
op<- par(mfrow=c(1,2), mar=c(5,5,1,1))


IT0 <-
  raster::getData('GADM',
                  country = 'ITA',
                  level = 0,
                  path = 'DATA')
projection(IT0)
#IT0<-subset(IT0, NAME_1 !='Sicily' & NAME_1 !='Sardegna')
it_boundary <- spTransform(IT0,  CRS("+proj=longlat +datum=WGS84"))

it_dem <- crop(italy_elev, extent(it_boundary))
it_dem_mask <- mask(it_dem, it_boundary)
it_slope <- raster::terrain(it_dem_mask, opt = 'slope')#get slope and aspect
it_aspect <- raster::terrain(it_dem_mask, opt = 'aspect')
it_hill <- raster::hillShade(it_slope, it_aspect, angle=40, direction=270)# compute hillshade
it_hill_mask <- it_hill


plot(
  it_hill_mask,
  #maxpixels=5e6,
  col = grey(0:100 / 100),
  legend = FALSE,
  ylim = c(36, 49),
  xlab = "",
  ylab = "" ,
  fg = "grey",
  axes = T
)
vpd.r_mask <- mask(vpd.r, it_boundary)
plot(vpd.r_mask,
     #col=adjustcolor(my_col, alpha.f = 0.45),
     #maxpixels=5e6,
     #breaks=brk,
     add = T, legend = T)
usr <- par("usr")
text(usr[1],
     usr[4]-0.5,
     labels = 'a)',
     pos=4,
     col = 'black', cex=1)

plot(
  it_hill_mask,
  #maxpixels=5e6,
  col = NA,
  legend = FALSE,
  ylim = c(36, 49),
  xlab = "",
  ylab = "" ,
  fg = "grey",
  axes = T
)

plot(
  it_boundary,
  col = 'grey80',
  add = T,
  fg = "grey",
  axes = T
)

pal<-c( "#FEE6CE" ,"#FDD0A2" ,"#FDAE6B", "#FD8D3C" ,"#F16913" ,"#D94801", "#A63603", "#7F2704")
plot(
  quanti.sp,
  bg = (brewer.pal(9, 'Oranges'))[cut(
    quanti.sp$Capv_ha,
    breaks = c(0, 50, 100, 150, 200, 250, 300, 350, Inf),
    labels = FALSE
  )],
  col = (pal)[cut(
    quanti.sp$Capv_ha,
    breaks = c(0, 50, 100, 150, 200, 250, 300, 350, Inf),
    labels = FALSE
  )],
  pch = 16,
  axes = TRUE,
  cex = .1,
  add = T
)
usr <- par("usr")
text(usr[1],
     usr[4]-0.5,
     labels = 'b)',
     pos=4,
     col = 'black', cex=1)
dev.off()


##%######################################################%##
#                                                          #
####                        Fig1                        ####
#                                                          #
##%######################################################%##
pdf(
  "output_plot/Fig_1.pdf",
  width = 10,
  height = 5
)
op<- par(mfrow=c(1,2), mar=c(5,5,1,1))



plot(
  it_hill_mask,
  #maxpixels=5e6,
  col = grey(0:100 / 100),
  legend = FALSE,
  ylim = c(36, 49),
  xlab = "",
  ylab = "" ,
  fg = "grey",
  axes = T
)
vpd.r_mask <- mask(vpd.r, it_boundary)
plot(vpd.r_mask,
     #col=adjustcolor(my_col, alpha.f = 0.45),
     #maxpixels=5e6,
     #breaks=brk,
     add = T, legend = T)
usr <- par("usr")
text(usr[1],
     usr[4]-0.5,
     labels = 'a)',
     pos=4,
     col = 'black', cex=1)

plot(
  quanti.sp,
  col = 'black',
  pch = 16,
  axes = TRUE,
  cex = .1,
  add = T
)


plot(0,type='n',axes=FALSE,ann=FALSE)


dev.off()
