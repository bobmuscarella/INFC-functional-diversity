##%######################################################%##
#                                                          #
####                        modA                        ####
#                                                          #
##%######################################################%##
png(
  "output_plot/Fg_5.jpg",
  width = 10,
  height = 7,
  units = 'in',
  res = 300
)
op<- par(mfrow=c(2,3), mar=c(5,5,1,1))

mycols<-c('dodgerblue', 'indianred', 'black')
plot(
  Capv_ha_log ~ FDis_SeedMass_log,
  dataset,
  ylab = expression(C[apv]),
  xlab =  expression(FDis[SeedMass]), ylim=c(-1,1),
  type = 'n',  cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0
b <- 0.112
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_SeedMass_log, na.rm = T),
  x2 = max(dataset$FDis_SeedMass_log, na.rm = T), lty=0
)

at <- 0.053
bt <- 0.07
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_SeedMass_log, na.rm = T),
  x2 = max(dataset.t$FDis_SeedMass_log, na.rm = T),col=mycols[1]
)

am <- -0.135
bm <-0.204
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_SeedMass_log, na.rm = T),
  x2 = max(dataset.m$FDis_SeedMass_log, na.rm = T),col=mycols[2]
)
usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'a)',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)
##%######################################################%##
#                                                          #
####                        modB                        ####
#                                                          #
##%######################################################%##

plot(
  Capv_ha_log ~ FDis_Height_log,
  dataset,
  ylab = expression(C[apv]),
  xlab = expression(FDis[Height]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0.002
b <- 0.081
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_Height_log, na.rm = T),
  x2 = max(dataset$FDis_Height_log, na.rm = T), lty=0

)

at <- 0.037
bt <- 0.054
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_Height_log, na.rm = T),
  x2 = max(dataset.t$FDis_Height_log, na.rm = T),col=mycols[1]
)

am <- -0.052
bm <-0.148
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_Height_log, na.rm = T),
  x2 = max(dataset.m$FDis_Height_log, na.rm = T),col=mycols[2]
)
usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'b)',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)
##%######################################################%##
#                                                          #
####                        modC                        ####
#                                                          #
##%######################################################%##

plot(
  Capv_ha_log ~ FDis_SLA_log,
  dataset,
  ylab = expression(C[apv]),
  xlab = expression(FDis[SLA]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0.001
b <- 0.118
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_SLA_log, na.rm = T),
  x2 = max(dataset$FDis_SLA_log, na.rm = T), lty=0

)

at <- 0.057
bt <- 0.086
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_SLA_log, na.rm = T),
  x2 = max(dataset.t$FDis_SLA_log, na.rm = T),col=mycols[1]
)

am <- 0.029
bm <- 0.183
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_SLA_log, na.rm = T),
  x2 = max(dataset.m$FDis_SLA_log, na.rm = T),col=mycols[2]
)
usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'c)',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)
##%######################################################%##
#                                                          #
####                        modD                        ####
#                                                          #
##%######################################################%##

plot(
  Capv_ha_log ~ FDis_StemDensity_log,
  dataset,
  ylab = expression(C[apv]),
  xlab =  expression(FDis[WD]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0.001
b <-  0.121
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_StemDensity_log, na.rm = T),
  x2 = max(dataset$FDis_StemDensity_log, na.rm = T), lty=0

)

at <- 0.055
bt <- 0.084
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_StemDensity_log, na.rm = T),
  x2 = max(dataset.t$FDis_StemDensity_log, na.rm = T),col=mycols[1]
)

am <- -0.032
bm <- 0.207
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_StemDensity_log, na.rm = T),
  x2 = max(dataset.m$FDis_StemDensity_log, na.rm = T),col=mycols[2]
)
usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'd)',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)
##%######################################################%##
#                                                          #
####                        modE                        ####
#                                                          #
##%######################################################%##

plot(
  Capv_ha_log  ~ FDis_XylemVulnerability_log,
  dataset,
  ylab = expression(C[apv]),
  xlab =  expression(FDis[Xylem]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0
b <-  0.058
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_XylemVulnerability_log, na.rm = T),
  x2 = max(dataset$FDis_XylemVulnerability_log, na.rm = T), lty=0

)

at <- 0.057
bt <- 0.027
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_XylemVulnerability_log, na.rm = T),
  x2 = max(dataset.t$FDis_XylemVulnerability_log, na.rm = T),col=mycols[1], lty=2
)

am <- -0.169
bm <- 0.124
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_XylemVulnerability_log, na.rm = T),
  x2 = max(dataset.m$FDis_XylemVulnerability_log, na.rm = T),col=mycols[2]
)
usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'e)',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)
##%######################################################%##
#                                                          #
####                        modF                        ####
#                                                          #
##%######################################################%##

plot(
  Capv_ha_log ~ FDis_All_log,
  dataset,
  ylab = expression(C[apv]),
  xlab =   expression(FDis[all]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0
b <- 0.150
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_All_log, na.rm = T),
  x2 = max(dataset$FDis_All_log, na.rm = T), lty=0

)

at <- 0.05
bt <- 0.066
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_All_log, na.rm = T),
  x2 = max(dataset.t$FDis_All_log, na.rm = T),col=mycols[1]
)

am <- -0.123
bm <- 0.189
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_All_log, na.rm = T),
  x2 = max(dataset.m$FDis_All_log, na.rm = T),col=mycols[2]
)
usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'f)',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)
par(op)
dev.off()
