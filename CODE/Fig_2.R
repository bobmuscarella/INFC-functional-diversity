##%######################################################%##
#                                                          #
####                        modA                        ####
#                                                          #
##%######################################################%##
png(
  "output_plot/Fg_2.jpg",
  width = 10,
  height = 7,
  units = 'in',
  res = 300
)
op<- par(mfrow=c(2,3), mar=c(5,5,1,1))

mycols<-c('dodgerblue', 'indianred', 'black')
plot(
  cwm_SeedMass_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab = expression(CWM[SeedMass]),
  type = 'n', ylim=c(-3,2), cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0.003
b <- 0.303
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T)
)

at <- 0.053
bt <- 0.306
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)

am <- -0.135
bm <- -0.138
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2]
)

##%######################################################%##
#                                                          #
####                        modB                        ####
#                                                          #
##%######################################################%##

plot(
  cwm_Height_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab = expression(CWM[Height]),
  type = 'n', ylim=c(-4,2), cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- -0.010
b <- -0.509
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T)
)

at <- 0.063
bt <- -0.485
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)

am <- -0.252
bm <- -0.242
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2]
)

##%######################################################%##
#                                                          #
####                        modC                        ####
#                                                          #
##%######################################################%##

plot(
  cwm_SLA_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab = expression(CWM[SLA]),
  type = 'n', ylim=c(-4,2), cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0.002
b <- 0.193
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T)
)

at <- 0.125
bt <- 0.323
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)

am <- 0.082
bm <- -0.265
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2]
)

##%######################################################%##
#                                                          #
####                        modD                        ####
#                                                          #
##%######################################################%##

plot(
  cwm_StemDensity_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab =  expression(CWM[WD]),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- -0.002
b <-  0.405
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T)
)

at <- 0.040
bt <- 0.434
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)

am <- 0.110
bm <- 0.121
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2]
)

##%######################################################%##
#                                                          #
####                        modE                        ####
#                                                          #
##%######################################################%##

plot(
  cwm_XylemVulnerability_log  ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab =  expression(CWM[Xylem]),
  type = 'n', ylim=c(-4,3), cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- -0.001
b <-  0.014
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=2
)

at <- 0.124
bt <- 0.175
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)

am <- 0.086
bm <- -0.193
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2]
)

##%######################################################%##
#                                                          #
####                        modF                        ####
#                                                          #
##%######################################################%##

plot(
  cwm_Dim1_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab =   expression(CWM[Dim1]),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0.009
b <-  0.240
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=1
)

at <- -0.021
bt <- 0.227
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)

am <- 0.251
bm <- 0.026
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=2
)

par(op)
dev.off()
