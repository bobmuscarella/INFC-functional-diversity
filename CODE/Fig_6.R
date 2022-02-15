##%######################################################%##
#                                                          #
####                        modA                        ####
#                                                          #
##%######################################################%##
png(
  "output_plot/Fg_6.jpg",
  width = 10,
  height = 7,
  units = 'in',
  res = 300
)
op<- par(mfrow=c(2,3), mar=c(5,5,1,1))

mycols<-c('dodgerblue', 'indianred', 'black')
plot(
  Capv_ha_log ~ cwm_SeedMass_log,
  dataset,
  ylab = expression(C[apv]),
  xlab =  expression(CWM[SeedMass]), ylim=c(-1,1),
  type = 'n',  cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0
b <- 0.085
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$cwm_SeedMass_log, na.rm = T),
<<<<<<< HEAD
  x2 = max(dataset$cwm_SeedMass_log, na.rm = T), lty=0
=======
  x2 = max(dataset$cwm_SeedMass_log, na.rm = T)
>>>>>>> 6ac22465f308c794457741913f1c48c88de0f8cb
)

at <- 0.053
bt <- 0.099
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$cwm_SeedMass_log, na.rm = T),
  x2 = max(dataset.t$cwm_SeedMass_log, na.rm = T),col=mycols[1]
)

am <- -0.135
bm <-0.084
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$cwm_SeedMass_log, na.rm = T),
  x2 = max(dataset.m$cwm_SeedMass_log, na.rm = T),col=mycols[2]
)

##%######################################################%##
#                                                          #
####                        modB                        ####
#                                                          #
##%######################################################%##

plot(
  Capv_ha_log ~ cwm_Height_log,
  dataset,
  ylab = expression(C[apv]),
  xlab = expression(CWM[Height]), #ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0.002
b <- 0.240
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$cwm_Height_log, na.rm = T),
<<<<<<< HEAD
  x2 = max(dataset$cwm_Height_log, na.rm = T), lty=0
=======
  x2 = max(dataset$cwm_Height_log, na.rm = T)
>>>>>>> 6ac22465f308c794457741913f1c48c88de0f8cb
)

at <- 0.037
bt <- 0.305
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$cwm_Height_log, na.rm = T),
  x2 = max(dataset.t$cwm_Height_log, na.rm = T),col=mycols[1]
)

am <- -0.052
bm <0.042
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$cwm_Height_log, na.rm = T),
  x2 = max(dataset.m$cwm_Height_log, na.rm = T),col=mycols[2], lty=2
)

##%######################################################%##
#                                                          #
####                        modC                        ####
#                                                          #
##%######################################################%##

plot(
  Capv_ha_log ~ cwm_SLA_log,
  dataset,
  ylab = expression(C[apv]),
  xlab = expression(CWM[SLA]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0.001
b <- -0.004
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$cwm_SLA_log, na.rm = T),
<<<<<<< HEAD
  x2 = max(dataset$cwm_SLA_log, na.rm = T),lty=0#2
=======
  x2 = max(dataset$cwm_SLA_log, na.rm = T),lty=2
>>>>>>> 6ac22465f308c794457741913f1c48c88de0f8cb
)

at <- 0.057
bt <- -0.032
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$cwm_SLA_log, na.rm = T),
  x2 = max(dataset.t$cwm_SLA_log, na.rm = T),col=mycols[1]
)

am <- 0.029
bm <- -0.021
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$cwm_SLA_log, na.rm = T),
  x2 = max(dataset.m$cwm_SLA_log, na.rm = T),col=mycols[2]
)

##%######################################################%##
#                                                          #
####                        modD                        ####
#                                                          #
##%######################################################%##

plot(
  Capv_ha_log ~ cwm_StemDensity_log,
  dataset,
  ylab = expression(C[apv]),
  xlab =  expression(CWM[WD]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0.001
b <-  -0.074
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$cwm_StemDensity_log, na.rm = T),
<<<<<<< HEAD
  x2 = max(dataset$cwm_StemDensity_log, na.rm = T), lty=0
=======
  x2 = max(dataset$cwm_StemDensity_log, na.rm = T)
>>>>>>> 6ac22465f308c794457741913f1c48c88de0f8cb
)

at <- 0.055
bt <- -0.104
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$cwm_StemDensity_log, na.rm = T),
  x2 = max(dataset.t$cwm_StemDensity_log, na.rm = T),col=mycols[1]
)

am <- -0.032
bm <- -0.001
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$cwm_StemDensity_log, na.rm = T),
  x2 = max(dataset.m$cwm_StemDensity_log, na.rm = T),col=mycols[2],lty=2
)

##%######################################################%##
#                                                          #
####                        modE                        ####
#                                                          #
##%######################################################%##

plot(
  Capv_ha_log  ~ cwm_XylemVulnerability_log,
  dataset,
  ylab = expression(C[apv]),
  xlab =  expression(CWM[Xylem]), #ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0
b <-  0.049
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$cwm_XylemVulnerability_log, na.rm = T),
<<<<<<< HEAD
  x2 = max(dataset$cwm_XylemVulnerability_log, na.rm = T), lty=0
=======
  x2 = max(dataset$cwm_XylemVulnerability_log, na.rm = T)
>>>>>>> 6ac22465f308c794457741913f1c48c88de0f8cb
)

at <- 0.057
bt <- 0.001
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$cwm_XylemVulnerability_log, na.rm = T),
  x2 = max(dataset.t$cwm_XylemVulnerability_log, na.rm = T),col=mycols[1], lty=2
)

am <- -0.169
bm <- 0.130
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$cwm_XylemVulnerability_log, na.rm = T),
  x2 = max(dataset.m$cwm_XylemVulnerability_log, na.rm = T),col=mycols[2]
)

##%######################################################%##
#                                                          #
####                        modF                        ####
#                                                          #
##%######################################################%##

plot(
  Capv_ha_log ~ cwm_Dim1_log,
  dataset,
  ylab = expression(C[apv]),
  xlab =   expression(CWM[Dim1]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0
b <- 0.009
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$cwm_Dim1_log, na.rm = T),
<<<<<<< HEAD
  x2 = max(dataset$cwm_Dim1_log, na.rm = T), lty=0#2
=======
  x2 = max(dataset$cwm_Dim1_log, na.rm = T), lty=2
>>>>>>> 6ac22465f308c794457741913f1c48c88de0f8cb
)

at <- 0.05
bt <- 0.014
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$cwm_Dim1_log, na.rm = T),
  x2 = max(dataset.t$cwm_Dim1_log, na.rm = T),col=mycols[1],lty=2
)

am <- -0.123
bm <- 0.015
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$cwm_Dim1_log, na.rm = T),
  x2 = max(dataset.m$cwm_Dim1_log, na.rm = T),col=mycols[2],lty=2
)

par(op)
dev.off()
