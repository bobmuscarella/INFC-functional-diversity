##%######################################################%##
#                                                          #
####                        modA                        ####
#                                                          #
##%######################################################%##
png(
  "output_plot/Fig_5.jpg",
  width = 10,
  height = 7,
  units = 'in',
  res = 300
)
op<- par(mfrow=c(2,3), mar=c(5,5,1,1))

mycols<-c('dodgerblue', 'indianred', 'black')
plot(
  ICCapv_ha_log ~ FDis_SeedMass_log,
  dataset,
  ylab = expression(C[cai]),
  xlab =  expression(FDis[SeedMass]), ylim=c(-1,1),
  type = 'n',  cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=1.3, box.lty=0, inset=0.02)

a <- parameterEstimates(fit1.a,standardized = TRUE)[6,12]
b <-parameterEstimates(fit1.a,standardized = TRUE)[3,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_SeedMass_log, na.rm = T),
  x2 = max(dataset$FDis_SeedMass_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.a,standardized = TRUE)[3,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.a,standardized = TRUE)[6,14]
bt <- parameterEstimates(fit1mg.a,standardized = TRUE)[3,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_SeedMass_log, na.rm = T),
  x2 = max(dataset.t$FDis_SeedMass_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.a,standardized = TRUE)[3,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.a,standardized = TRUE)[19,14]
bm <- parameterEstimates(fit1mg.a,standardized = TRUE)[16,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_SeedMass_log, na.rm = T),
  x2 = max(dataset.m$FDis_SeedMass_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.a,standardized = TRUE)[16,10] <0.05, 1,2)
)

usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'Mod.A',
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
  ICCapv_ha_log ~ FDis_Height_log,
  dataset,
  ylab = expression(C[cai]),
  xlab = expression(FDis[Height]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
a <- parameterEstimates(fit1.b,standardized = TRUE)[10,12]
b <-parameterEstimates(fit1.b,standardized = TRUE)[3,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_Height_log, na.rm = T),
  x2 = max(dataset$FDis_Height_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.b,standardized = TRUE)[3,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.b,standardized = TRUE)[6,14]
bt <- parameterEstimates(fit1mg.b,standardized = TRUE)[3,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_Height_log, na.rm = T),
  x2 = max(dataset.t$FDis_Height_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.b,standardized = TRUE)[3,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.b,standardized = TRUE)[19,14]
bm <- parameterEstimates(fit1mg.b,standardized = TRUE)[16,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_Height_log, na.rm = T),
  x2 = max(dataset.m$FDis_Height_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.b,standardized = TRUE)[16,10] <0.05, 1,2)
)

usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'Mod.B',
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
  ICCapv_ha_log ~ FDis_SLA_log,
  dataset,
  ylab = expression(C[cai]),
  xlab = expression(FDis[SLA]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
a <- parameterEstimates(fit1.c,standardized = TRUE)[10,12]
b <-parameterEstimates(fit1.c,standardized = TRUE)[3,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_SLA_log, na.rm = T),
  x2 = max(dataset$FDis_SLA_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.c,standardized = TRUE)[3,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.c,standardized = TRUE)[6,14]
bt <- parameterEstimates(fit1mg.c,standardized = TRUE)[3,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_SLA_log, na.rm = T),
  x2 = max(dataset.t$FDis_SLA_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.c,standardized = TRUE)[3,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.c,standardized = TRUE)[19,14]
bm <- parameterEstimates(fit1mg.c,standardized = TRUE)[16,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_SLA_log, na.rm = T),
  x2 = max(dataset.m$FDis_SLA_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.c,standardized = TRUE)[16,10] <0.05, 1,2)
)

usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'Mod.C',
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
  ICCapv_ha_log ~ FDis_StemDensity_log,
  dataset,
  ylab = expression(C[cai]),
  xlab =  expression(FDis[WD]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
a <- parameterEstimates(fit1.d,standardized = TRUE)[10,12]
b <-parameterEstimates(fit1.d,standardized = TRUE)[3,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_StemDensity_log, na.rm = T),
  x2 = max(dataset$FDis_StemDensity_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.d,standardized = TRUE)[3,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.d,standardized = TRUE)[6,14]
bt <- parameterEstimates(fit1mg.d,standardized = TRUE)[3,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_StemDensity_log, na.rm = T),
  x2 = max(dataset.t$FDis_StemDensity_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.d,standardized = TRUE)[3,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.d,standardized = TRUE)[19,14]
bm <- parameterEstimates(fit1mg.d,standardized = TRUE)[16,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_StemDensity_log, na.rm = T),
  x2 = max(dataset.m$FDis_StemDensity_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.d,standardized = TRUE)[16,10] <0.05, 1,2)
)

usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'Mod.D',
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
  ICCapv_ha_log  ~ FDis_XylemVulnerability_log,
  dataset,
  ylab = expression(C[cai]),
  xlab =  expression(FDis[Xylem]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
a <- parameterEstimates(fit1.e,standardized = TRUE)[10,12]
b <-parameterEstimates(fit1.e,standardized = TRUE)[3,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_XylemVulnerability_log, na.rm = T),
  x2 = max(dataset$FDis_XylemVulnerability_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.e,standardized = TRUE)[3,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.e,standardized = TRUE)[6,14]
bt <- parameterEstimates(fit1mg.e,standardized = TRUE)[3,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_XylemVulnerability_log, na.rm = T),
  x2 = max(dataset.t$FDis_XylemVulnerability_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.e,standardized = TRUE)[3,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.e,standardized = TRUE)[19,14]
bm <- parameterEstimates(fit1mg.e,standardized = TRUE)[16,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_XylemVulnerability_log, na.rm = T),
  x2 = max(dataset.m$FDis_XylemVulnerability_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.e,standardized = TRUE)[16,10] <0.05, 1,2)
)


usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'Mod.E',
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
  ICCapv_ha_log ~ FDis_All_log,
  dataset,
  ylab = expression(C[cai]),
  xlab =   expression(FDis[all]), ylim=c(-1,1),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
a <- parameterEstimates(fit1.f,standardized = TRUE)[10,12]
b <-parameterEstimates(fit1.f,standardized = TRUE)[3,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$FDis_All_log, na.rm = T),
  x2 = max(dataset$FDis_All_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.f,standardized = TRUE)[3,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.f,standardized = TRUE)[6,14]
bt <- parameterEstimates(fit1mg.f,standardized = TRUE)[3,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$FDis_All_log, na.rm = T),
  x2 = max(dataset.t$FDis_All_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.f,standardized = TRUE)[3,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.f,standardized = TRUE)[19,14]
bm <- parameterEstimates(fit1mg.f,standardized = TRUE)[16,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$FDis_All_log, na.rm = T),
  x2 = max(dataset.m$FDis_All_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.f,standardized = TRUE)[16,10] <0.05, 1,2)
)

usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'Mod.F',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)
par(op)
dev.off()
