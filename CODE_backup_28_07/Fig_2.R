##%######################################################%##
#                                                          #
####                        modA                        ####
#                                                          #
##%######################################################%##
# png(
#   "output_plot/Fig_2.jpg",
#   width = 10,
#   height = 7,
#   units = 'in',
#   res = 300
# )
pdf('output_plot/Fig_2.pdf', width = 10, height = 6.5)

op<- par(mfrow=c(2,3), mar=c(5,5,1,1))

mycols<-c('dodgerblue', 'indianred', 'black')
plot(
  cwm_SeedMass_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab = expression(CWM[SeedMass]),
  type = 'n', ylim=c(-2,2), cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=1.3, box.lty=0, inset=0.02)

a <- parameterEstimates(fit1.a,standardized = TRUE)[8,12]
b <-parameterEstimates(fit1.a,standardized = TRUE)[4,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.a,standardized = TRUE)[4,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.a,standardized = TRUE)[8,14]
bt <- parameterEstimates(fit1mg.a,standardized = TRUE)[4,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.a,standardized = TRUE)[4,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.a,standardized = TRUE)[21,14]
bm <- parameterEstimates(fit1mg.a,standardized = TRUE)[17,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.a,standardized = TRUE)[17,10] <0.05, 1,2)
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
  cwm_Height_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab = expression(CWM[Height]),
  type = 'n', ylim=c(-2,2), cex.lab=1.1, cex.axis=1.1
)

a <- parameterEstimates(fit1.b,standardized = TRUE)[11,12]
b <-parameterEstimates(fit1.b,standardized = TRUE)[4,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.b,standardized = TRUE)[4,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.b,standardized = TRUE)[8,14]
bt <- parameterEstimates(fit1mg.b,standardized = TRUE)[4,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.b,standardized = TRUE)[4,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.b,standardized = TRUE)[21,14]
bm <- parameterEstimates(fit1mg.b,standardized = TRUE)[17,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.b,standardized = TRUE)[17,10] <0.05, 1,2)
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
  cwm_SLA_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab = expression(CWM[SLA]),
  type = 'n', ylim=c(-2,2), cex.lab=1.1, cex.axis=1.1
)

a <- parameterEstimates(fit1.c,standardized = TRUE)[11,12]
b <-parameterEstimates(fit1.c,standardized = TRUE)[4,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.c,standardized = TRUE)[4,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.c,standardized = TRUE)[8,14]
bt <- parameterEstimates(fit1mg.c,standardized = TRUE)[4,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.c,standardized = TRUE)[4,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.c,standardized = TRUE)[21,14]
bm <- parameterEstimates(fit1mg.c,standardized = TRUE)[17,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.c,standardized = TRUE)[17,10] <0.05, 1,2)
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
  cwm_StemDensity_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab =  expression(CWM[WD]),
  type = 'n', cex.lab=1.1, cex.axis=1.1
)

a <- parameterEstimates(fit1.d,standardized = TRUE)[11,12]
b <-parameterEstimates(fit1.d,standardized = TRUE)[4,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.d,standardized = TRUE)[4,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.d,standardized = TRUE)[8,14]
bt <- parameterEstimates(fit1mg.d,standardized = TRUE)[4,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.d,standardized = TRUE)[4,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.d,standardized = TRUE)[21,14]
bm <- parameterEstimates(fit1mg.d,standardized = TRUE)[17,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.d,standardized = TRUE)[17,10] <0.05, 1,2)
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
  cwm_XylemVulnerability_log  ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab =  expression(CWM[Xylem]),
  type = 'n', ylim=c(-2,2), cex.lab=1.1, cex.axis=1.1
)

a <- parameterEstimates(fit1.e,standardized = TRUE)[11,12]
b <-parameterEstimates(fit1.e,standardized = TRUE)[4,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.e,standardized = TRUE)[4,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.e,standardized = TRUE)[8,14]
bt <- parameterEstimates(fit1mg.e,standardized = TRUE)[4,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.e,standardized = TRUE)[4,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.e,standardized = TRUE)[21,14]
bm <- parameterEstimates(fit1mg.e,standardized = TRUE)[17,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.e,standardized = TRUE)[17,10] <0.05, 1,2)
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
  cwm_Dim1_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab =   expression(CWM[Dim1]),
  type = 'n', ylim=c(-2,2),cex.lab=1.1, cex.axis=1.1
)

a <- parameterEstimates(fit1.f,standardized = TRUE)[11,12]
b <-parameterEstimates(fit1.f,standardized = TRUE)[4,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.f,standardized = TRUE)[4,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.f,standardized = TRUE)[8,14]
bt <- parameterEstimates(fit1mg.f,standardized = TRUE)[4,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.f,standardized = TRUE)[4,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.f,standardized = TRUE)[21,14]
bm <- parameterEstimates(fit1mg.f,standardized = TRUE)[17,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.f,standardized = TRUE)[17,10] <0.05, 1,2)
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

