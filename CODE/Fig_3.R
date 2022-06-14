##%######################################################%##
#                                                          #
####                        modA                        ####
#                                                          #
##%######################################################%##
png(
  "output_plot/Fig_3.jpg",
  width = 10,
  height = 7,
  units = 'in',
  res = 300
)
op<- par(mfrow=c(2,3), mar=c(5,5,1,1))

mycols<-c('dodgerblue', 'indianred', 'black')
plot(
  FDis_SeedMass_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab =  expression(FDis[SeedMass]),
  type = 'n', ylim=c(-2,2), cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=1.3, box.lty=0, inset=0.02)

a <- parameterEstimates(fit1.a,standardized = TRUE)[7,12]
b <-parameterEstimates(fit1.a,standardized = TRUE)[5,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.a,standardized = TRUE)[5,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.a,standardized = TRUE)[7,14]
bt <- parameterEstimates(fit1mg.a,standardized = TRUE)[5,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.a,standardized = TRUE)[5,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.a,standardized = TRUE)[20,14]
bm <- parameterEstimates(fit1mg.a,standardized = TRUE)[18,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.a,standardized = TRUE)[18,10] <0.05, 1,2)
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

# ----------- Testing slope differences ---------#
library(lsmeans)
model <- function(x){
  bt * x +  at
}
pd.t<-data.frame(pd=model(dataset.t$vpd_log),vpd_log=dataset.t$vpd_log)
pd.t$climate_classification<-paste0(0)
pd.m<-data.frame(pd=model(dataset.m$vpd_log),vpd_log=dataset.m$vpd_log)
pd.m$climate_classification<-paste0(1)
pd<- rbind(pd.t,pd.m)
head(pd)
m.interaction <- lm(pd ~ vpd_log *climate_classification, data = pd)
anova(m.interaction)
# Obtain slopes
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "climate_classification", var="vpd_log")
m.lst
# Compare slopes
pairs(m.lst)
rm(pd.t,pd.m, pd,m.interaction,m.lst)

##%######################################################%##
#                                                          #
####                        modB                        ####
#                                                          #
##%######################################################%##

plot(
  FDis_Height_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab = expression(FDis[Height]),
  type = 'n', ylim=c(-2,2), cex.lab=1.1, cex.axis=1.1
)

a <- parameterEstimates(fit1.b,standardized = TRUE)[12,12]
b <-parameterEstimates(fit1.b,standardized = TRUE)[5,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.b,standardized = TRUE)[5,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.b,standardized = TRUE)[7,14]
bt <- parameterEstimates(fit1mg.b,standardized = TRUE)[5,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.b,standardized = TRUE)[5,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.b,standardized = TRUE)[20,14]
bm <- parameterEstimates(fit1mg.b,standardized = TRUE)[18,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.b,standardized = TRUE)[18,10] <0.05, 1,2)
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

# ----------- Testing slope differences ---------#
library(lsmeans)
model <- function(x){
  bt * x +  at
}
pd.t<-data.frame(pd=model(dataset.t$vpd_log),vpd_log=dataset.t$vpd_log)
pd.t$climate_classification<-paste0(0)
pd.m<-data.frame(pd=model(dataset.m$vpd_log),vpd_log=dataset.m$vpd_log)
pd.m$climate_classification<-paste0(1)
pd<- rbind(pd.t,pd.m)
head(pd)
m.interaction <- lm(pd ~ vpd_log *climate_classification, data = pd)
anova(m.interaction)
# Obtain slopes
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "climate_classification", var="vpd_log")
m.lst
# Compare slopes
pairs(m.lst)
rm(pd.t,pd.m, pd,m.interaction,m.lst)

##%######################################################%##
#                                                          #
####                        modC                        ####
#                                                          #
##%######################################################%##

plot(
  FDis_SLA_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab = expression(FDis[SLA]),
  type = 'n', ylim=c(-2,2), cex.lab=1.1, cex.axis=1.1
)

a <- parameterEstimates(fit1.c,standardized = TRUE)[12,12]
b <-parameterEstimates(fit1.c,standardized = TRUE)[5,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.c,standardized = TRUE)[5,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.c,standardized = TRUE)[7,14]
bt <- parameterEstimates(fit1mg.c,standardized = TRUE)[5,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.c,standardized = TRUE)[5,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.c,standardized = TRUE)[20,14]
bm <- parameterEstimates(fit1mg.c,standardized = TRUE)[18,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.c,standardized = TRUE)[18,10] <0.05, 1,2)
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

# ----------- Testing slope differences ---------#
library(lsmeans)
model <- function(x){
  bt * x +  at
}
pd.t<-data.frame(pd=model(dataset.t$vpd_log),vpd_log=dataset.t$vpd_log)
pd.t$climate_classification<-paste0(0)
pd.m<-data.frame(pd=model(dataset.m$vpd_log),vpd_log=dataset.m$vpd_log)
pd.m$climate_classification<-paste0(1)
pd<- rbind(pd.t,pd.m)
head(pd)
m.interaction <- lm(pd ~ vpd_log *climate_classification, data = pd)
anova(m.interaction)
# Obtain slopes
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "climate_classification", var="vpd_log")
m.lst
# Compare slopes
pairs(m.lst)
rm(pd.t,pd.m, pd,m.interaction,m.lst)

##%######################################################%##
#                                                          #
####                        modD                        ####
#                                                          #
##%######################################################%##

plot(
  FDis_StemDensity_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab =  expression(FDis[WD]),
  type = 'n', ylim=c(-2,2), cex.lab=1.1, cex.axis=1.1
)

a <- parameterEstimates(fit1.d,standardized = TRUE)[12,12]
b <-parameterEstimates(fit1.d,standardized = TRUE)[5,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.d,standardized = TRUE)[5,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.d,standardized = TRUE)[7,14]
bt <- parameterEstimates(fit1mg.d,standardized = TRUE)[5,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.d,standardized = TRUE)[5,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.d,standardized = TRUE)[20,14]
bm <- parameterEstimates(fit1mg.d,standardized = TRUE)[18,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.d,standardized = TRUE)[18,10] <0.05, 1,2)
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

# ----------- Testing slope differences ---------#
library(lsmeans)
model <- function(x){
  bt * x +  at
}
pd.t<-data.frame(pd=model(dataset.t$vpd_log),vpd_log=dataset.t$vpd_log)
pd.t$climate_classification<-paste0(0)
pd.m<-data.frame(pd=model(dataset.m$vpd_log),vpd_log=dataset.m$vpd_log)
pd.m$climate_classification<-paste0(1)
pd<- rbind(pd.t,pd.m)
head(pd)
m.interaction <- lm(pd ~ vpd_log *climate_classification, data = pd)
anova(m.interaction)
# Obtain slopes
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "climate_classification", var="vpd_log")
m.lst
# Compare slopes
pairs(m.lst)
rm(pd.t,pd.m, pd,m.interaction,m.lst)

##%######################################################%##
#                                                          #
####                        modE                        ####
#                                                          #
##%######################################################%##

plot(
  FDis_XylemVulnerability_log  ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab =  expression(FDis[Xylem]),
  type = 'n', ylim=c(-2,2), cex.lab=1.1, cex.axis=1.1
)

a <- parameterEstimates(fit1.e,standardized = TRUE)[12,12]
b <-parameterEstimates(fit1.e,standardized = TRUE)[5,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.e,standardized = TRUE)[5,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.e,standardized = TRUE)[7,14]
bt <- parameterEstimates(fit1mg.e,standardized = TRUE)[5,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.e,standardized = TRUE)[5,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.e,standardized = TRUE)[20,14]
bm <- parameterEstimates(fit1mg.e,standardized = TRUE)[18,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.e,standardized = TRUE)[18,10] <0.05, 1,2)
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

# ----------- Testing slope differences ---------#
library(lsmeans)
model <- function(x){
  bt * x +  at
}
pd.t<-data.frame(pd=model(dataset.t$vpd_log),vpd_log=dataset.t$vpd_log)
pd.t$climate_classification<-paste0(0)
pd.m<-data.frame(pd=model(dataset.m$vpd_log),vpd_log=dataset.m$vpd_log)
pd.m$climate_classification<-paste0(1)
pd<- rbind(pd.t,pd.m)
head(pd)
m.interaction <- lm(pd ~ vpd_log *climate_classification, data = pd)
anova(m.interaction)
# Obtain slopes
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "climate_classification", var="vpd_log")
m.lst
# Compare slopes
pairs(m.lst)
rm(pd.t,pd.m, pd,m.interaction,m.lst)

##%######################################################%##
#                                                          #
####                        modF                        ####
#                                                          #
##%######################################################%##

plot(
  FDis_All_log ~ vpd_log,
  dataset,
  xlab = 'VPD',
  ylab =   expression(FDis[all]),
  type = 'n', ylim=c(-2,2), cex.lab=1.1, cex.axis=1.1
)
a <- parameterEstimates(fit1.f,standardized = TRUE)[12,12]
b <-parameterEstimates(fit1.f,standardized = TRUE)[5,12]
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=ifelse(
    parameterEstimates(fit1.f,standardized = TRUE)[5,8] <0.05, 1,2)
)

at <- parameterEstimates(fit1mg.f,standardized = TRUE)[7,14]
bt <- parameterEstimates(fit1mg.f,standardized = TRUE)[5,14]
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1], lty=ifelse(
    parameterEstimates(fit1mg.f,standardized = TRUE)[5,10] <0.05, 1,2)
)

am <- parameterEstimates(fit1mg.f,standardized = TRUE)[20,14]
bm <- parameterEstimates(fit1mg.f,standardized = TRUE)[18,14]
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=ifelse(
    parameterEstimates(fit1mg.f,standardized = TRUE)[18,10] <0.05, 1,2)
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

# ----------- Testing slope differences ---------#
library(lsmeans)
model <- function(x){
  bt * x +  at
}
pd.t<-data.frame(pd=model(dataset.t$vpd_log),vpd_log=dataset.t$vpd_log)
pd.t$climate_classification<-paste0(0)
pd.m<-data.frame(pd=model(dataset.m$vpd_log),vpd_log=dataset.m$vpd_log)
pd.m$climate_classification<-paste0(1)
pd<- rbind(pd.t,pd.m)
head(pd)
m.interaction <- lm(pd ~ vpd_log *climate_classification, data = pd)
anova(m.interaction)
# Obtain slopes
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "climate_classification", var="vpd_log")
m.lst
# Compare slopes
pairs(m.lst)
rm(pd.t,pd.m, pd,m.interaction,m.lst)


par(op)
dev.off()
