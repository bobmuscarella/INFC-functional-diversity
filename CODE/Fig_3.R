##%######################################################%##
#                                                          #
####                        modA                        ####
#                                                          #
##%######################################################%##
png(
  "output_plot/Fg_3.jpg",
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
  type = 'n', ylim=c(-1.5,2), cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- -0.001
b <- 0.063
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T)
)

at <- 0.118
bt <- 0.171
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)


am <- -0.304
bm <- -0.015
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=2
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
  type = 'n', ylim=c(-1.5,2), cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- -0.001
b <- 0.010
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=2
)

at <- 0.080
bt <- 0.082
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)

am <- -0.223
bm <- 0.001
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=2
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
  type = 'n', ylim=c(-1.5,2), cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- -0.004
b <- -0.191
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T)
)

at <- 0.088
bt <- -0.112
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)

am <- -0.685
bm <- 0.006
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=2
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
  type = 'n', ylim=c(-1.5,2), cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- -0.002
b <-  -0.016
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=2
)

at <- 0.112
bt <- 0.091
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)

am <- -0.308
bm <- -0.064
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2]
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
  type = 'n', ylim=c(-1.5,2), cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- 0.002
b <-  0.160
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T)
)

at <- 0.051
bt <- 0.207
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)

am <- -0.136
bm <- 0.095
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2]
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
  type = 'n', cex.lab=1.1, cex.axis=1.1
)
legend('topleft', legend=c("Temperate", "Mediterranean", "Pooled"),
       col=mycols, lty=1, cex=0.9, box.lty=0, inset=0.02)

a <- -0.001
b <-  0.005
plotrix::ablineclip(
  a = a,
  b = b,
  x1 = min(dataset$vpd_log, na.rm = T),
  x2 = max(dataset$vpd_log, na.rm = T), lty=2
)

at <- 0.988
bt <- 0.112
dataset.t<- subset(dataset,climate_classification==0)
plotrix::ablineclip(
  a = at,
  b = bt,
  x1 = min(dataset.t$vpd_log, na.rm = T),
  x2 = max(dataset.t$vpd_log, na.rm = T),col=mycols[1]
)

am <- -0.366
bm <- -0.005
dataset.m<- subset(dataset,climate_classification==1)
plotrix::ablineclip(
  a = am,
  b = bm,
  x1 = min(dataset.m$vpd_log, na.rm = T),
  x2 = max(dataset.m$vpd_log, na.rm = T),col=mycols[2], lty=2
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
