.temp <-
  dataset[, c(
    'SpRich',
    'FDis_All',
    'FDis_SeedMass',
    'FDis_Height',
    'FDis_SLA',
    'FDis_StemDensity',
    'FDis_XylemVulnerability',
    #'cwm_SeedMass','cwm_Height','cwm_SLA',
    'cwm_StemDensity',
    'cwm_XylemVulnerability',
    'cwm_Dim1',
    'cwm_Dim2'
  )]


head(dataset$SpRich)


hist(.temp$SpRich)
hist(.temp$FDis_All)
hist(.temp$FDis_SeedMass)
hist(.temp$FDis_Height)
hist(.temp$FDis_SLA)
hist(.temp$FDis_StemDensity)
hist(.temp$FDis_XylemVulnerability)


.temp$SpRich_log <-
  log(.temp$SpRich + (1 - min(.temp$SpRich, na.rm = T)))
hist(.temp$SpRich_log)

.temp$FDis_All_log <-
  log(.temp$FDis_All + (1 - min(.temp$FDis_All, na.rm = T)))
hist(.temp$SpRich_log)

.temp$FDis_SeedMass_log <-
  log(.temp$FDis_SeedMass + (1 - min(.temp$FDis_SeedMass, na.rm = T)))
hist(.temp$FDis_SeedMass_log)

.temp$FDis_Height_log <-
  log(.temp$FDis_Height + (1 - min(.temp$FDis_Height, na.rm = T)))
hist(.temp$FDis_Height_log)

.temp$FDis_SLA_log <-
  log(.temp$FDis_SLA + (1 - min(.temp$FDis_SLA, na.rm = T)))
hist(.temp$FDis_SLA_log)

.temp$FDis_StemDensity_log <-
  log(.temp$FDis_StemDensity + (1 - min(.temp$FDis_Stem, na.rm = T)))
hist(.temp$FDis_StemDensity_log)

.temp$FDis_XylemVulnerability_log <-
  log(.temp$FDis_XylemVulnerability + (1 - min(.temp$FDis_XylemVulnerability, na.rm = T)))
hist(.temp$FDis_XylemVulnerability_log)


cor.test(.temp$SpRich_log, .temp$FDis_All_log)
cor.test(.temp$SpRich_log, .temp$FDis_Height_log)
cor.test(.temp$SpRich_log, .temp$FDis_SeedMass_log)
cor.test(.temp$SpRich_log, .temp$FDis_SLA_log)
cor.test(.temp$SpRich_log, .temp$FDis_StemDensity_log)
cor.test(.temp$SpRich_log, .temp$FDis_XylemVulnerability_log)


png(
  "output_plot/SuppMat.jpg",
  width = 6,
  height = 10,
  units = 'in',
  res = 300
)
op<- par(mfrow=c(3,2))

plot(SpRich_log~FDis_SeedMass_log, .temp, ylab='Log(Species richness)', xlab=expression(Log(FDis[SeedMass])))
mod<-lm(SpRich_log~FDis_SeedMass_log, .temp)
mods<-summary(mod)
mods$r.squared
mods$fstatistic
abline(mod, col='blue')
usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'a)',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)

plot(SpRich_log~FDis_Height_log, .temp, ylab='Log(Species richness)', xlab=expression(Log(FDis[Height])))
mod<-lm(SpRich_log~FDis_Height_log, .temp)
mods<-summary(mod)
mods$r.squared
mods$fstatistic
abline(mod, col='blue')
usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'b)',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)
plot(SpRich_log~FDis_SLA_log, .temp, ylab='Log(Species richness)', xlab=expression(Log(FDis[SLA])))
mod<-lm(SpRich_log~FDis_SLA_log, .temp)
mods<-summary(mod)
mods$r.squared
mods$fstatistic
abline(mod, col='blue')
usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'c)',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)
plot(SpRich_log~FDis_StemDensity_log, .temp, ylab='Log(Species richness)', xlab=expression(Log(FDis[WD])))
mod<-lm(SpRich_log~FDis_StemDensity_log, .temp)
mods<-summary(mod)
mods$r.squared
mods$fstatistic
abline(mod, col='blue')
usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'd)',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)
plot(SpRich_log~FDis_XylemVulnerability_log, .temp, ylab='Log(Species richness)', xlab=expression(Log(FDis[Xylem])))
mod<-lm(SpRich_log~FDis_XylemVulnerability_log, .temp)
mods<-summary(mod)
mods$r.squared
mods$fstatistic
abline(mod, col='blue')
usr <- par("usr")
text(
  usr[2],
  usr[4],
  labels = 'e)',
  adj = c(1.5, 1.5),
  col = 'black',
  cex = 1
)
plot(SpRich_log~FDis_All_log, .temp, ylab='Log(Species richness)', xlab=expression(Log(FDis[all])))
mod<-lm(SpRich_log~FDis_All_log, .temp)
mods<-summary(mod)
mods$r.squared
mods$fstatistic
abline(mod, col='blue')
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

