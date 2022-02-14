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
hist(.temp$FDis_XylemVulnerability)


.temp$SpRich_log <-
  log(.temp$SpRich + (1 - min(.temp$SpRich, na.rm = T)))
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

.temp$FDis_XylemVulnerability_log <-
  log(.temp$FDis_XylemVulnerability + (1 - min(.temp$FDis_XylemVulnerability, na.rm = T)))
hist(.temp$FDis_XylemVulnerability_log)


png(
  "output_plot/SuppMat.jpg",
  width = 10,
  height = 10,
  units = 'in',
  res = 300
)
op<- par(mfrow=c(2,2))
plot(SpRich_log~FDis_SeedMass_log, .temp)
mod<-lm(SpRich_log~FDis_SeedMass_log, .temp)
mods<-summary(mod)
mods$r.squared
mods$fstatistic
abline(mod, col='blue')

plot(SpRich_log~FDis_Height_log, .temp)
mod<-lm(SpRich_log~FDis_Height_log, .temp)
mods<-summary(mod)
mods$r.squared
mods$fstatistic
abline(mod, col='blue')

plot(SpRich_log~FDis_SLA_log, .temp)
mod<-lm(SpRich_log~FDis_SLA_log, .temp)
mods<-summary(mod)
mods$r.squared
mods$fstatistic
abline(mod, col='blue')

plot(SpRich_log~FDis_XylemVulnerability_log, .temp)
mod<-lm(SpRich_log~FDis_XylemVulnerability_log, .temp)
mods<-summary(mod)
mods$r.squared
mods$fstatistic
abline(mod, col='blue')

par(op)
dev.off()

