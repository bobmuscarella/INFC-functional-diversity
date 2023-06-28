# usage
packages <- c('lavaan','semPlot','semTools', 'semptools','semTable','dplyr','piecewiseSEM')
ipak(packages)

##%######################################################%##
#                                                          #
####                       Mod. A                       ####
#                                                          #
##%######################################################%##
dataset$ICCapv_ha_log <-
  scale(log(dataset$ICCapv_ha + (1 - min(dataset$ICCapv_ha, na.rm = T))), center = TRUE)
hist(dataset$ICCapv_ha_log)

dataset$Capv_ha_log <-
  scale(log(dataset$Capv_ha + (1 - min(dataset$Capv_ha, na.rm = T))), center = TRUE)
hist(dataset$Capv_ha_log)

dataset$Capm_ha_log <-
  scale(log(dataset$Capm_ha + (1 - min(dataset$Capm_ha, na.rm = T))), center = TRUE)
hist(dataset$Capm_ha_log)

dataset$cwm_SeedMass_log <-
  scale(dataset$cwm_SeedMass_log, center = TRUE)
hist(dataset$cwm_SeedMass_log)

dataset$FDis_SeedMass_log <-
  scale(log(dataset$FDis_SeedMass + (1 - min(dataset$FDis_SeedMass, na.rm = T))), center = TRUE)
hist(dataset$FDis_SeedMass_log)

dataset$vpd_log <-
  scale(log(dataset$vpd + (1 - min(dataset$vpd, na.rm = T))), center = TRUE)
hist(dataset$vpd_log)

modA <- '
# Direct effect
ICCapv_ha_log ~ c*vpd_log

# Mediator effect
ICCapv_ha_log ~ d * cwm_SeedMass_log
ICCapv_ha_log ~ e * FDis_SeedMass_log
cwm_SeedMass_log ~ a * vpd_log
FDis_SeedMass_log ~ b * vpd_log

# Indirect effects
be := b * e # The indirect (i.e., Mediator) effect of vpd_log and FDIs on Capv id the product of the mediator coefficient (b*e)
ad := a * d # The indirect (i.e., Mediator) effect of vpd_log and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total := c + (b * e) + (a * d)

# Observed means
ICCapv_ha_log ~ 1
FDis_SeedMass_log ~ 1
cwm_SeedMass_log ~ 1
vpd_log ~ 1
'

# datasetscaled<- dataset %>% mutate_at(vars(ICCapv_ha_log, FDis_SeedMass_log, cwm_SeedMass_log, vpd), funs(scale))
fit1.a = lavaan::cfa(
  modA,
  fixed.x = F,
  data = dataset,
  estimator = "MLR",
  likelihood = "wishart",
  missing = "FIML",
  std.lv = TRUE
) # , se = "bootstrap"
summary(fit1.a, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
semPaths(fit1.a,whatLabels = "std", layout = "tree2", intercepts = F, nCharNodes = 7, sizeMan = 5,sizeLat = 8,curvePivot = T,ask = F)
parameterEstimates(fit1.a)
modificationIndices(fit1.a, minimum.value = 10)
ft<-data.frame(t(as.matrix(fitMeasures(fit1.a))))
cfi_moda<-ft$cfi
tli_moda<-ft$tli
rmsea_moda<-ft$rmsea
rmsea.upp_moda<-ft$rmsea.ci.upper
rmsea.low_moda<-ft$rmsea.ci.upper
srmr_moda<-ft$srmr


library(semPlot)
m <- matrix(
  c(NA, 'vpd_log',  NA,
    'cwm_SeedMass_log', NA, "FDis_SeedMass_log",
    NA, "ICCapv_ha_log", NA),
  byrow = TRUE,
  3, 3)
p_pa <- semPaths(
  fit1.a,
  whatLabels = "std",
  intercepts = F,
  sizeMan = 10,
  edge.label.cex = 1.15,
  style = "ram",
  nCharNodes = 0,
  nCharEdges = 0,
  layout = m, 
  fade=F, DoNotPlot = T
)
p_pa$graphAttributes$Nodes$labels
p_pa$graphAttributes$Nodes$labels <-
  c(list(
    expression(Cai),
    expression(CWM[SeedMass]),
    expression(FDis[SeedMass]),
    expression(VPD)
  ))
p_pa2_moda <- mark_sig(p_pa, fit1.a)
plot(p_pa2_moda)


##%######################################################%##
#                                                          #
####                       Mod. B                       ####
#                                                          #
##%######################################################%##
dataset$cwm_Height_log <-
  scale(dataset$cwm_Height_log, center = TRUE)
hist(dataset$cwm_Height_log)

dataset$FDis_Height_log <-
  scale(log(dataset$FDis_Height + (1 - min(dataset$FDis_Height, na.rm = T))), center = TRUE)
hist(dataset$FDis_Height)



modB <- '
# Direct effect
ICCapv_ha_log ~ c*vpd_log

# Mediator effect
ICCapv_ha_log ~ d*cwm_Height_log
ICCapv_ha_log ~ e*FDis_Height_log
cwm_Height_log ~ a*vpd_log
FDis_Height_log ~ b*vpd_log

# Indirect effects
be:=b*e # The indirect (i.e., Mediator) effect of vpd_log and FDIs on Capv id the product of the mediator coefficient (b*e)
ad:=a*d # The indirect (i.e., Mediator) effect of vpd_log and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total:=c+(b*e)+(a*d)
'

fit1.b = lavaan::cfa(
  modB,
  fixed.x = F,
  data = dataset,
  estimator = "MLR",
  likelihood = "wishart",
  missing = "FIML",
  std.lv = TRUE
) # , se = "bootstrap"
summary(fit1.b, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit1.b)
ft<-data.frame(t(as.matrix(fitMeasures(fit1.b))))
cfi_modb<-ft$cfi
tli_modb<-ft$tli
rmsea_modb<-ft$rmsea
rmsea.upp_modb<-ft$rmsea.ci.upper
rmsea.low_modb<-ft$rmsea.ci.upper
srmr_modb<-ft$srmr

library(semPlot)
m <- matrix(
  c(NA, 'vpd_log',  NA,
    'cwm_Height_log', NA, "FDis_Height_log",
    NA, "ICCapv_ha_log", NA),
  byrow = TRUE,
  3, 3)

p_pa <- semPaths(
  fit1.b,
  whatLabels = "std",
  intercepts = F,
  sizeMan = 10,
  edge.label.cex = 1.15,
  style = "ram",
  nCharNodes = 0,
  nCharEdges = 0,
  layout = m, DoNotPlot = T
)
p_pa$graphAttributes$Nodes$labels
p_pa$graphAttributes$Nodes$labels <-
  c(list(
    expression(Cai),
    expression(CWM[Height]),
    expression(FDis[Height]),
    expression(VPD)
  ))
library(semptools)
p_pa2_modb <- mark_sig(p_pa, fit1.b)
plot(p_pa2_modb)



##%######################################################%##
#                                                          #
####                       Mod. C                       ####
#                                                          #
##%######################################################%##
dataset$cwm_SLA_log <-
  scale(dataset$cwm_SLA_log, center = TRUE)
hist(dataset$cwm_SLA_log)

dataset$FDis_SLA_log <-
  scale(log(dataset$FDis_SLA + (1 - min(dataset$FDis_SLA, na.rm = T))), center = TRUE)
hist(dataset$FDis_SLA_log)


modC <- '
# Direct effect
ICCapv_ha_log ~ c*vpd_log

# Mediator effect
ICCapv_ha_log ~ d*cwm_SLA_log
ICCapv_ha_log ~ e*FDis_SLA_log
cwm_SLA_log ~ a*vpd_log
FDis_SLA_log ~ b*vpd_log

# Indirect effects
be:=b*e # The indirect (i.e., Mediator) effect of vpd_log and FDIs on Capv id the product of the mediator coefficient (b*e)
ad:=a*d # The indirect (i.e., Mediator) effect of vpd_log and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total:=c+(b*e)+(a*d)
'

fit1.c = lavaan::cfa(
  modC,
  fixed.x = F,
  data = dataset,
  estimator = "MLR",
  likelihood = "wishart",
  missing = "FIML",
  std.lv = TRUE
) # , se = "bootstrap"
summary(fit1.c, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit1.c)
ft<-data.frame(t(as.matrix(fitMeasures(fit1.c))))
cfi_modc<-ft$cfi
tli_modc<-ft$tli
rmsea_modc<-ft$rmsea
rmsea.upp_modc<-ft$rmsea.ci.upper
rmsea.low_modc<-ft$rmsea.ci.upper
srmr_modc<-ft$srmr


library(semPlot)
m <- matrix(
  c(NA, 'vpd_log',  NA,
    'cwm_SLA_log', NA, "FDis_SLA_log",
    NA, "ICCapv_ha_log", NA),
  byrow = TRUE,
  3, 3)

p_pa <- semPaths(
  fit1.c,
  whatLabels = "std",
  intercepts = F,
  sizeMan = 10,
  edge.label.cex = 1.15,
  style = "ram",
  nCharNodes = 0,
  nCharEdges = 0,
  layout = m, DoNotPlot = T
)
p_pa$graphAttributes$Nodes$labels
p_pa$graphAttributes$Nodes$labels <-
  c(list(
    expression(Cai),
        expression(CWM[SLA]),
expression(FDis[SLA]),
    expression(VPD)
  ))
library(semptools)
p_pa2_modc <- mark_sig(p_pa, fit1.c)
plot(p_pa2_modc)


##%######################################################%##
#                                                          #
####                       Mod. D                       ####
#                                                          #
##%######################################################%##
dataset$FDis_StemDensity_log <-
  scale(log(dataset$FDis_StemDensity + (1 - min(dataset$FDis_StemDensity, na.rm = T))), center = TRUE)
hist(dataset$FDis_StemDensity_log)

dataset$cwm_StemDensity_log <-
  scale(log(dataset$cwm_StemDensity + (1 - min(dataset$cwm_StemDensity, na.rm = T))), center = TRUE)
hist(dataset$cwm_StemDensity_log)

modD <- '
# Direct effect
ICCapv_ha_log ~ c*vpd_log

# Mediator effect
ICCapv_ha_log ~ d*cwm_StemDensity_log
ICCapv_ha_log ~ e*FDis_StemDensity_log
cwm_StemDensity_log ~ a*vpd_log
FDis_StemDensity_log ~ b*vpd_log

# Indirect effects
be:=b*e # The indirect (i.e., Mediator) effect of vpd_log and FDIs on Capv id the product of the mediator coefficient (b*e)
ad:=a*d # The indirect (i.e., Mediator) effect of vpd_log and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total:=c+(b*e)+(a*d)
'


fit1.d = lavaan::cfa(
  modD,
  fixed.x = F,
  data = dataset,
  estimator = "MLR",
  likelihood = "wishart",
  missing = "FIML",
  std.lv = TRUE
) # , se = "bootstrap"
summary(fit1.d, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit1.d)
ft<-data.frame(t(as.matrix(fitMeasures(fit1.d))))
cfi_modd<-ft$cfi
tli_modd<-ft$tli
rmsea_modd<-ft$rmsea
rmsea.upp_modd<-ft$rmsea.ci.upper
rmsea.low_modd<-ft$rmsea.ci.upper
srmr_modd<-ft$srmr


library(semPlot)
m <- matrix(
  c(NA, 'vpd_log',  NA,
    'cwm_StemDensity_log', NA, "FDis_StemDensity_log",
    NA, "ICCapv_ha_log", NA),
  byrow = TRUE,
  3, 3)

p_pa <- semPaths(
  fit1.d,
  whatLabels = "std",
  intercepts = F,
  sizeMan = 10,
  edge.label.cex = 1.15,
  style = "ram",
  nCharNodes = 0,
  nCharEdges = 0,
  layout = m, DoNotPlot = T
)
p_pa$graphAttributes$Nodes$labels
p_pa$graphAttributes$Nodes$labels <-
  c(list(
    expression(Cai),
        expression(CWM[WD]),
expression(FDis[WD]),
    expression(VPD)
  ))
library(semptools)
p_pa2_modd <- mark_sig(p_pa, fit1.d)
plot(p_pa2_modd)


##%######################################################%##
#                                                          #
####                       Mod. E                       ####
#                                                          #
##%######################################################%##
dataset$FDis_XylemVulnerability_log <-
  scale(log(dataset$FDis_XylemVulnerability + (1 - min(dataset$FDis_XylemVulnerability, na.rm = T))), center = TRUE)
hist(dataset$FDis_XylemVulnerability_log)

dataset$cwm_XylemVulnerability_log <-
  scale(log(dataset$cwm_XylemVulnerability + (1 - min(dataset$cwm_XylemVulnerability, na.rm = T))), center = TRUE)
hist(dataset$cwm_XylemVulnerability_log)

modE <- '
# Direct effect
ICCapv_ha_log ~ c*vpd_log

# Mediator effect
ICCapv_ha_log ~ d*cwm_XylemVulnerability_log
ICCapv_ha_log ~ e*FDis_XylemVulnerability_log
cwm_XylemVulnerability_log ~ a*vpd_log
FDis_XylemVulnerability_log ~ b*vpd_log

# Indirect effects
be:=b*e # The indirect (i.e., Mediator) effect of vpd_log and FDIs on Capv id the product of the mediator coefficient (b*e)
ad:=a*d # The indirect (i.e., Mediator) effect of vpd_log and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total:=c+(b*e)+(a*d)
'

fit1.e = lavaan::cfa(
  modE,
  fixed.x = F,
  data = dataset,
  estimator = "MLR",
  likelihood = "wishart",
  missing = "FIML",
  std.lv = TRUE
) # , se = "bootstrap"
summary(fit1.e, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit1.e)
ft<-data.frame(t(as.matrix(fitMeasures(fit1.e))))
cfi_mode<-ft$cfi
tli_mode<-ft$tli
rmsea_mode<-ft$rmsea
rmsea.upp_mode<-ft$rmsea.ci.upper
rmsea.low_mode<-ft$rmsea.ci.upper
srmr_mode<-ft$srmr


library(semPlot)
m <- matrix(
  c(NA, 'vpd_log',  NA,
    'cwm_XylemVulnerability_log', NA, "FDis_XylemVulnerability_log",
    NA, "ICCapv_ha_log", NA),
  byrow = TRUE,
  3, 3)

p_pa <- semPaths(
  fit1.e,
  whatLabels = "std",
  intercepts = F,
  sizeMan = 10,
  edge.label.cex = 1.15,
  style = "ram",
  nCharNodes = 0,
  nCharEdges = 0,
  layout = m, DoNotPlot = T
)
p_pa$graphAttributes$Nodes$labels
p_pa$graphAttributes$Nodes$labels <-
  c(list(
    expression(Cai),
       expression(CWM[Xylem]),
 expression(FDis[Xylem]),
    expression(VPD)
  ))
library(semptools)
p_pa2_mode <- mark_sig(p_pa, fit1.e)
plot(p_pa2_mode)


##%######################################################%##
#                                                          #
####                       Mod. F                       ####
#                                                          #
##%######################################################%##
dataset$FDis_All_log <-
  scale(log(dataset$FDis_All + (1 - min(dataset$FDis_All, na.rm = T))), center = TRUE)
hist(dataset$FDis_All_log)

dataset$cwm_Dim1_log <-
  scale(log(dataset$cwm_Dim1 + (1 - min(dataset$cwm_Dim1, na.rm = T))), center = TRUE)
hist(dataset$cwm_Dim1_log)


modF <- '
# Direct effect
ICCapv_ha_log ~ c*vpd_log

# Mediator effect
ICCapv_ha_log ~ d*cwm_Dim1_log
ICCapv_ha_log ~ e*FDis_All_log
cwm_Dim1_log ~ a*vpd_log
FDis_All_log ~ b*vpd_log

# Indirect effects
be:=b*e # The indirect (i.e., Mediator) effect of vpd_log and FDIs on Capv id the product of the mediator coefficient (b*e)
ad:=a*d # The indirect (i.e., Mediator) effect of vpd_log and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total:=c+(b*e)+(a*d)
'

fit1.f = lavaan::cfa(
  modF,
  fixed.x = F,
  data = dataset,
  estimator = "MLR",
  likelihood = "wishart",
  missing = "FIML",
  std.lv = TRUE
) # , se = "bootstrap"
summary(fit1.f, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit1.f)
ft<-data.frame(t(as.matrix(fitMeasures(fit1.f))))
cfi_modf<-ft$cfi
tli_modf<-ft$tli
rmsea_modf<-ft$rmsea
rmsea.upp_modf<-ft$rmsea.ci.upper
rmsea.low_modf<-ft$rmsea.ci.upper
srmr_modf<-ft$srmr


library(semPlot)
m <- matrix(
  c(NA, 'vpd_log',  NA,
    'cwm_Dim1_log', NA, "FDis_All_log",
    NA, "ICCapv_ha_log", NA),
  byrow = TRUE,
  3, 3)

p_pa <- semPaths(
  fit1.f,
  whatLabels = "std",
  intercepts = F,
  sizeMan = 10,
  edge.label.cex = 1.15,
  style = "ram",
  nCharNodes = 0,
  nCharEdges = 0,
  layout = m, DoNotPlot = T
)
p_pa$graphAttributes$Nodes$labels
p_pa$graphAttributes$Nodes$labels <-
  c(list(
    expression(Cai),
        expression(CWM[Dim1]),
expression(FDis[all]),
    expression(VPD)
  ))
library(semptools)
p_pa2_modf <- mark_sig(p_pa, fit1.f)
plot(p_pa2_modf)


