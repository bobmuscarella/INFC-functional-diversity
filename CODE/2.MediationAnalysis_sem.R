# usage
packages <- c('lavaan','semPlot','semTools', 'semptools','semTable','dplyr','piecewiseSEM')
ipak(packages)

##%######################################################%##
#                                                          #
####                       Mod. A                       ####
#                                                          #
##%######################################################%##
dataset$ICCapv_ha_log<- log(dataset$ICCapv_ha+1)
dataset$Capv_ha_log<- log(dataset$Capv_ha+1)
dataset$Capm_ha_log<- log(dataset$Capm_ha+1)
hist(dataset$cwm_SeedMass_log)
hist(dataset$FDis_SeedMass)
dataset$FDis_SeedMass_log<- log(dataset$FDis_SeedMass+1)
hist(dataset$FDis_SeedMass_log)
ks.test(x=dataset$cwm_SeedMass_log,y='pnorm',alternative='two.sided')
ks.test(x=dataset$FDis_SeedMass,y='pnorm',alternative='two.sided')


modA <- '
# Direct effect
Capv_ha_log ~ c*vpd

# Mediator effect
Capv_ha_log ~ d * cwm_SeedMass_log
Capv_ha_log ~ e * FDis_SeedMass_log
cwm_SeedMass_log ~ a * vpd
FDis_SeedMass_log ~ b * vpd

# Indirect effects
be := b * e # The indirect (i.e., Mediator) effect of VPD and FDIs on Capv id the product of the mediator coefficient (b*e)
ad := a * d # The indirect (i.e., Mediator) effect of VPD and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total := c + (b * e) + (a * d)

# Observed means
Capv_ha_log ~ 1
FDis_SeedMass_log ~ 1
cwm_SeedMass_log ~ 1
vpd ~ 1

'
datasetscaled<- dataset %>% mutate_at(vars(Capv_ha_log, FDis_SeedMass_log, cwm_SeedMass_log, vpd), funs(scale))
fit1=lavaan::cfa(modA, fixed.x=F, data=dataset, estimator = "MLR", likelihood = "wishart", missing = "FIML", std.lv=TRUE) # , se = "bootstrap" 
summary(fit1, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
semPaths(fit1,whatLabels = "std", layout = "tree2", intercepts = F, nCharNodes = 7, sizeMan = 5,sizeLat = 8,curvePivot = T,ask = F)
parameterEstimates(fit1)
modificationIndices(fit1, minimum.value = 10)
ft<-data.frame(t(as.matrix(fitMeasures(fit1))))
cfi_moda<-ft$cfi
tli_moda<-ft$tli
rmsea_moda<-ft$rmsea
rmsea.upp_moda<-ft$rmsea.ci.upper
rmsea.low_moda<-ft$rmsea.ci.upper
srmr_moda<-ft$srmr


library(semPlot)
m <- matrix(
  c(NA, 'vpd',  NA,
    'cwm_SeedMass_log', NA, "FDis_SeedMass_log",
    NA, "Capv_ha_log", NA),
  byrow = TRUE,
  3, 3)
p_pa <- semPaths(
  fit1,
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
    expression(C[apv]),
    expression(CWM[SeedMass]),
    expression(FDis[SeedMass]),
    expression(VPD)
  ))
p_pa2_moda <- mark_sig(p_pa, fit1)
plot(p_pa2_moda)

semTable(
  fit1,
  paramSets = "all",
  paramSetLabels = c(
    "composites" = "Composites",
    "loadings" = "Factor Loadings",
    "slopes" = "Regression Slopes",
    "intercepts" = "Intercepts",
    "means" = "Means",
    "residualvariances" = "Residual Variances",
    "residualcovariances" = "Residual Covariances",
    "variances" = "Variances",
    "latentvariances" = "Latent Variances",
    "latentcovariances" = "Latent Covariances",
    "latentmeans" = "Latent Intercepts",
    "thresholds" = "Thresholds",
    "constructed" = "Constructed",
    "fits" = "Fit Indices"
  ),
  columns = c("est", "se", "z", "p", "rsquare"
              #"estse", "eststars", "estsestars"
  ),
  columnLabels =  c(
    "est" = "Estimate",
    se = "Std. Err.",
    z = "z",
    p = "p",
    rsquare = "R Square" #, estse = "Estimate(Std.Err.)", eststars = "Estimate", estsestars = "Estimate(Std.Err.)"
  ),
  
  fits = c("tli", "chisq", "rmsea"),
  fitLabels = c(tli = "TLI", chisq = "chisq"),
  type = "html",
  file = ("output_tab/moda"),
  print.results = T
)


##%######################################################%##
#                                                          #
####                       Mod. B                       ####
#                                                          #
##%######################################################%##
hist(dataset$cwm_Height_log)
hist(dataset$FDis_Height)
dataset$FDis_Height_log<- log(dataset$FDis_Height+1)
hist(dataset$FDis_Height_log)


modB <- '
# Direct effect
Capv_ha_log ~ c*vpd

# Mediator effect
Capv_ha_log ~ d*cwm_Height_log
Capv_ha_log ~ e*FDis_Height_log
cwm_Height_log ~ a*vpd
FDis_Height_log ~ b*vpd

# Indirect effects
be:=b*e # The indirect (i.e., Mediator) effect of VPD and FDIs on Capv id the product of the mediator coefficient (b*e)
ad:=a*d # The indirect (i.e., Mediator) effect of VPD and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total:=c+(b*e)+(a*d)
'

fit1 <- lavaan::sem(modB, data=dataset)
summary(fit1, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit1)
ft<-data.frame(t(as.matrix(fitMeasures(fit1))))
cfi_modb<-ft$cfi
tli_modb<-ft$tli
rmsea_modb<-ft$rmsea
rmsea.upp_modb<-ft$rmsea.ci.upper
rmsea.low_modb<-ft$rmsea.ci.upper
srmr_modb<-ft$srmr

library(semPlot)
m <- matrix(
  c(NA, 'vpd',  NA,
    'cwm_Height_log', NA, "FDis_Height_log",
    NA, "Capv_ha_log", NA),
  byrow = TRUE,
  3, 3)

p_pa <- semPaths(
  fit1,
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
    expression(C[apv]),
        expression(CWM[Height]),
expression(FDis[Height]),
    expression(VPD)
  ))
library(semptools)
p_pa2_modb <- mark_sig(p_pa, fit1)
plot(p_pa2_modb)

semTable(
  fit1,
  paramSets = "all",
  paramSetLabels = c(
    "composites" = "Composites",
    "loadings" = "Factor Loadings",
    "slopes" = "Regression Slopes",
    "intercepts" = "Intercepts",
    "means" = "Means",
    "residualvariances" = "Residual Variances",
    "residualcovariances" = "Residual Covariances",
    "variances" = "Variances",
    "latentvariances" = "Latent Variances",
    "latentcovariances" = "Latent Covariances",
    "latentmeans" = "Latent Intercepts",
    "thresholds" = "Thresholds",
    "constructed" = "Constructed",
    "fits" = "Fit Indices"
  ),
  columns = c("est", "se", "z", "p", "rsquare"
              #"estse", "eststars", "estsestars"
  ),
  columnLabels =  c(
    "est" = "Estimate",
    se = "Std. Err.",
    z = "z",
    p = "p",
    rsquare = "R Square" #, estse = "Estimate(Std.Err.)", eststars = "Estimate", estsestars = "Estimate(Std.Err.)"
  ),
  
  fits = c("tli", "chisq", "rmsea"),
  fitLabels = c(tli = "TLI", chisq = "chisq"),
  type = "html",
  file = ("output_tab/modb"),
  print.results = T
)


##%######################################################%##
#                                                          #
####                       Mod. C                       ####
#                                                          #
##%######################################################%##
hist(dataset$cwm_SLA_log)
hist(dataset$FDis_SLA)
dataset$FDis_SLA_log<- log(dataset$FDis_SLA+1)
hist(log(dataset$FDis_SLA))

modC <- '
# Direct effect
Capv_ha_log ~ c*vpd

# Mediator effect
Capv_ha_log ~ d*cwm_SLA_log
Capv_ha_log ~ e*FDis_SLA_log
cwm_SLA_log ~ a*vpd
FDis_SLA_log ~ b*vpd

# Indirect effects
be:=b*e # The indirect (i.e., Mediator) effect of VPD and FDIs on Capv id the product of the mediator coefficient (b*e)
ad:=a*d # The indirect (i.e., Mediator) effect of VPD and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total:=c+(b*e)+(a*d)
'

fit1 <- lavaan::sem(modC, data=dataset)
summary(fit1, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit1)
ft<-data.frame(t(as.matrix(fitMeasures(fit1))))
cfi_modc<-ft$cfi
tli_modc<-ft$tli
rmsea_modc<-ft$rmsea
rmsea.upp_modc<-ft$rmsea.ci.upper
rmsea.low_modc<-ft$rmsea.ci.upper
srmr_modc<-ft$srmr


library(semPlot)
m <- matrix(
  c(NA, 'vpd',  NA,
    'cwm_SLA_log', NA, "FDis_SLA_log",
    NA, "Capv_ha_log", NA),
  byrow = TRUE,
  3, 3)

p_pa <- semPaths(
  fit1,
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
    expression(C[apv]),
        expression(CWM[SLA]),
expression(FDis[SLA]),
    expression(VPD)
  ))
library(semptools)
p_pa2_modc <- mark_sig(p_pa, fit1)
plot(p_pa2_modc)

semTable(
  fit1,
  paramSets = "all",
  paramSetLabels = c(
    "composites" = "Composites",
    "loadings" = "Factor Loadings",
    "slopes" = "Regression Slopes",
    "intercepts" = "Intercepts",
    "means" = "Means",
    "residualvariances" = "Residual Variances",
    "residualcovariances" = "Residual Covariances",
    "variances" = "Variances",
    "latentvariances" = "Latent Variances",
    "latentcovariances" = "Latent Covariances",
    "latentmeans" = "Latent Intercepts",
    "thresholds" = "Thresholds",
    "constructed" = "Constructed",
    "fits" = "Fit Indices"
  ),
  columns = c("est", "se", "z", "p", "rsquare"
              #"estse", "eststars", "estsestars"
  ),
  columnLabels =  c(
    "est" = "Estimate",
    se = "Std. Err.",
    z = "z",
    p = "p",
    rsquare = "R Square" #, estse = "Estimate(Std.Err.)", eststars = "Estimate", estsestars = "Estimate(Std.Err.)"
  ),
  
  fits = c("tli", "chisq", "rmsea"),
  fitLabels = c(tli = "TLI", chisq = "chisq"),
  type = "html",
  file = ("output_tab/modc"),
  print.results = T
)

##%######################################################%##
#                                                          #
####                       Mod. D                       ####
#                                                          #
##%######################################################%##
hist(dataset$cwm_StemDensity)
hist(dataset$FDis_StemDensity)
dataset$cwm_StemDensity_log<- log(dataset$cwm_StemDensity+1)
dataset$FDis_StemDensity_log<- log(dataset$FDis_StemDensity+1)

modD <- '
# Direct effect
Capv_ha_log ~ c*vpd

# Mediator effect
Capv_ha_log ~ d*cwm_StemDensity_log
Capv_ha_log ~ e*FDis_StemDensity_log
cwm_StemDensity_log ~ a*vpd
FDis_StemDensity_log ~ b*vpd

# Indirect effects
be:=b*e # The indirect (i.e., Mediator) effect of VPD and FDIs on Capv id the product of the mediator coefficient (b*e)
ad:=a*d # The indirect (i.e., Mediator) effect of VPD and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total:=c+(b*e)+(a*d)
'


fit1 <- lavaan::sem(modD, data=dataset)
summary(fit1, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit1)
ft<-data.frame(t(as.matrix(fitMeasures(fit1))))
cfi_modd<-ft$cfi
tli_modd<-ft$tli
rmsea_modd<-ft$rmsea
rmsea.upp_modd<-ft$rmsea.ci.upper
rmsea.low_modd<-ft$rmsea.ci.upper
srmr_modd<-ft$srmr


library(semPlot)
m <- matrix(
  c(NA, 'vpd',  NA,
    'cwm_StemDensity_log', NA, "FDis_StemDensity_log",
    NA, "Capv_ha_log", NA),
  byrow = TRUE,
  3, 3)

p_pa <- semPaths(
  fit1,
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
    expression(C[apv]),
        expression(CWM[WD]),
expression(FDis[WD]),
    expression(VPD)
  ))
library(semptools)
p_pa2_modd <- mark_sig(p_pa, fit1)
plot(p_pa2_modd)

semTable(
  fit1,
  paramSets = "all",
  paramSetLabels = c(
    "composites" = "Composites",
    "loadings" = "Factor Loadings",
    "slopes" = "Regression Slopes",
    "intercepts" = "Intercepts",
    "means" = "Means",
    "residualvariances" = "Residual Variances",
    "residualcovariances" = "Residual Covariances",
    "variances" = "Variances",
    "latentvariances" = "Latent Variances",
    "latentcovariances" = "Latent Covariances",
    "latentmeans" = "Latent Intercepts",
    "thresholds" = "Thresholds",
    "constructed" = "Constructed",
    "fits" = "Fit Indices"
  ),
  columns = c("est", "se", "z", "p", "rsquare"
              #"estse", "eststars", "estsestars"
  ),
  columnLabels =  c(
    "est" = "Estimate",
    se = "Std. Err.",
    z = "z",
    p = "p",
    rsquare = "R Square" #, estse = "Estimate(Std.Err.)", eststars = "Estimate", estsestars = "Estimate(Std.Err.)"
  ),
  
  fits = c("tli", "chisq", "rmsea"),
  fitLabels = c(tli = "TLI", chisq = "chisq"),
  type = "html",
  file = ("output_tab/modd"),
  print.results = T
)

##%######################################################%##
#                                                          #
####                       Mod. E                       ####
#                                                          #
##%######################################################%##
hist(dataset$cwm_XylemVulnerability)
hist(dataset$FDis_XylemVulnerability)
dataset$cwm_XylemVulnerability_log<- log(dataset$cwm_XylemVulnerability+11)
dataset$FDis_XylemVulnerability_log<- log(dataset$FDis_XylemVulnerability+1)

modE <- '
# Direct effect
Capv_ha_log ~ c*vpd

# Mediator effect
Capv_ha_log ~ d*cwm_XylemVulnerability_log
Capv_ha_log ~ e*FDis_XylemVulnerability_log
cwm_XylemVulnerability_log ~ a*vpd
FDis_XylemVulnerability_log ~ b*vpd

# Indirect effects
be:=b*e # The indirect (i.e., Mediator) effect of VPD and FDIs on Capv id the product of the mediator coefficient (b*e)
ad:=a*d # The indirect (i.e., Mediator) effect of VPD and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total:=c+(b*e)+(a*d)
'

fit1 <- lavaan::sem(modE, data=dataset)
summary(fit1, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit1)
ft<-data.frame(t(as.matrix(fitMeasures(fit1))))
cfi_mode<-ft$cfi
tli_mode<-ft$tli
rmsea_mode<-ft$rmsea
rmsea.upp_mode<-ft$rmsea.ci.upper
rmsea.low_mode<-ft$rmsea.ci.upper
srmr_mode<-ft$srmr


library(semPlot)
m <- matrix(
  c(NA, 'vpd',  NA,
    'cwm_XylemVulnerability_log', NA, "FDis_XylemVulnerability_log",
    NA, "Capv_ha_log", NA),
  byrow = TRUE,
  3, 3)

p_pa <- semPaths(
  fit1,
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
    expression(C[apv]),
       expression(CWM[Xylem]),
 expression(FDis[Xylem]),
    expression(VPD)
  ))
library(semptools)
p_pa2_mode <- mark_sig(p_pa, fit1)
plot(p_pa2_mode)

semTable(
  fit1,
  paramSets = "all",
  paramSetLabels = c(
    "composites" = "Composites",
    "loadings" = "Factor Loadings",
    "slopes" = "Regression Slopes",
    "intercepts" = "Intercepts",
    "means" = "Means",
    "residualvariances" = "Residual Variances",
    "residualcovariances" = "Residual Covariances",
    "variances" = "Variances",
    "latentvariances" = "Latent Variances",
    "latentcovariances" = "Latent Covariances",
    "latentmeans" = "Latent Intercepts",
    "thresholds" = "Thresholds",
    "constructed" = "Constructed",
    "fits" = "Fit Indices"
  ),
  columns = c("est", "se", "z", "p", "rsquare"
              #"estse", "eststars", "estsestars"
  ),
  columnLabels =  c(
    "est" = "Estimate",
    se = "Std. Err.",
    z = "z",
    p = "p",
    rsquare = "R Square" #, estse = "Estimate(Std.Err.)", eststars = "Estimate", estsestars = "Estimate(Std.Err.)"
  ),
  
  fits = c("tli", "chisq", "rmsea"),
  fitLabels = c(tli = "TLI", chisq = "chisq"),
  type = "html",
  file = ("output_tab/mode"),
  print.results = T
)


##%######################################################%##
#                                                          #
####                       Mod. F                       ####
#                                                          #
##%######################################################%##
hist(dataset$cwm_Dim1)
hist(dataset$FDis_All)
dataset$cwm_Dim1_log<- log(dataset$cwm_Dim1+2)
dataset$FDis_All_log<- log(dataset$FDis_All+1)

modF <- '
# Direct effect
Capv_ha_log ~ c*vpd

# Mediator effect
Capv_ha_log ~ d*cwm_Dim1_log
Capv_ha_log ~ e*FDis_All_log
cwm_Dim1_log ~ a*vpd
FDis_All_log ~ b*vpd

# Indirect effects
be:=b*e # The indirect (i.e., Mediator) effect of VPD and FDIs on Capv id the product of the mediator coefficient (b*e)
ad:=a*d # The indirect (i.e., Mediator) effect of VPD and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total:=c+(b*e)+(a*d)
'
fit1 <- lavaan::sem(modF, data=dataset)
summary(fit1, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit1)
ft<-data.frame(t(as.matrix(fitMeasures(fit1))))
cfi_modf<-ft$cfi
tli_modf<-ft$tli
rmsea_modf<-ft$rmsea
rmsea.upp_modf<-ft$rmsea.ci.upper
rmsea.low_modf<-ft$rmsea.ci.upper
srmr_modf<-ft$srmr


library(semPlot)
m <- matrix(
  c(NA, 'vpd',  NA,
    'cwm_Dim1_log', NA, "FDis_All_log",
    NA, "Capv_ha_log", NA),
  byrow = TRUE,
  3, 3)

p_pa <- semPaths(
  fit1,
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
    expression(C[apv]),
        expression(CWM[Dim1]),
expression(FDis[all]),
    expression(VPD)
  ))
library(semptools)
p_pa2_modf <- mark_sig(p_pa, fit1)
plot(p_pa2_modf)

semTable(
  fit1,
  paramSets = "all",
  paramSetLabels = c(
    "composites" = "Composites",
    "loadings" = "Factor Loadings",
    "slopes" = "Regression Slopes",
    "intercepts" = "Intercepts",
    "means" = "Means",
    "residualvariances" = "Residual Variances",
    "residualcovariances" = "Residual Covariances",
    "variances" = "Variances",
    "latentvariances" = "Latent Variances",
    "latentcovariances" = "Latent Covariances",
    "latentmeans" = "Latent Intercepts",
    "thresholds" = "Thresholds",
    "constructed" = "Constructed",
    "fits" = "Fit Indices"
  ),
  columns = c("est", "se", "z", "p", "rsquare"
              #"estse", "eststars", "estsestars"
  ),
  columnLabels =  c(
    "est" = "Estimate",
    se = "Std. Err.",
    z = "z",
    p = "p",
    rsquare = "R Square" #, estse = "Estimate(Std.Err.)", eststars = "Estimate", estsestars = "Estimate(Std.Err.)"
  ),
  
  fits = c("tli", "chisq", "rmsea"),
  fitLabels = c(tli = "TLI", chisq = "chisq"),
  type = "html",
  file = ("output_tab/modf"),
  print.results = T
)

##%######################################################%##
#                                                          #
####                       Mod. G                       ####
#                                                          #
##%######################################################%##
hist(dataset$cwm_Dim2)
hist(dataset$FDis_All)
dataset$cwm_Dim2_log<- log(dataset$cwm_Dim2+3)

modG <- '
# Direct effect
Capv_ha_log ~ c*vpd

# Mediator effect
Capv_ha_log ~ d*cwm_Dim2_log
Capv_ha_log ~ e*FDis_All_log
cwm_Dim2_log ~ a*vpd
FDis_All_log ~ b*vpd

# Indirect effects
be:=b*e # The indirect (i.e., Mediator) effect of VPD and FDIs on Capv id the product of the mediator coefficient (b*e)
ad:=a*d # The indirect (i.e., Mediator) effect of VPD and CWM on Capv id the product of the mediator coefficient (a*d)

# Total direct+indirect effect
total:=c+(b*e)+(a*d)
'

fit1 <- lavaan::sem(modG, data=dataset)
summary(fit1, fit.measures=TRUE,standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit1)
ft<-data.frame(t(as.matrix(fitMeasures(fit1))))
cfi_modg<-ft$cfi
tli_modg<-ft$tli
rmsea_modg<-ft$rmsea
rmsea.upp_modg<-ft$rmsea.ci.upper
rmsea.low_modg<-ft$rmsea.ci.upper
srmr_modg<-ft$srmr


library(semPlot)
m <- matrix(
  c(NA, 'vpd',  NA,
    'cwm_Dim2_log', NA, "FDis_All_log",
    NA, "Capv_ha_log", NA),
  byrow = TRUE,
  3, 3)

p_pa <- semPaths(
  fit1,
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
    expression(C[apv]),
        expression(CWM[Dim2]),
expression(FDis[all]),
    expression(VPD)
  ))
library(semptools)
p_pa2_modg <- mark_sig(p_pa, fit1)
plot(p_pa2_modg)

semTable(
  fit1,
  paramSets = "all",
  paramSetLabels = c(
    "composites" = "Composites",
    "loadings" = "Factor Loadings",
    "slopes" = "Regression Slopes",
    "intercepts" = "Intercepts",
    "means" = "Means",
    "residualvariances" = "Residual Variances",
    "residualcovariances" = "Residual Covariances",
    "variances" = "Variances",
    "latentvariances" = "Latent Variances",
    "latentcovariances" = "Latent Covariances",
    "latentmeans" = "Latent Intercepts",
    "thresholds" = "Thresholds",
    "constructed" = "Constructed",
    "fits" = "Fit Indices"
  ),
  columns = c("est", "se", "z", "p", "rsquare"
              #"estse", "eststars", "estsestars"
  ),
  columnLabels =  c(
    "est" = "Estimate",
    se = "Std. Err.",
    z = "z",
    p = "p",
    rsquare = "R Square" #, estse = "Estimate(Std.Err.)", eststars = "Estimate", estsestars = "Estimate(Std.Err.)"
  ),
  
  fits = c("tli", "chisq", "rmsea"),
  fitLabels = c(tli = "TLI", chisq = "chisq"),
  type = "html",
  file = ("output_tab/modg"),
  print.results = T
)

##%######################################################%##
#                                                          #
####                        Plot                        ####
#                                                          #
##%######################################################%##
##--- Root mean square error of approximation (RMSEA)---#
# The RMSEA tells us how well the model, with unknown but optimally chosen parameter estimates, would fit the populations' covariance matrix (Byrne, 1998). In recent years it has become regarded as ‘one of the most informative fit indices’ (Diamantopoulos and Siguaw, 2000) due to its sensitivity to the number of estimated parameters in the model. In other words, the RMSEA favors parsimony in that it will choose the model with a lesser number of parameters. Recommendations for RMSEA cut-off points have been reduced considerably in the last fifteen years. Up until the early nineties, an RMSEA in the range of 0.05 to 0.10 was considered an indication of fair fit, and values above 0.10 indicated poor fit (MacCallum et al, 1996). It was then thought that an RMSEA of between 0.08 to 0.10 provides a mediocre fit and below 0.08 shows a good fit (MacCallum et al, 1996). However, more recently, a cut-off value close to .06 (Hu and Bentler, 1999) or a stringent upper limit of 0.07 (Steiger, 2007) seems to be the general consensus amongst authorities in this area.

##--- Standardized root mean square residual (SRMR) ---#
# The RMR and the SRMR are the square root of the difference between the residuals of the sample covariance matrix and the hypothesized covariance model. The range of the RMR is calculated based upon the scales of each indicator, therefore, if a questionnaire contains items with varying levels (some items may range from 1 – 5 while others range from 1 – 7) the RMR becomes difficult to interpret (Kline, 2005). The standardized RMR (SRMR) resolves this problem and is therefore much more meaningful to interpret. Values for the SRMR range from zero to 1.0 with well-fitting models obtaining values less than .05 (Byrne, 1998; Diamantopoulos and Siguaw, 2000), however values as high as 0.08 are deemed acceptable (Hu and Bentler, 1999). An SRMR of 0 indicates perfect fit but it must be noted that SRMR will be lower when there is a high number of parameters in the model and in models based on large sample sizes

##--- CFI (Comparative fit index) ---#
# The Comparative Fit Index (CFI: Bentler, 1990) is a revised form of the NFI which takes into account sample size (Byrne, 1998) that performs well even when sample size is small (Tabachnick and Fidell, 2007). This index was first introduced by Bentler (1990) and subsequently included as part of the fit indices in his EQS program (Kline, 2005). Like the NFI, this statistic assumes that all latent variables are uncorrelated (null/independence model) and compares the sample covariance matrix with this null model. As with the NFI, values for this statistic range between 0.0 and 1.0 with values closer to 1.0 indicating good fit. A cut-off criterion of CFI ≥ 0.90 was initially advanced however, recent studies have shown that a value greater than 0.90 is needed in order to ensure that misspecified models are not accepted (Hu and Bentler, 1999). From this, a value of CFI ≥ 0.95 is presently recognised as indicative of good fit (Hu and Bentler, 1999). Today this index is included in all SEM programs and is one of the most popularly reported fit indices due to being one of the measures least effected by sample size (Fan et al, 1999).

# pdf('Fig_cfa.pdf', width = 10, height = 7)
# 
# op<- par(mfrow=c(2,3))

png(
  "output_plot/Mod_a.jpg",
  width = 5,
  height = 5,
  units = 'in',
  res = 300
)
plot(p_pa2_moda)
legend(
  'topright',
  legend = c(
    paste0('CFI=', round(cfi_moda, 2)),
    paste0('TLI=', round(tli_moda, 2)),
    paste0('RMSEA=', round(rmsea_moda, 2)),
    paste0('SRMR=', round(srmr_moda, 2))
  ),
  cex = 0.7,
  box.lty = 0,
  bg = "transparent",
  text.col = 'grey30'
)
usr <- par("usr")
text(usr[1],
     usr[4],
     'Mod. A',
     adj = c(-0.2, 1.5),
     col = 'black',
     cex = 1)
dev.off()

png(
  "output_plot/Mod_b.jpg",
  width = 5,
  height = 5,
  units = 'in',
  res = 300
)
plot(p_pa2_modb)
legend(
  'topright',
  legend = c(
    paste0('CFI=', round(cfi_modb, 2)),
    paste0('TLI=', round(tli_modb, 2)),
    paste0('RMSEA=', round(rmsea_modb, 2)),
    paste0('SRMR=', round(srmr_modb, 2))
  ),
  cex = 0.7,
  box.lty = 0,
  bg = "transparent",
  text.col = 'grey30'
)
usr <- par("usr")
text(usr[1],
     usr[4],
     'Mod. B',
     adj = c(-0.2, 1.5),
     col = 'black',
     cex = 1)
dev.off()

png(
  "output_plot/Mod_c.jpg",
  width = 5,
  height = 5,
  units = 'in',
  res = 300
)
plot(p_pa2_modc)
legend(
  'topright',
  legend = c(
    paste0('CFI=', round(cfi_modc, 2)),
    paste0('TLI=', round(tli_modc, 2)),
    paste0('RMSEA=', round(rmsea_modc, 2)),
    paste0('SRMR=', round(srmr_modc, 2))
  ),
  cex = 0.7,
  box.lty = 0,
  bg = "transparent",
  text.col = 'grey30'
)
usr <- par("usr")
text(usr[1],
     usr[4],
     'Mod. C',
     adj = c(-0.2, 1.5),
     col = 'black',
     cex = 1)
dev.off()

png(
  "output_plot/Mod_d.jpg",
  width = 5,
  height = 5,
  units = 'in',
  res = 300
)
plot(p_pa2_modd)
legend(
  'topright',
  legend = c(
    paste0('CFI=', round(cfi_modd, 2)),
    paste0('TLI=', round(tli_modd, 2)),
    paste0('RMSEA=', round(rmsea_modd, 2)),
    paste0('SRMR=', round(srmr_modd, 2))
  ),
  cex = 0.7,
  box.lty = 0,
  bg = "transparent",
  text.col = 'grey30'
)
usr <- par("usr")
text(usr[1],
     usr[4],
     'Mod. D',
     adj = c(-0.2, 1.5),
     col = 'black',
     cex = 1)
dev.off()

png(
  "output_plot/Mod_e.jpg",
  width = 5,
  height = 5,
  units = 'in',
  res = 300
)
plot(p_pa2_mode)
legend(
  'topright',
  legend = c(
    paste0('CFI=', round(cfi_mode, 2)),
    paste0('TLI=', round(tli_mode, 2)),
    paste0('RMSEA=', round(rmsea_mode, 2)),
    paste0('SRMR=', round(srmr_mode, 2))
  ),
  cex = 0.7,
  box.lty = 0,
  bg = "transparent",
  text.col = 'grey30'
)
usr <- par("usr")
text(usr[1],
     usr[4],
     'Mod. E',
     adj = c(-0.2, 1.5),
     col = 'black',
     cex = 1)
dev.off()

png(
  "output_plot/Mod_f.jpg",
  width = 5,
  height = 5,
  units = 'in',
  res = 300
)
plot(p_pa2_modf)
legend(
  'topright',
  legend = c(
    paste0('CFI=', round(cfi_modf, 2)),
    paste0('TLI=', round(tli_modf, 2)),
    paste0('RMSEA=', round(rmsea_modf, 2)),
    paste0('SRMR=', round(srmr_modf, 2))
  ),
  cex = 0.7,
  box.lty = 0,
  bg = "transparent",
  text.col = 'grey30'
)
usr <- par("usr")
text(usr[1],
     usr[4],
     'Mod. F',
     adj = c(-0.2, 1.5),
     col = 'black',
     cex = 1)
dev.off()

png(
  "output_plot/Mod_g.jpg",
  width = 5,
  height = 5,
  units = 'in',
  res = 300
)
plot(p_pa2_modg)
legend(
  'topright',
  legend = c(
    paste0('CFI=', round(cfi_modg, 2)),
    paste0('TLI=', round(tli_modg, 2)),
    paste0('RMSEA=', round(rmsea_modg, 2)),
    paste0('SRMR=', round(srmr_modg, 2))
  ),
  cex = 0.7,
  box.lty = 0,
  bg = "transparent",
  text.col = 'grey30'
)
usr <- par("usr")
text(usr[1],
     usr[4],
     'Mod. G',
     adj = c(-0.2, 1.5),
     col = 'black',
     cex = 1)
dev.off()
# par(op)
# dev.off()
par(resetPar)
