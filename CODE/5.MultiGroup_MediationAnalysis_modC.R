# load packages
packages <- c('lavaan','semPlot','semTools', 'semptools','semTable','dplyr','piecewiseSEM')
ipak(packages)

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
Capv_ha_log ~ c(c0,c1)*vpd

# Mediator effect
Capv_ha_log ~ c(d0,d1) * cwm_SLA_log
Capv_ha_log ~ c(e0,e1) * FDis_SLA_log
cwm_SLA_log ~ c(a0,a1) * vpd
FDis_SLA_log ~ c(b0,b1) * vpd

# Indirect effects
b0e0 := b0 * e0 # The indirect (i.e., Mediator) effect of VPD and FDIs on Capv id the product of the mediator coefficient (b0*e0)
a0d0 := a0 * d0 # The indirect (i.e., Mediator) effect of VPD and CWM on Capv id the product of the mediator coefficient (a0*d0)

# Total direct+indirect effect
total0 := c0 + (b0 * e0) + (a0 * d0)

# Indirect effects
b1e1 := b1 * e1 # The indirect (i.e., Mediator) effect of VPD and FDIs on Capv id the product of the mediator coefficient (b1*e1)
a1d1 := a1 * d1 # The indirect (i.e., Mediator) effect of VPD and CWM on Capv id the product of the mediator coefficient (a1*d1)

# Total direct+indirect effect
total1 := c1 + (b1 * e1) + (a1 * d1)

# Observed means
Capv_ha_log ~ 1
FDis_SLA_log ~ 1
cwm_SLA_log ~ 1
vpd ~ 1
'

##%######################################################%##
#                                                          #
####                     Configural                     ####
#                                                          #
##%######################################################%##

fit.Configural <-
  cfa(
    modC,
    data = dataset,
    group = "climate_classification",
    meanstructure = TRUE,
    estimator = "MLR",
    likelihood = "wishart",
    missing = "FIML",
    std.lv = TRUE
  )

summary(fit.Configural, fit.measures = TRUE,standardized = TRUE, rsq = TRUE)
parameterEstimates(fit.Configural)
fitMeasures(fit.Configural)["rmsea"]
fitMeasures(fit.Configural, "cfi")
modificationIndices(fit.Configural, minimum.value = 10)
mi <- modindices(fit.Configural)
mi[mi$op == "~~",]
head(mi[order(mi$mi, decreasing=TRUE), ], 10)#Spoting the top 10:
subset(mi[order(mi$mi, decreasing=TRUE), ], mi > 5)#And the bigger than 5:

##%######################################################%##
#                                                          #
####       Next, we fit the constrained model by        ####
####         specifying the additional argument         ####
####   group.equal = c("intercepts", "regressions").    ####
####  This argument fixes both the intercepts and path  ####
####     coefficients in each group to be the same.     ####
#                                                          #
##%######################################################%##

fit.Constrained <- sem(modC,
                       fixed.x = F,
                       data = dataset,
                       estimator = "MLR",
                       likelihood = "wishart",
                       missing = "FIML",
                       std.lv = TRUE,
                       group = 'climate_classification', 
                       group.equal = c("intercepts", "regressions"))

summary(fit.Constrained, fit.measures = TRUE, standardized = TRUE, ci=TRUE)
# Both the constrained and free models fit the data well based on the χ^2 statistic, and we can formally compare the two using a Chi-squared difference test:
anova(fit.Configural, fit.Constrained)
# The significant P-value implies that the free and constrained models are significantly different. In other words, some paths vary while others may not. If the models were not significantly different, then one would conclude that the constrained model is equivalent to the free model, or that the coefficients would not vary by group, and it would be fair to analyze the pooled data in a single global model.


##%######################################################%##
#                                                          #
####               We can now undergo the               ####
####        process of introducing and releasing        ####
####          constraints to try and identify           ####
####          which path varies between groups          ####
#                                                          #
##%######################################################%##

sub.dataset<- dataset[,c('Capv_ha_log','FDis_SLA_log','cwm_SLA_log','vpd','climate_classification')]
sub.dataset<- na.omit(sub.dataset)
library(piecewiseSEM)
pmultigroup <- psem(
  lm(Capv_ha_log ~ FDis_SLA_log + cwm_SLA_log+vpd, sub.dataset),
  lm(FDis_SLA_log ~ vpd, sub.dataset),
  lm(cwm_SLA_log ~ vpd, sub.dataset)
  
)
multigroup(pmultigroup, group = "climate_classification")

#----- Overall (Omnibus) Wald-Test ------#
all.constraints<- 'd0 == d1' #Tell Lavaan these are the constraints we are interested in testing simultaneously.
lavTestWald(fit.Configural, #the name of the Lavaan 'fitted' object
            constraints = all.constraints) #the name of our previously specified paths that we would like to test


modCp <- '
# Direct effect
Capv_ha_log ~ c(c0,c1)*vpd

# Mediator effect
Capv_ha_log ~ c(d0,d0) * cwm_SLA_log
Capv_ha_log ~ c(e0,e1) * FDis_SLA_log
cwm_SLA_log ~ c(a0,a1) * vpd
FDis_SLA_log ~ c(b0,b1) * vpd

# Indirect effects
b0e0 := b0 * e0 # The indirect (i.e., Mediator) effect of VPD and FDIs on Capv id the product of the mediator coefficient (b0*e0)
a0d0 := a0 * d0 # The indirect (i.e., Mediator) effect of VPD and CWM on Capv id the product of the mediator coefficient (a0*d0)

# Total direct+indirect effect
total0 := c0 + (b0 * e0) + (a0 * d0)

# Indirect effects
b1e1 := b1 * e1 # The indirect (i.e., Mediator) effect of VPD and FDIs on Capv id the product of the mediator coefficient (b1*e1)
a1d0 := a1 * d0 # The indirect (i.e., Mediator) effect of VPD and CWM on Capv id the product of the mediator coefficient (a1*d0)

# Total direct+indirect effect
total1 := c1 + (b1 * e1) + (a1 * d0)

# Observed means
Capv_ha_log ~ 1
FDis_SLA_log ~ 1
cwm_SLA_log ~ 1
vpd ~ 1
'

fit.PartConstrained <- sem(modCp,
                           fixed.x = F,
                           data = dataset,
                           estimator = "MLR",
                           likelihood = "wishart",
                           missing = "FIML",
                           std.lv = TRUE,
                           group = 'climate_classification')

summary(fit.PartConstrained, fit.measures = TRUE, standardized = TRUE)
anova(fit.Configural, fit.PartConstrained)

standardizedSolution(fit.PartConstrained)

ft<-data.frame(t(as.matrix(fitMeasures(fit.PartConstrained))))
cfi_modC<-ft$cfi
tli_modC<-ft$tli
rmsea_modC<-ft$rmsea
rmsea.upp_modC<-ft$rmsea.ci.upper
rmsea.low_modC<-ft$rmsea.ci.upper
srmr_modC<-ft$srmr

############################################################
#                                                          #
#                       compare fit                        #
#                                                          #
############################################################
modelcomparison <- compareFit(fit.Configural, fit.Constrained, fit.PartConstrained)
summary(modelcomparison)
summary(modelcomparison, fit.measures = c("aic", "bic", 'cfi', 'rmsea'))
lavTestLRT(fit.Configural, fit.Constrained, fit.PartConstrained)

############################################################
#                                                          #
#                 Measurement invariance                   #
#                                                          #
############################################################
(out <- measurementInvariance(model=modC,
                              fixed.x = FALSE,
                              estimator = "MLR",likelihood = "wishart", 
                              missing = "FIML",
                              data = dataset,
                              group = "climate_classification"))
cf<-compareFit(out)
summary(cf)

##%######################################################%##
#                                                          #
####                        Plot                        ####
#                                                          #
##%######################################################%##
m <- matrix(
  c(NA, 'vpd',  NA,
    'cwm_SLA_log', NA, "FDis_SLA_log",
    NA, "Capv_ha_log", NA),
  byrow = TRUE,
  3, 3)
p_pa<-semPaths(
  fit.PartConstrained,
  whatLabels = "std",
  #what="eq",
  ask = FALSE,
  intercepts = F,
  residuals=T,
  thresholds=T,
  sizeMan = 10,
  edge.label.cex = 1.15,
  style = "ram",
  nCharNodes = 20,
  nCharEdges = 0,
  #layout = 'tree2',
  layout = m, 
  title=F,
  combineGroups=F,allVars=F,ThreshAtSide=T,
  fade=F, DoNotPlot = T, mar = c(5, 7, 5, 7)
)


p_pa[[1]]$graphAttributes$Nodes$labels
p_pa[[1]]$graphAttributes$Nodes$labels <-
  c(list(
    expression(C[apv]),
    expression(CWM[SLA]),
    expression(FDis[SLA]),
    expression(VPD)
  ))
p_pa[[2]]$graphAttributes$Nodes$labels
p_pa[[2]]$graphAttributes$Nodes$labels <-
  c(list(
    expression(C[apv]),
    expression(CWM[SLA]),
    expression(FDis[SLA]),
    expression(VPD)
  ))

png(
  "output_plot/MultiGroup_Mod_a.jpg",
  width = 10,
  height = 4.5,
  units = 'in',
  res = 300
)
op<- par(mfrow=c(1,2), mar=c(5,5,5,5))
plot(
  p_pa[[1]])
usr <- par("usr")
text(usr[1],
     usr[4]-0.1,
     'Mod. C',
     adj = c(-0.2, 1.5),
     col = 'black',
     cex = 1)

text(usr[1],
     usr[4]-0.5,
     labels = 'temperate',
     pos=4,
     col = 'blue', cex=.7)

plot(
  p_pa[[2]])
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
     usr[4]-0.5,
     labels = 'Mediterranean',
     pos=4,
     col = 'blue', cex=.7)

par(op)
dev.off()
par(resetPar())
