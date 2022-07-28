##%######################################################%##
#                                                          #
####                    Supll Tab S5                    ####
#                                                          #
##%######################################################%##


semTable(
  fit1.b,
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
  file = ("output_tab/modb_Ccai"),
  print.results = T
)
