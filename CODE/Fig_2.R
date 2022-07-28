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

# png(
#   "output_plot/Mod_a.jpg",
#   width = 5,
#   height = 5,
#   units = 'in',
#   res = 300
# )
pdf('output_plot/Mod_a.pdf', width = 5, height = 4.5)
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

# png(
#   "output_plot/Mod_b.jpg",
#   width = 5,
#   height = 5,
#   units = 'in',
#   res = 300
# )
pdf('output_plot/Mod_b.pdf', width = 5, height = 4.5)
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

# png(
#   "output_plot/Mod_c.jpg",
#   width = 5,
#   height = 5,
#   units = 'in',
#   res = 300
# )
pdf('output_plot/Mod_c.pdf', width = 5, height = 4.5)
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

# png(
#   "output_plot/Mod_d.jpg",
#   width = 5,
#   height = 5,
#   units = 'in',
#   res = 300
# )
pdf('output_plot/Mod_d.pdf', width = 5, height = 4.5)
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

# png(
#   "output_plot/Mod_e.jpg",
#   width = 5,
#   height = 5,
#   units = 'in',
#   res = 300
# )
pdf('output_plot/Mod_e.pdf', width = 5, height = 4.5)
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

# png(
#   "output_plot/Mod_f.jpg",
#   width = 5,
#   height = 5,
#   units = 'in',
#   res = 300
# )
pdf('output_plot/Mod_f.pdf', width = 5, height = 4.5)
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

par(resetPar)
