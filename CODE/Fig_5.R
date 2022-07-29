##%######################################################%##
#                                                          #
####                        modA                        ####
#                                                          #
##%######################################################%##
pdf(
  "output_plot/Fig_5.pdf",
  width = 5,
  height = 3.5
)

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 2,6, byrow = T))

par(mar = c(0,0,0,0), oma = c(0.5,5,0.5,3), las =1)

par(mar = c(4.5,0,0,0)) #Add a space in the bottom axis


mypch<-21
mycols<-c('dodgerblue', 'indianred')
myylim<-c(-0.1,0.3)
mylabs<-c( "T", "M")

value<-c(
  #parameterEstimates(fit1.a,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.a,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.a,standardized = TRUE)[16,13])
low<-c(
  #parameterEstimates(fit1.a,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.a,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.a,standardized = TRUE)[16,11])
upp<-c(
  #parameterEstimates(fit1.a,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.a,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.a,standardized = TRUE)[16,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  lty=1, # line type: see help(par)
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(FDis[SeedMass]), # x axis label
  ylim=myylim,
  ylab=expression(beta)
)
# axis(side=1,at=1:2,  ## add custom x-axis
#      label=mylabs)
abline(h=0, lty=2, col='grey')
legend('topleft', legend=c("Temperate", "Mediterranean"),
        col=mycols, pch = 16, box.lty=0, inset=0.02, cex = .5)

#######################################################################
value<-c(
  #parameterEstimates(fit1.b,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.b,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.b,standardized = TRUE)[16,13])
low<-c(
  #parameterEstimates(fit1.b,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.b,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.b,standardized = TRUE)[16,11])
upp<-c(
  #parameterEstimates(fit1.b,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.b,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.b,standardized = TRUE)[16,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(FDis[Height]),
  ylim=myylim,
  ylab='',yaxt='n'
)
abline(h=0, lty=2, col='grey')

#######################################################################
value<-c(
  #parameterEstimates(fit1.c,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.c,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.c,standardized = TRUE)[16,13])
low<-c(
  #parameterEstimates(fit1.c,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.c,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.c,standardized = TRUE)[16,11])
upp<-c(
  #parameterEstimates(fit1.c,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.c,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.c,standardized = TRUE)[16,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(FDis[SLA]),
  ylim=myylim,
  ylab='',yaxt='n'
)
abline(h=0, lty=2, col='grey')
#######################################################################
value<-c(
  #parameterEstimates(fit1.d,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.d,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.d,standardized = TRUE)[16,13])
low<-c(
  #parameterEstimates(fit1.d,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.d,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.d,standardized = TRUE)[16,11])
upp<-c(
  #parameterEstimates(fit1.d,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.d,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.d,standardized = TRUE)[16,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(FDis[WD]),
  ylim=myylim,
  ylab='',yaxt='n'
)
abline(h=0, lty=2, col='grey')
#######################################################################
value<-c(
  #parameterEstimates(fit1.e,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.e,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.e,standardized = TRUE)[16,13])
low<-c(
  #parameterEstimates(fit1.e,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.e,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.e,standardized = TRUE)[16,11])
upp<-c(
  #parameterEstimates(fit1.e,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.e,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.e,standardized = TRUE)[16,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(FDis[Xylem]),
  ylim=myylim,
  ylab='',yaxt='n'
)
abline(h=0, lty=2, col='grey')
#######################################################################
value<-c(
  #parameterEstimates(fit1.f,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.f,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.f,standardized = TRUE)[16,13])
low<-c(
  #parameterEstimates(fit1.f,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.f,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.f,standardized = TRUE)[16,11])
upp<-c(
  #parameterEstimates(fit1.f,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.f,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.f,standardized = TRUE)[16,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(FDis[all]),
  ylim=myylim,
  ylab='',yaxt='n'
)
abline(h=0, lty=2, col='grey')

############################################################
############################################################
myylim<-c(-0.15,0.3)


value<-c(
  #parameterEstimates(fit1.a,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.a,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.a,standardized = TRUE)[15,13])

low<-c(
  #parameterEstimates(fit1.a,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.a,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.a,standardized = TRUE)[15,11])

upp<-c(
  #parameterEstimates(fit1.a,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.a,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.a,standardized = TRUE)[15,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  lty=1, # line type: see help(par)
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(CWM[SeedMass]),
  ylim=myylim,
  ylab=expression(beta)
  
)
abline(h=0, lty=2, col='grey')



############################################################
value<-c(
  #parameterEstimates(fit1.b,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.b,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.b,standardized = TRUE)[15,13])
low<-c(
  #parameterEstimates(fit1.b,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.b,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.b,standardized = TRUE)[15,11])
upp<-c(
  #parameterEstimates(fit1.b,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.b,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.b,standardized = TRUE)[15,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  lty=1, # line type: see help(par)
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(CWM[Height]),
  ylim=myylim,
  ylab='', yaxt = 'n'
)
abline(h=0, lty=2, col='grey')

############################################################
value<-c(
  #parameterEstimates(fit1.c,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.c,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.c,standardized = TRUE)[15,13])
low<-c(
  #parameterEstimates(fit1.c,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.c,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.c,standardized = TRUE)[15,11])
upp<-c(
  #parameterEstimates(fit1.c,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.c,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.c,standardized = TRUE)[15,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  lty=1, # line type: see help(par)
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(CWM[SLA]),
  ylim=myylim,
  ylab='', yaxt = 'n',
  
)
abline(h=0, lty=2, col='grey')

############################################################
value<-c(
  #parameterEstimates(fit1.d,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.d,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.d,standardized = TRUE)[15,13])
low<-c(
  #parameterEstimates(fit1.d,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.d,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.d,standardized = TRUE)[15,11])
upp<-c(
  #parameterEstimates(fit1.d,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.d,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.d,standardized = TRUE)[15,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  lty=1, # line type: see help(par)
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(CWM[WD]),
  ylim=myylim,
  ylab='', yaxt = 'n',
  
)
abline(h=0, lty=2, col='grey')

############################################################
value<-c(
  #parameterEstimates(fit1.e,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.e,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.e,standardized = TRUE)[15,13])
low<-c(
  #parameterEstimates(fit1.e,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.e,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.e,standardized = TRUE)[15,11])
upp<-c(
  #parameterEstimates(fit1.e,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.e,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.e,standardized = TRUE)[15,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  lty=1, # line type: see help(par)
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(CWM[Xylem]),
  ylim=myylim,
  ylab='', yaxt = 'n',
  
)
abline(h=0, lty=2, col='grey')

############################################################
value<-c(
  #parameterEstimates(fit1.f,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.f,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.f,standardized = TRUE)[15,13])
low<-c(
  #parameterEstimates(fit1.f,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.f,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.f,standardized = TRUE)[15,11])
upp<-c(
  #parameterEstimates(fit1.f,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.f,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.f,standardized = TRUE)[15,12])

gplots::plotCI(
  1:2,
  value,
  li	= low,
  ui=upp,
  pch=mypch, # symbol (plotting character) type: see help(pch); 24 = filled triangle pointing up
  pt.bg=mycols, # fill colour for symbol
  cex=1, # symbol size multiplier
  lty=1, # line type: see help(par)
  type="p", # p=points, l=lines, b=both, o=overplotted points/lines, etc.; see help(plot.default)
  gap=0, # distance from symbol to error bar
  sfrac=0.005, # width of error bar as proportion of x plotting region (default 0.01)
  xaxt = 'n',
  xlim=c(0.5,2.5),
  xaxp=c(1,2,1), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab=expression(CWM[Dim1]),
  ylim=myylim,
  ylab='', yaxt = 'n'
)

abline(h=0, lty=2, col='grey')


par(op)
dev.off()
par(resetPar())
