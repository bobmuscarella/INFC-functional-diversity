##%######################################################%##
#                                                          #
####                        modA                        ####
#                                                          #
##%######################################################%##
pdf(
  "output_plot/Fig_5.pdf",
  width = 6,
  height = 3.5
)

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 2,6, byrow = T))

par(mar = c(0,0,0,0), oma = c(0.5,4,0.5,1), las =1)

par(mar = c(2.5,0,0,0)) #Add a space in the bottom axis


mypch<-21
mycols<-c('black','dodgerblue', 'indianred')
myylim<-c(-0.1,0.3)
mylabs<-c("P", "T", "M")

value<-c(
  parameterEstimates(fit1.a,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.a,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.a,standardized = TRUE)[16,13])
low<-c(
  parameterEstimates(fit1.a,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.a,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.a,standardized = TRUE)[16,11])
upp<-c(
  parameterEstimates(fit1.a,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.a,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.a,standardized = TRUE)[16,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab=expression(F[Dis])
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')
# legend('topleft', legend=c("Pooled", "Temperate", "Mediterranean"),
#        col=mycols, pch = mypch, box.lty=0, inset=0.02)

#######################################################################
value<-c(
  parameterEstimates(fit1.b,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.b,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.b,standardized = TRUE)[16,13])
low<-c(
  parameterEstimates(fit1.b,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.b,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.b,standardized = TRUE)[16,11])
upp<-c(
  parameterEstimates(fit1.b,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.b,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.b,standardized = TRUE)[16,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab='',yaxt='n'
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')

#######################################################################
value<-c(
  parameterEstimates(fit1.c,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.c,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.c,standardized = TRUE)[16,13])
low<-c(
  parameterEstimates(fit1.c,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.c,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.c,standardized = TRUE)[16,11])
upp<-c(
  parameterEstimates(fit1.c,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.c,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.c,standardized = TRUE)[16,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab='',yaxt='n'
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')
#######################################################################
value<-c(
  parameterEstimates(fit1.d,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.d,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.d,standardized = TRUE)[16,13])
low<-c(
  parameterEstimates(fit1.d,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.d,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.d,standardized = TRUE)[16,11])
upp<-c(
  parameterEstimates(fit1.d,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.d,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.d,standardized = TRUE)[16,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab='',yaxt='n'
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')
#######################################################################
value<-c(
  parameterEstimates(fit1.e,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.e,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.e,standardized = TRUE)[16,13])
low<-c(
  parameterEstimates(fit1.e,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.e,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.e,standardized = TRUE)[16,11])
upp<-c(
  parameterEstimates(fit1.e,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.e,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.e,standardized = TRUE)[16,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab='',yaxt='n'
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')
#######################################################################
value<-c(
  parameterEstimates(fit1.f,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.f,standardized = TRUE)[3,13],
  parameterEstimates(fit1mg.f,standardized = TRUE)[16,13])
low<-c(
  parameterEstimates(fit1.f,standardized = TRUE)[3,9],
  parameterEstimates(fit1mg.f,standardized = TRUE)[3,11],
  parameterEstimates(fit1mg.f,standardized = TRUE)[16,11])
upp<-c(
  parameterEstimates(fit1.f,standardized = TRUE)[3,10],
  parameterEstimates(fit1mg.f,standardized = TRUE)[3,12],
  parameterEstimates(fit1mg.f,standardized = TRUE)[16,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab='',yaxt='n'
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')

############################################################
############################################################
myylim<-c(-0.15,0.3)


value<-c(
  parameterEstimates(fit1.a,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.a,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.a,standardized = TRUE)[15,13])

low<-c(
  parameterEstimates(fit1.a,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.a,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.a,standardized = TRUE)[15,11])

upp<-c(
  parameterEstimates(fit1.a,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.a,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.a,standardized = TRUE)[15,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab=expression(CWM)
  
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')



############################################################
value<-c(
  parameterEstimates(fit1.b,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.b,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.b,standardized = TRUE)[15,13])
low<-c(
  parameterEstimates(fit1.b,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.b,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.b,standardized = TRUE)[15,11])
upp<-c(
  parameterEstimates(fit1.b,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.b,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.b,standardized = TRUE)[15,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab='', yaxt = 'n'
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')

############################################################
value<-c(
  parameterEstimates(fit1.c,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.c,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.c,standardized = TRUE)[15,13])
low<-c(
  parameterEstimates(fit1.c,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.c,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.c,standardized = TRUE)[15,11])
upp<-c(
  parameterEstimates(fit1.c,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.c,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.c,standardized = TRUE)[15,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab='', yaxt = 'n',
  
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')

############################################################
value<-c(
  parameterEstimates(fit1.d,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.d,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.d,standardized = TRUE)[15,13])
low<-c(
  parameterEstimates(fit1.d,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.d,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.d,standardized = TRUE)[15,11])
upp<-c(
  parameterEstimates(fit1.d,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.d,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.d,standardized = TRUE)[15,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab='', yaxt = 'n',
  
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')

############################################################
value<-c(
  parameterEstimates(fit1.e,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.e,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.e,standardized = TRUE)[15,13])
low<-c(
  parameterEstimates(fit1.e,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.e,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.e,standardized = TRUE)[15,11])
upp<-c(
  parameterEstimates(fit1.e,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.e,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.e,standardized = TRUE)[15,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab='', yaxt = 'n',
  
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')

############################################################
value<-c(
  parameterEstimates(fit1.f,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.f,standardized = TRUE)[2,13],
  parameterEstimates(fit1mg.f,standardized = TRUE)[15,13])
low<-c(
  parameterEstimates(fit1.f,standardized = TRUE)[2,9],
  parameterEstimates(fit1mg.f,standardized = TRUE)[2,11],
  parameterEstimates(fit1mg.f,standardized = TRUE)[15,11])
upp<-c(
  parameterEstimates(fit1.f,standardized = TRUE)[2,10],
  parameterEstimates(fit1mg.f,standardized = TRUE)[2,12],
  parameterEstimates(fit1mg.f,standardized = TRUE)[15,12])

gplots::plotCI(
  1:3,
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
  xlim=c(0.5,3.5),
  xaxp=c(1,3,2), # x-min tick mark, x-max tick mark, number of intervals between tick marks
  xlab="", # x axis label
  ylim=myylim,
  ylab='', yaxt = 'n',
  
)
axis(side=1,at=1:3,  ## add custom x-axis
     label=mylabs)
abline(h=0, lty=2, col='grey')


par(op)
dev.off()
par(resetPar())
