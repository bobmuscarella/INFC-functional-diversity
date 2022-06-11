par(pty ="s",
    cex.main = 1.2,
    cex.lab = 1,
    font.main = 2,
    font.lab = 2,
    family ="sans",
    col.main = "gray10",
    col.lab = "gray10",
    fg = "gray10",
    las =1)

plot.new()
range(ordB$x[,1])
range(ordB$x[,2])
par(mar = c(5,5,5,5))
plot.window(xlim = c(-4,4),
            ylim = c(-4,4),
            asp = 1)

axis(side = 1,
     at = c(-4, -2, 0, 2, 4),
     labels = TRUE)

axis(side = 2,
     at = c(-4, -2, 0, 2, 4),
     labels = TRUE)

#title(main = "Biplot for functional trait PCA",
#      line = 3,
#      adj = 0.5)

title(xlab = paste("PC 1 (", round(summary(ordB)$importance[2]*100
                                   , digits = 1),
                   "%)",
                   sep = ""),
      ylab = paste("PC 2 (", round(summary(ordB)$importance[5]*100
                                   , digits = 1),
                   "%)",
                   sep = ""),
      cex = 1.5,
      line = 2,
      adj = 0.5)

points(x= ordB$x[,1:2],
       pch = 16,
       cex = 1,
       col = "orangered")


par(new = TRUE, las =1)

plot.window(xlim = c(-1, 1),
            ylim= c(-1, 1),
            asp = 1)

axis(side = 3,
     at = c(-1, -0.5, 0, 0.5, 1),
     labels = TRUE,
     col = "navy",
     col.ticks = NULL,
     lwd = 2,
     col.axis = "navy")

axis(side = 4,
     at = c(-1, -0.5, 0, 0.5, 1),
     labels = TRUE,
     col = "navy",
     col.ticks = NULL,
     lwd = 2,
     col.axis = "navy")


mtext((text= "PC1 rotations"),
      side = 3,
      cex = 1,
      font = 2,
      family = "sans",
      col = "grey10",
      line = 2.2)

mtext((text= "PC2 rotations"),
      side = 4,
      cex = 1,
      font = 2,
      family = "sans",
      col = "grey25",
      line = 2.2,
      las = 3)
box()

abline(v = 0, h = 0, lty = 2, col = "grey25")

arrows (x0 = 0, x1 = ordB$rotation[,1],
        y0 = 0, y1 = ordB$rotation[,2],
        col = "navy",
        length = 0,2,
        lwd = 2,
        angle = 30)

text(x = ordB$rotation[,1], y = ordB$rotation[,2],
     labels = row.names(ordB$rotation),
     cex=1,
     font = 2,
     col = "gray10",
     pos = c(1,1,2,1,1))

ucircle = cbind(cos((0:360)/180*pi),
                sin((0:360)/180*pi))

polygon(ucircle, 
        lty = "solid", border = "gray10", lwd =1)

