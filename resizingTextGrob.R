resizingTextGrob <- function(...)
{
  grob(tg=textGrob(...), cl="resizingTextGrob")
}
preDrawDetails.resizingTextGrob <- function(x)
{
  h <- convertHeight(unit(1, "snpc"), "mm", valueOnly=TRUE)
  print(h)
  fs <- rescale(h, to=c(18, 7), from=c(120, 20))
  print(fs)
  pushViewport(viewport(gp = gpar(fontsize = fs)))
}

drawDetails.resizingTextGrob <- function(x, recording=TRUE)
{
  grid.draw(x$tg)
}
postDrawDetails.resizingTextGrob <- function(x)
  popViewport()


library(gridExtra)
a <- seq(2*pi, 2*pi/ 12, length=12) + pi/3
x <- cos(a) / 2
y <- sin(a) / 2
segs <- segmentsGrob(x*.2 + .5, y*.2+.5, x*.3 + .5, y*.3 + .5)
# the standard approach
tgs.1 <- textGrob(1:12, x*.4 + .5, y*.4 + .5)
# the new grob class
tgs.2 <- resizingTextGrob(1:12, x*.4 + .5, y*.4 + .5)
grid.arrange(grobTree(segs, tgs.1), grobTree(segs, tgs.2))
