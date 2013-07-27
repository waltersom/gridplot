
library(gridplot)

set.seed(1234)
n <- 12^2
m0 <- matrix(rnorm(12*16, 4, 3), 12)
m1 <- matrix(rnorm(n, 4, 3), sqrt(n))
m2 <- matrix(rnorm(n, 3, 3), sqrt(n))
m3 <- matrix(rnorm(n, 3, 3), sqrt(n))
pal = brewer.pal(11,"PRGn")

annotation.height <- unit(2, "lines")

require(png)
img <- readPNG(system.file("img", "Rlogo.png", package="png"))

vp <- function(ii, jj, layout)
  viewport(layout=layout, layout.pos.row=ii, layout.pos.col=jj)

dev.new(width=6, height=4)
grid.newpage()

## split the page in 3x1 regions
pushViewport(viewport(layout=grid.layout(nrow=3, ncol=1,
                        heights=unit.c(unit(0.5, "npc") - 0.5*annotation.height,
                          annotation.height, unit(0.5, "npc") -
                          0.5*annotation.height -unit(2, "lines")))))
## navigate to the top region
pushViewport(viewport(layout.pos.row=1))
top.row <- row_layout(m1, m2, m3, colors = pal, width = unit(3, "cm"))
upViewport()
## navigate to the middle region
pushViewport(viewport(layout.pos.row=2))
## copy the layout from the top row, but with 1 row only
lay <- grid.layout(ncol=top.row$ncol, nrow=1, widths=top.row$widths)
pushViewport(viewport(layout=lay))
grid.rect(vp=vp(1, 2, lay), gp=gpar(lty=3))
grid.raster(img, vp=vp(1, 2, lay))
grid.rect(vp=vp(1, 4, lay), gp=gpar(lty=3))
grid.raster(img, vp=vp(1, 4, lay))
grid.rect(vp=vp(1, 6, lay), gp=gpar(lty=3))
grid.raster(img, vp=vp(1, 6, lay))
upViewport()
upViewport()
## navigate to the bottom region
pushViewport(viewport(layout.pos.row=3))
bottom.row <- row_layout(m1, m2, m3, colors = pal,
                         width = unit(3, "cm"), show.xlab=FALSE, title.x=NULL)
upViewport()




