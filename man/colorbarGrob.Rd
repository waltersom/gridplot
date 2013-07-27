\name{colorbarGrob}
\alias{colorbarGrob}
\alias{grid.colorbar}
\title{colorbarGrob}
\usage{
  colorbarGrob(d, x = unit(0.5, "npc"), y = unit(0.1,
  "npc"), height = unit(0.8, "npc"), width = unit(0.5,
  "cm"), margin = unit(1, "mm"), tick.length = 0.2 * width,
  pretty.breaks = grid.pretty(range(d)), digits = 2,
  show.extrema = TRUE, palette = diverging_palette(d), n =
  100, point.negative = TRUE, gap = 5, interpolate = TRUE,
  ...)
}
\arguments{
  \item{d}{data to map}

  \item{x}{x}

  \item{y}{y}

  \item{height}{height}

  \item{width}{width}

  \item{margin}{space before text}

  \item{tick.length}{tick length}

  \item{pretty.breaks}{pretty breaks}

  \item{digits}{precision of labels}

  \item{show.extrema}{logical, show extreme values}

  \item{palette}{palette}

  \item{n}{resolution of colors}

  \item{point.negative}{logical, draw points in negative
  spots}

  \item{gap}{gap between points in mm}

  \item{interpolate}{logical, passed to grid.raster}

  \item{...}{additional params for the gTree}
}
\value{
  a gTree
}
\description{
  draw a colorscale with tick marks
}
\details{
  draw a colorscale with tick marks
}
\examples{
set.seed(1234)
m <- rnorm(100, 4, 3)
library(RColorBrewer)
pal1 = brewer.pal(3,"PRGn")
grid.newpage()
grid.colorbar(m, x=unit(0.3, "npc"), palette = diverging_palette(m, colors=pal1, center=TRUE))
grid.colorbar(m)
grid.colorbar(m, x=unit(0.7, "npc"), show.extrema=FALSE,
n=30, interpolate=FALSE,
width=unit(1,"in"),tick.length = unit(1,"lines"),
gp=gpar(lwd=2, col="grey50"))
}
\author{
  baptiste Auguie
}

