\name{diverging_palette}
\alias{diverging_palette}
\title{diverging_palette}
\usage{
  diverging_palette(d = NULL, centered = FALSE, midpoint =
  0, colors = RColorBrewer::brewer.pal(7, "PRGn"))
}
\arguments{
  \item{d}{data giving the range of the palette}

  \item{centered}{logical, whether to use both sides from
  the midpoint symmetrically}

  \item{midpoint}{numeric value corresponding to the
  central colour}

  \item{colors}{vector of colors, length must be odd}
}
\value{
  a function
}
\description{
  diverging colour palette function with set midpoint
}
\details{
  returns a palette function that maps values to colours,
  with a midpoint (defaulting to 0) corresponding to the
  central colour
}
\examples{
grid.raster(diverging_palette(1:10, TRUE, mid=2, col=c("blue", "white", "red"))(1:10))
}
\author{
  baptiste Auguie
}
\seealso{
  Other low_level: \code{\link{colaxisGrob}},
  \code{\link{gradient_palette}}, \code{\link{rowaxisGrob}}
}

