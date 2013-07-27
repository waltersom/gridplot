\name{dmatrixGrob}
\alias{dmatrixGrob}
\alias{grid.dmatrix}
\title{dmatrixGrob}
\usage{
  dmatrixGrob(d, point.negative = TRUE, palette =
  diverging_palette(d, center = FALSE), frame.gp =
  gpar(fill = "transparent", col = "grey50"), ...)
}
\arguments{
  \item{d}{matrix}

  \item{point.negative}{logical, add points to negative
  values}

  \item{palette}{palette}

  \item{frame.gp}{gpar for the frame}

  \item{...}{passed to gTree}
}
\value{
  gTree
}
\description{
  display a matrix
}
\details{
  colormap of a matrix with top and left labels
}
\examples{
set.seed(1234)
n <- 30^2
m <- matrix(rnorm(n, 4, 3), sqrt(n))
pal = brewer.pal(11,"PRGn")
grid.newpage()
g <- dmatrixGrob(m)
grid.draw(g)
}
\author{
  baptiste Auguie
}

