\name{gradient_palette}
\alias{gradient_palette}
\title{gradient_palette}
\usage{
  gradient_palette(d = NULL, low = "white", high = "black",
  ...)
}
\arguments{
  \item{d}{data giving the range of the palette}

  \item{low}{low color}

  \item{high}{high color}

  \item{...}{ignored}
}
\value{
  a function
}
\description{
  gradient palette function
}
\details{
  returns a palette function that maps values between 2
  colors
}
\examples{
grid.raster(gradient_palette(1:10)(1:10))
}
\author{
  baptiste Auguie
}
\seealso{
  Other low_level: \code{\link{colaxisGrob}},
  \code{\link{diverging_palette}},
  \code{\link{rowaxisGrob}}
}

