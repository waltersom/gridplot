\docType{package}
\name{gridplot-package}
\alias{gridplot}
\alias{gridplot-package}
\title{gridplot}
\description{
  grid graphics functions to display matrices
}
\examples{
library(gridplot) ; set.seed(123) ; m = matrix(rnorm(21), 3) ; grid.newpage() ; row_layout(m, m, legend.params=list(digits=2, pretty.breaks = c(-1, 0, 1, -2), show.extrema=FALSE))
}
\author{
  baptiste Auguie \email{baptiste.auguie@gmail.com}
}
\seealso{
  Other matrix user_level: \code{\link{row_layout}},
  \code{\link{single_panel}}, \code{\link{tmatrixGrob}}
}
\keyword{packagelibrary}

