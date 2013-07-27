\name{tmatrixGrob}
\alias{tmatrixGrob}
\title{tmatrix}
\usage{
  tmatrixGrob(t11, t12, t21, t22, legend = TRUE, width =
  unit(2, "cm"), palette = diverging_palette(do.call(c,
  list(t11, t12, t21, t22)), colors =
  RColorBrewer::brewer.pal(7, "RdBu"), center = TRUE),
  legend.params = list(), margin = unit(1, "mm"), spacing =
  unit(3, "mm"), at.x = NULL, at.y = NULL, xaxis = TRUE,
  yaxis = TRUE, labels.x = NULL, labels.y = NULL, title.x =
  expression(italic(k)), title.y = expression(italic(n)),
  gp.frame = gpar(fill = "transparent"), gp.names =
  gpar(cex = 5), point.negative = TRUE, debug = FALSE,
  show.names = TRUE, ...)
}
\arguments{
  \item{t11}{t11}

  \item{t12}{t12}

  \item{t21}{t21}

  \item{t22}{t22}

  \item{legend}{logical, add legend}

  \item{width}{block width}

  \item{palette}{see colorbarGrob}

  \item{colors}{see colorbarGrob}

  \item{center}{see colorbarGrob}

  \item{legend.params}{passed to colorbarGrob}

  \item{margin}{axis margin}

  \item{spacing}{spacing between blocks}

  \item{at.x}{see single_panel}

  \item{at.y}{see single_panel}

  \item{xaxis}{see single_panel}

  \item{yaxis}{see single_panel}

  \item{labels.x}{see single_panel}

  \item{labels.y}{see single_panel}

  \item{title.x}{see single_panel}

  \item{title.y}{see single_panel}

  \item{gp.frame}{frame gpar()}

  \item{point.negative}{see single_panel}

  \item{debug}{logical, show layout}

  \item{show.names}{logical, add block names}

  \item{...}{passed to gTree}
}
\value{
  gTree
}
\description{
  layout a T-matrix
}
\details{
  2x2 blocks with legend
}
\examples{
grid.newpage()
set.seed(1234)
N1 <- 13
N2 <- 20
N3 <- 15
N4 <- 12
foo <- identity # abs
d1 <- foo(matrix(rnorm(N1*N2), ncol=N1))
d2 <- foo(matrix(rnorm(N2*N3), ncol=N3))
d3 <- foo(matrix(rnorm(N1*N4), ncol=N1))
d4 <- foo(matrix(rnorm(N3*N4), ncol=N3))
grid.tmatrix(d1, d2, d3, d4, spacing=unit(1, "mm"), width=unit(5, "cm"))
}
\author{
  Baptiste Auguie
}
\seealso{
  Other matrix user_level: \code{\link{gridplot-package}},
  \code{\link{row_layout}}, \code{\link{single_panel}}
}

