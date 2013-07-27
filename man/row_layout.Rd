\name{row_layout}
\alias{row_layout}
\title{row_layout}
\usage{
  row_layout(..., width = unit(5, "cm"), palette =
  diverging_palette, colors = RColorBrewer::brewer.pal(7,
  "RdBu"), center = FALSE, legend.params = list(), margin =
  unit(1.5, "mm"), spacing = unit(0.3, "cm"), at.x = NULL,
  at.y = NULL, xaxis = TRUE, yaxis = TRUE, labels.x = NULL,
  labels.y = NULL, show.xlab = TRUE, show.ylab = TRUE,
  title.x = expression(italic(k)), title.y =
  expression(italic(n)), point.negative = TRUE, debug =
  FALSE)
}
\arguments{
  \item{...}{matrices, equal dimensions}

  \item{width}{individual width of matrix}

  \item{palette}{palette}

  \item{colors}{colors}

  \item{center}{center scale}

  \item{legend.params}{list of legend parameters, see
  colorbarGrob}

  \item{margin}{margin}

  \item{spacing}{spacing between panels}

  \item{at.x}{see single_panel}

  \item{at.y}{see single_panel}

  \item{xaxis}{see single_panel}

  \item{yaxis}{see single_panel}

  \item{labels.x}{see single_panel}

  \item{labels.y}{see single_panel}

  \item{show.xlab}{see colaxisGrob}

  \item{show.ylab}{see colaxisGrob}

  \item{title.x}{see single_panel}

  \item{title.y}{see single_panel}

  \item{point.negative}{see single_panel}

  \item{debug}{logical, display grid layout}
}
\value{
  layout
}
\description{
  layout matrices in a row
}
\details{
  given N matrices, display them as a row with a common
  legend x axes are displayed for matrices, y axis only for
  first
}
\examples{
set.seed(1234)
n <- 30^2
m0 <- matrix(rnorm(12*16, 4, 3), 12)
m1 <- matrix(rnorm(n, 4, 3), sqrt(n))
m2 <- matrix(rnorm(n, 3, 3), sqrt(n))
m3 <- matrix(rnorm(n, 3, 3), sqrt(n))
pal = brewer.pal(11,"PRGn")
grid.newpage()
top.row <- row_layout(m1, m2, m3, colors = pal, width = unit(3, "cm"))
grid.newpage()
row_layout(m0)
grid.newpage()
row_layout(m1, m1, at.x=c(1, 3, 4, 6), labels.x=letters[1:4])
grid.newpage()
row_layout(m1, m1, center=FALSE)
grid.newpage()
row_layout(m0, m0, margin = unit(1, "cm"))
grid.newpage()
row_layout(m1, m1, spacing = unit(1, "cm"))
grid.newpage()
row_layout(m1, m1, xaxis=FALSE)
grid.newpage()
row_layout(m1, m1, title.y = "very long and\\n tall \\n title")
}
\author{
  Baptiste Auguie
}
\seealso{
  Other matrix user_level: \code{\link{gridplot-package}},
  \code{\link{single_panel}}, \code{\link{tmatrixGrob}}
}

