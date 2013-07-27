\name{single_panel}
\alias{single_panel}
\title{single_panel}
\usage{
  single_panel(d, width = unit(5, "cm"), palette =
  diverging_palette(d, center = FALSE, colors =
  RColorBrewer::brewer.pal(7, "RdBu")), margin = unit(1.5,
  "mm"), at.x = unique(round(grid.pretty(c(1, ncol(d))))),
  at.y = unique(round(grid.pretty(c(1, nrow(d))))), xaxis =
  TRUE, yaxis = TRUE, labels.x = at.x, labels.y = at.y,
  show.xlab = TRUE, show.ylab = TRUE, title.x =
  expression(italic(k)), title.y = expression(italic(n)),
  point.negative = TRUE, ...)
}
\arguments{
  \item{d}{matrix}

  \item{width}{matrix width}

  \item{palette}{palette}

  \item{margin}{margin}

  \item{at.x}{locations of ticks marks}

  \item{at.y}{locations of ticks marks}

  \item{xaxis}{logical}

  \item{yaxis}{logical}

  \item{labels.x}{labels of ticks marks}

  \item{labels.y}{labels of ticks marks}

  \item{show.xlab}{see colaxisGrob}

  \item{show.ylab}{see rowaxisGrob}

  \item{title.x}{x title}

  \item{title.y}{y title}

  \item{point.negative}{logical}

  \item{...}{passed to gTree}
}
\value{
  gTree, with border.width and border.height
}
\description{
  layout a single matrix with optional axis annotations
}
\details{
  layout a single matrix with optional axis annotations
}
\author{
  Baptiste Auguie
}
\seealso{
  Other matrix user_level: \code{\link{gridplot-package}},
  \code{\link{row_layout}}, \code{\link{tmatrixGrob}}
}

