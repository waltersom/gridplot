\name{colaxisGrob}
\alias{colaxisGrob}
\title{colaxisGrob}
\usage{
  colaxisGrob(x, y, tick.length = unit(0.5, "mm"), gp.ticks
  = gpar(), show.lab = TRUE, margin = unit(1, "mm"), labels
  = seq_along(x), gp.labels = gpar(col = "grey50", fontsize
  = 8, fontface = "italic"), ...)
}
\arguments{
  \item{x}{x}

  \item{y}{y}

  \item{tick.length}{tick length}

  \item{gp.ticks}{gp.ticks}

  \item{show.lab}{logical, display labels}

  \item{margin}{margin between ticks and labels}

  \item{labels}{labels}

  \item{gp.labels}{gp.labels}

  \item{...}{passed to gTree}
}
\value{
  gTree
}
\description{
  column axis
}
\details{
  draws a minimal axis with tick marks and labels above
  designated matrix columns
}
\author{
  Baptiste Auguie
}
\seealso{
  Other low_level: \code{\link{diverging_palette}},
  \code{\link{gradient_palette}}, \code{\link{rowaxisGrob}}
}

