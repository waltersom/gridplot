\name{rowaxisGrob}
\alias{rowaxisGrob}
\title{rowaxisGrob}
\usage{
  rowaxisGrob(x, y, tick.length = unit(0.5, "mm"), gp.ticks
  = gpar(), show.lab = TRUE, margin = unit(1, "mm"), labels
  = seq_along(y), gp.labels = gpar(col = "grey50", fontsize
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
  row axis
}
\details{
  draws a minimal axis with tick marks and labels on the
  left side of designated matrix rows
}
\author{
  Baptiste Auguie
}
\seealso{
  Other low_level: \code{\link{colaxisGrob}},
  \code{\link{diverging_palette}},
  \code{\link{gradient_palette}}
}

