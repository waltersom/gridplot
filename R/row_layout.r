##' layout matrices in a row
##'
##' given N matrices, display them as a row with a common legend
##' x axes are displayed for matrices, y axis only for first
##' @export
##' @title row_layout
##' @param ... matrices, equal dimensions
##' @param width individual width of matrix
##' @param palette palette
##' @param colors colors
##' @param center center scale
##' @param legend.params list of legend parameters, see colorbarGrob
##' @param margin margin
##' @param spacing spacing between panels
##' @param at.x see single_panel
##' @param at.y see single_panel
##' @param xaxis see single_panel
##' @param yaxis see single_panel
##' @param labels.x see single_panel
##' @param labels.y see single_panel
##' @param show.xlab see colaxisGrob
##' @param show.ylab see colaxisGrob
##' @param title.x see single_panel
##' @param title.y see single_panel
##' @param point.negative see single_panel
##' @param debug logical, display grid layout
##' @return layout
##' @family matrix user_level
##' @author Baptiste Auguie
##' @examples
##' set.seed(1234)
##' n <- 30^2
##' m0 <- matrix(rnorm(12*16, 4, 3), 12)
##' m1 <- matrix(rnorm(n, 4, 3), sqrt(n))
##' m2 <- matrix(rnorm(n, 3, 3), sqrt(n))
##' m3 <- matrix(rnorm(n, 3, 3), sqrt(n))
##' pal = brewer.pal(11,"PRGn")
##' grid.newpage()
##' top.row <- row_layout(m1, m2, m3, colors = pal, width = unit(3, "cm"))
##' grid.newpage()
##' row_layout(m0)
##' grid.newpage()
##' row_layout(m1, m1, at.x=c(1, 3, 4, 6), labels.x=letters[1:4])
##' grid.newpage()
##' row_layout(m1, m1, center=FALSE)
##' grid.newpage()
##' row_layout(m0, m0, margin = unit(1, "cm"))
##' grid.newpage()
##' row_layout(m1, m1, spacing = unit(1, "cm"))
##' grid.newpage()
##' row_layout(m1, m1, xaxis=FALSE)
##' grid.newpage()
##' row_layout(m1, m1, title.y = "very long and\n tall \n title")
row_layout <- function(..., 
                       width=unit(5, "cm"),
                       palette = diverging_palette,
                       colors = RColorBrewer::brewer.pal(7,"RdBu"),
                       center=FALSE, legend.params = list(),
                       margin = unit(1.5, "mm"),
                       spacing = unit(0.3, "cm"),
                       at.x=NULL,
                       at.y=NULL,
                       xaxis=TRUE, yaxis=TRUE,
                       labels.x=NULL, 
                       labels.y=NULL,
                       show.xlab = TRUE, show.ylab=TRUE,
                       title.x = expression(italic(k)),
                       title.y = expression(italic(n)),
                       point.negative=TRUE,
                       debug=FALSE){
  
  matrices <- list(...)
  
  n <- length(matrices)
  if(n > 1 ){
  stopifnot(do.call(all.equal, lapply(matrices, nrow)))
  stopifnot(do.call(all.equal, lapply(matrices, ncol)))
  }
  
  nr <- nrow(matrices[[1]])
  nc <- ncol(matrices[[1]])
  height <- nr/nc*width
  pixel <- convertUnit(1/nc * width, "mm", valueOnly=TRUE)
  
  if(is.null(at.x))  at.x <- unique(round(grid.pretty(c(1,nc)))) 
  if(is.null(at.y))  at.y <- unique(round(grid.pretty(c(1,nr)))) 
  if(is.null(labels.x))  labels.x <- at.x
  if(is.null(labels.y))  labels.y <- at.y
  
    
  combined <- do.call(c, matrices)
  range <- range(combined)
  
  pal <- palette(range, colors = colors, center=center)

  ## matrices
  grobs <- list()
  ## left matrix has a row axis
  
  
  grobs[[1]] <- single_panel(matrices[[1]], point.negative=point.negative,
                                palette = pal, at.y = at.y, at.x=at.x, width=width,
                             labels.x=labels.x,labels.y=labels.y, margin=margin,
                             show.xlab = show.xlab, show.ylab=show.ylab,
                             xaxis=xaxis, yaxis=yaxis, title.y=title.y, title.x=title.x,
                             frame.gp = gpar(fill="transparent", col="grey50", lwd=0.5))
  ## remaining matrices, if any
  for(ii in seq.int(from=2, to = n, length.out = n-1)){
    grobs[[ii]] <- single_panel(matrices[[ii]],
                                point.negative=point.negative, width=width,
                                palette = pal, at.y = at.y, at.x=at.x, title.x=title.x,
                                labels.x=labels.x,labels.y=NULL,
                                show.xlab = show.xlab, show.ylab=show.ylab,
                                xaxis=xaxis, yaxis=TRUE, title.y=NULL, margin=margin,
                                frame.gp = gpar(fill="transparent", col="grey50", lwd=0.5))

    }
  grobs[[n+1]] <- do.call(colorbarGrob,
                          modifyList(list(d = range, palette=pal,
                                          x=unit(0, "npc"), point.negative=point.negative,
                                          y = unit(0, "npc"), gap=pixel, 
                                          height = height), legend.params))

  

  ## define the widths for the layout
  ## trick: embed units in a list to use correct `[<-` replacement functions
  widths <- rep(list(width), 2*n + 2)
  widths[seq(1, by=2, to=2*n+1)] <- list(spacing)
  widths[1] <- list(convertUnit(grobs[[1]]$border.x, "cm"))
  ## TODO: fix widthDetails method for legend ?
  widths[2*n+2] <- list(convertUnit(grobWidth(grobs[[n+1]]), "cm"))
  ## finally recombine the unit object
  widths <- do.call(unit.c, widths)
  annotation.height <- convertUnit(grobs[[1]]$border.y, "cm") 

  ## make a layout
  gl <- grid.layout(ncol=2*n+2, nrow=2,
                    width= widths,
                  height = unit.c(annotation.height, height))
  ## helping function
  matvp <- function(ii, just=c("right", "bottom"))
    viewport(x=1, layout=gl, just=just,
             layout.pos.col = ii, layout.pos.row = 2)

  ## debugging
  if(debug)  grid.show.layout(gl)
  pushViewport(viewport(x=0,layout=gl, just=c("left", "center")))
  
  ## draw the matrices
  for (ii in seq_along(matrices)){
    grobs[[ii]] <- editGrob(grobs[[ii]], vp=matvp(2*ii))
  }
  grobs[[n+1]] <- editGrob(grobs[[n+1]], vp=matvp(2*n+2))
  
  grid.draw(gTree(children=do.call(gList, grobs)))
  
  upViewport()

  invisible(gl)
  
}

