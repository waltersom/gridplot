
makeImageRect <- 
function (nrow, ncol, cols, byrow) 
{
    xx <- (1:ncol)/ncol
    yy <- (1:nrow)/nrow
    if (byrow) {
        right <- rep(xx, nrow)
        top <- rep(yy, each = ncol)
    }
    else {
        right <- rep(xx, each = nrow)
        top <- rep(yy, ncol)
    }
    rectGrob(x = right, y = top, width = 1/ncol, height = 1/nrow, 
        just = c("right", "top"), gp = gpar(col = cols, fill = cols), 
        name = "image")
}

imageGrob <- 
function (nrow, ncol, cols, byrow = TRUE, name = NULL, gp = NULL, 
    vp = NULL) 
{
    igt <- gTree(nrow = nrow, ncol = ncol, cols = cols, byrow = byrow, 
        children = gList(makeImageRect(nrow, ncol, cols, byrow)), 
        gp = gp, name = name, vp = vp, cl = "imageGrob")
    igt
}

##'
##'
##' ##' gradient palette function 
##'
##' returns a palette function that maps values between 2 colors
##' @title gradient_palette
##' @export
##' @param d data giving the range of the palette
##' @param low low color
##' @param high high color
##' @param ... ignored
##' @return a function
##' @author baptiste Auguie
##' @family low_level
##' @examples
##' grid.raster(gradient_palette(1:10)(1:10))
gradient_palette <- function(d = NULL, low="white", high="black", ...){
  
  function(x){
    scales::seq_gradient_pal(low, high)((x - min(d))/diff(range(d)) * 1 + 0)
    }
  
}

##' diverging colour palette function with set midpoint
##'
##' returns a palette function that maps values to colours, with
##' a midpoint (defaulting to 0) corresponding to the central colour
##' @title diverging_palette
##' @export
##' @param d data giving the range of the palette
##' @param centered logical, whether to use both sides from the midpoint symmetrically
##' @param midpoint numeric value corresponding to the central colour
##' @param colors vector of colors, length must be odd
##' @return a function
##' @author baptiste Auguie
##' @family low_level
##' @examples
##' grid.raster(diverging_palette(1:10, TRUE, mid=2, col=c("blue", "white", "red"))(1:10))
diverging_palette <- function(d = NULL, centered = FALSE, midpoint = 0,
                              colors = RColorBrewer::brewer.pal(7,"PRGn")){

  half <- length(colors)/2

  if(!length(colors)%%2)
    stop("requires odd number of colors")
  if( !centered && !(midpoint <=  max(d) && midpoint >= min(d)))
    warning("Midpoint is outside the data range!")
  
  values <-  if(!centered) {
    low <- seq(min(d), midpoint, length=half)
    high <- seq(midpoint, max(d), length=half)
    c(low[-length(low)], midpoint, high[-1])
  } else {
    mabs <- max(abs(d - midpoint))
    seq(midpoint-mabs, midpoint + mabs, length=length(colors))
  }
  
  scales::gradient_n_pal(colors, values = values)
  
}

##' draw a colorscale with tick marks
##'
##' draw a colorscale with tick marks
##' @title colorbarGrob
##' @export
##' @aliases grid.colorbar
##' @param d data to map
##' @param x x
##' @param y y
##' @param height height
##' @param width width
##' @param margin space before text
##' @param tick.length tick length
##' @param pretty.breaks pretty breaks
##' @param digits precision of labels
##' @param show.extrema logical, show extreme values
##' @param palette palette
##' @param n resolution of colors
##' @param point.negative logical, draw points in negative spots
##' @param gap gap between points in mm
##' @param interpolate logical, passed to grid.raster
##' @param ... additional params for the gTree
##' @return a gTree
##' @author baptiste Auguie
##' @family user_level
##' @examples
##' set.seed(1234)
##' m <- rnorm(100, 4, 3)
##' library(RColorBrewer)
##' pal1 = brewer.pal(3,"PRGn")
##' grid.newpage()
##' grid.colorbar(m, x=unit(0.3, "npc"), palette = diverging_palette(m, colors=pal1, center=TRUE))
##' grid.colorbar(m)
##' grid.colorbar(m, x=unit(0.7, "npc"), show.extrema=FALSE,
##'               n=30, interpolate=FALSE,
##'               width=unit(1,"in"),tick.length = unit(1,"lines"),
##'               gp=gpar(lwd=2, col="grey50"))
colorbarGrob <- function(d, x = unit(0.5, "npc"), 
                         y = unit(0.1,"npc"),
                         height=unit(0.8,"npc"),
                         width=unit(0.5, "cm"), size=0.7,
                         margin=unit(1,"mm"), tick.length=0.2*width,
                         pretty.breaks = grid.pretty(range(d)),
                         digits = 2, show.extrema=TRUE,
                         palette = diverging_palette(d), n = 1e2,
                         point.negative=TRUE,   gap =5,
                         interpolate=TRUE,
                         ...){

  ## includes extreme limits of the data
  legend.vals <- unique(round(sort(c(pretty.breaks, min(d), max(d))), digits)) 

  legend.labs <- if(show.extrema)
    legend.vals else unique(round(sort(pretty.breaks), digits)) 

  ## interpolate the colors
  colors <- palette(seq(min(d), max(d), length=n))
  ## 1D strip of colors, from bottom <-> min(d) to top <-> max(d)
  lg <- rasterGrob(rev(colors), # rasterGrob draws from top to bottom
                   y=y, interpolate=interpolate,
                   x=x, just=c("left", "bottom"),
                   width=width, height=height)
  
  
  ## box around color strip
  bg <- rectGrob(x=x, y=y, just=c("left", "bottom"),
                 width=width, height=height, gp=gpar(fill="transparent"))

  ## positions of the tick marks
  pos.y <- y + height * rescale(legend.vals)
  if(!show.extrema) pos.y <-  pos.y[-c(1, length(pos.y))]

  ## tick labels
  ltg <- textGrob(legend.labs, x = x + width + margin, y=pos.y,
                          just=c("left", "center"))
  ## right tick marks
  rticks <- segmentsGrob(y0=pos.y, y1=pos.y,
                         x0 = x + width,
                         x1 = x + width - tick.length,
                         gp=gpar())
  ## left tick marks
 lticks <- segmentsGrob(y0=pos.y, y1=pos.y,
                        x0 = x ,
                        x1 = x + tick.length,
                        gp=gpar())
  
  ## position of the dots
  if(any( d < 0 )){
  yneg <- diff(range(c(0, d[d<0])))/diff(range(d))  * height
  clipvp <- viewport(clip=TRUE, x=x, y=y, width=width, height=yneg,
                     just=c("left", "bottom"))
  h <- convertUnit(yneg, "mm", "y", valueOnly=TRUE)

  pos <- seq(0, to=h, by=gap)
  }
  ## coloured dots
  cg <- if(!point.negative || !any( d < 0 )) nullGrob() else
  pointsGrob(x=unit(rep(0.5, length(pos)), "npc"), y = y + unit(pos, "mm") ,
          pch=21, gp=gpar(col="white", fill="black"),size=unit(size*gap, "mm"), vp=clipvp)
    
  gTree(children=gList(lg,  lticks, rticks, ltg, bg, cg),
        width = width + margin + max(stringWidth(legend.vals)), ... , cl="colorbar")
}

##' @export
grid.colorbar <- function(...){
  g <- colorbarGrob(...)
  grid.draw(g)
  invisible(g)
}

##' @export
widthDetails.colorbar <- function(x){
 x$width 
}


##' display a matrix
##'
##' colormap of a matrix with top and left labels
##' @title dmatrixGrob
##' @export
##' @aliases grid.dmatrix
##' @param d matrix
##' @param point.negative logical, add points to negative values
##' @param palette palette
##' @param frame.gp gpar for the frame
##' @param ... passed to gTree
##' @return gTree 
##' @author baptiste Auguie
##' @family matrix low_level
##' @examples
##' set.seed(1234)
##' n <- 30^2
##' m <- matrix(rnorm(n, 4, 3), sqrt(n))
##' pal = brewer.pal(11,"PRGn")
##' grid.newpage()
##' g <- dmatrixGrob(m)
##' grid.draw(g)
dmatrixGrob <- function(d, 
                        point.negative=FALSE, size=0.7,
                        palette = diverging_palette(d, center=FALSE),
                        frame.gp = gpar(fill="transparent", col="grey50"), use.raster=FALSE, ...){

  nc <- ncol(d)
  nr <- nrow(d)

  ## define the fill matrix for squares
  ## and color matrix for dots
  fill.matrix <- palette(d)
  dim(fill.matrix) <- dim(d)
 
  if(use.raster){
  rg <- rasterGrob(fill.matrix,
                   width=unit(1, "npc"),
                   height=unit(1, "npc"),
                   interpolate = FALSE)

   } else {
     
  rg <- imageGrob(nrow(fill.matrix), ncol(fill.matrix),
                  fill.matrix[seq(nrow(fill.matrix), 1, by=-1), ], byrow = FALSE)
}

  bg <- rectGrob( width=unit(1, "npc"),
                   height=unit(1, "npc"),
                   gp=frame.gp)


  full <- unit(1,"npc")

  ## coloured dots
  if(!point.negative){
  cg <-  nullGrob() } else {
    color.matrix <- ifelse(d >0 , "transparent", "black")
    color.matrix2 <- ifelse(d >0 , "transparent", "white")
    dim(color.matrix) <- dim(d)
    dim(color.matrix2) <- dim(d)
  ## position of the dots
  xy <- expand.grid(y = 1 - rescale(seq_len(nr), c(0.5/nr, 1 - 0.5/nr)   ),
                    x = rescale(seq_len(nc), c(0.5/nc, 1 - 0.5/nc))
                    )
  cg <- 
  pointsGrob(xy$x*full, xy$y*full,
             size=size/nc*full,
             pch=21, gp=gpar(fill=color.matrix, col=color.matrix2))
}
  
  gTree(children=gList(rg, bg, cg), nrow=nr, ncol=nc,
        cl="dmatrix", ...)

}

##' @export
grid.dmatrix <- function(...){
  g <- dmatrixGrob(...)
  grid.draw(g)
  invisible(g)
}

## TODO units are not correctly modified with <-

##' column axis
##'
##' draws a minimal axis with tick marks and labels above designated matrix columns
##' @title colaxisGrob
##' @export
##' @param x x
##' @param y y
##' @param tick.length tick length
##' @param gp.ticks gp.ticks
##' @param show.lab logical, display labels
##' @param margin margin between ticks and labels
##' @param labels labels
##' @param gp.labels gp.labels
##' @param ... passed to gTree
##' @return gTree
##' @family low_level
##' @author Baptiste Auguie
colaxisGrob <- function(x, y, tick.length=unit(0.5,"mm"),
                        gp.ticks=gpar(), show.lab = TRUE,
                        margin=unit(1, "mm"),
                        labels = seq_along(x),
                        gp.labels = gpar(col="grey50", fontsize=8, fontface="italic"),
                        ...){

  ticks.x <- segmentsGrob(x0=x, x1=x,
                          y0 = y,
                          y1 = y +  tick.length,
                          gp=gp.ticks)
  
  
  labels <- if(!show.lab) nullGrob() else
  textGrob(labels,
           x = x, y = y + margin,
           just=c("center", "bottom"), gp=gp.labels)
  
  gTree(children=gList(ticks.x, labels), margin=margin, ..., cl="colaxis")
  
}

##' @export
grid.colaxis <- function(...){
  grid.draw(colaxisGrob(...))
}

##' @export
heightDetails.colaxis <- function(x){
  ## unit(1,"lines")
   do.call(sum, lapply(x$children, grobHeight)) + x$margin
}

##' row axis
##'
##' draws a minimal axis with tick marks and labels on the left side of designated matrix rows
##' @title rowaxisGrob
##' @export
##' @param x x
##' @param y y
##' @param tick.length tick length
##' @param gp.ticks gp.ticks
##' @param show.lab logical, display labels
##' @param margin margin between ticks and labels
##' @param labels labels
##' @param gp.labels gp.labels
##' @param ... passed to gTree
##' @return gTree
##' @family low_level
##' @author Baptiste Auguie
rowaxisGrob <- function(x, y, tick.length=unit(0.5,"mm"),
                        gp.ticks=gpar(), show.lab = TRUE,
                        margin=unit(1, "mm"),
                        labels = seq_along(y),
                        gp.labels = gpar(col="grey50", fontsize=8, fontface="italic"),
                        ...){

  ticks.y <- segmentsGrob(x0=x - tick.length, x1=x,
                          y0 = y,
                          y1 = y,
                          gp=gp.ticks)

  labels <- if(!show.lab) nullGrob() else
  textGrob(labels,
           x = x - margin, y = y,
           just=c("right", "center"), gp=gp.labels)
  
  gTree(children=gList(ticks.y, labels),  margin=margin, ..., cl="rowaxis")
  
}

##' @export
widthDetails.rowaxis <- function(x){
  ## unit(1,"lines")
 do.call(sum, lapply(x$children, grobWidth)) + x$margin
}

##' @export
grid.rowaxis <- function(...){
  grid.draw(rowaxisGrob(...))
}

##' layout a single matrix with optional axis annotations
##'
##' layout a single matrix with optional axis annotations
##' @export
##' @title single_panel
##' @param d matrix
##' @param width matrix width
##' @param palette palette
##' @param margin margin
##' @param at.x locations of ticks marks
##' @param at.y locations of ticks marks
##' @param xaxis logical
##' @param yaxis logical
##' @param labels.x labels of ticks marks
##' @param labels.y labels of ticks marks
##' @param show.xlab see colaxisGrob
##' @param show.ylab see rowaxisGrob
##' @param title.x x title
##' @param title.y y title
##' @param point.negative logical
##' @param ... passed to gTree
##' @return gTree, with border.width and border.height
##' @family matrix user_level
##' @author Baptiste Auguie
single_panel <- function(d, width=unit(5, "cm") ,
                         palette = diverging_palette(d, center=FALSE,
                           colors = RColorBrewer::brewer.pal(7,"RdBu")),
                         margin = unit(1.5, "mm"),
                         at.x = unique(round(grid.pretty(c(1,ncol(d))))),
                         at.y = unique(round(grid.pretty(c(1,nrow(d))))),
                         xaxis=TRUE, yaxis=TRUE,
                         labels.x = at.x, 
                         labels.y = at.y,
                         show.xlab=TRUE, show.ylab=TRUE,
                         title.x = expression(italic(k)),
                         title.y = expression(italic(n)),
                         point.negative=TRUE, ...){

  nr <- nrow(d)
  nc <- ncol(d)
  height <- nr/nc*width
  
  xy <- list(y = rescale(seq_len(nr), c(0.5/nr, 1 - 0.5/nr)   ),
                    x = rescale(seq_len(nc), c(0.5/nc, 1 - 0.5/nc))  )

  ## grobs
  gm <- dmatrixGrob(d, point.negative=point.negative,
                    palette = palette,
                    frame.gp = gpar(fill="transparent", col="grey50", lwd=0.5))
    
  yaxis <- if(!yaxis) nullGrob() else
  rowaxisGrob(unit(1, "npc"), rev(xy$y)[at.y]*unit(1, "npc"),
              labels=labels.y, show.lab = show.ylab, margin=margin)
  
  xaxis <- if(!xaxis) nullGrob() else
  colaxisGrob(xy$x[at.x]*unit(1, "npc"), unit(0, "npc"),
                       labels=labels.x, show.lab = show.xlab, margin=margin)
  
  xtitle <- if(is.null(title.x)) nullGrob() else textGrob(title.x)
  ytitle <- if(is.null(title.y)) nullGrob() else textGrob(title.y)

  
  xaxis.height <- grobHeight(xaxis)
  yaxis.width <- grobWidth(yaxis)
  xtitle.height <- grobHeight(xtitle)
  ytitle.width <- grobWidth(ytitle)


  ## viewports
  vp_panel <- viewport(x=unit(0, "npc"),
                       y=unit(1, "npc"),
                       width=width, height = height, just=c("left", "top"))
  
  vp_cols <- viewport(x=unit(0, "npc") + 0.5*width,
                      y=unit(1, "npc"),
                      width=width, height = xaxis.height,
                      just=c("center", "bottom"))
  vp_rows <- viewport(x=unit(0, "npc") ,
                      y=unit(1, "npc") -  0.5*height,
                      width=yaxis.width, height = height ,
                      just=c("right", "center"))

  vp_xtitle <- viewport(x=unit(0, "npc") + 0.5*width,
                      y=unit(1, "npc") + xaxis.height + margin,
                      width=width, height = xtitle.height,
                      just=c("center", "bottom"))
  
  vp_ytitle <- viewport(x=unit(0, "npc") - yaxis.width - margin,
                      y=unit(1, "npc") - 0.5*height,
                      width=ytitle.width, height = height,
                      just=c("right", "center"))
  
  gm <- editGrob(gm, vp=vp_panel)
  xaxis <- editGrob(xaxis, vp=vp_cols)
  yaxis <- editGrob(yaxis, vp=vp_rows)
  xtitle <- editGrob(xtitle, vp=vp_xtitle)
  ytitle <- editGrob(ytitle, vp=vp_ytitle)
  
  gTree(children=gList(gm, xaxis, yaxis, xtitle, ytitle),
        border.x = yaxis.width + ytitle.width + margin,
        border.y = xaxis.height + xtitle.height + margin)

}


##' grid graphics functions to display matrices
##'
##' @name gridplot-package
##' @aliases gridplot
##' @docType package
##' @title gridplot
##' @author baptiste Auguie \email{baptiste.auguie@@gmail.com}
##' @keywords packagelibrary
##' @family matrix user_level
##' @examples
##' library(gridplot) ; set.seed(123) ; m = matrix(rnorm(21), 3) ; grid.newpage() ; row_layout(m, m, legend.params=list(digits=2, pretty.breaks = c(-1, 0, 1, -2), show.extrema=FALSE))
function()
  NULL

