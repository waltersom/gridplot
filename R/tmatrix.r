
##' shadow text
##'
##' adds a blurred white version of a label below the text
##' @title stextGrob
##' @param label see textGrob
##' @param r blur radius
##' @param x see textGrob
##' @param y see textGrob
##' @param just see textGrob
##' @param hjust see textGrob
##' @param vjust see textGrob
##' @param rot see textGrob
##' @param check.overlap see textGrob
##' @param default.units see textGrob
##' @param name see textGrob
##' @param gp see textGrob
##' @param vp see textGrob
##' @return gTree
##' @author Baptiste Auguie
##' @export
##' @examples
##' grid.rect(gp=gpar(fill="grey"))
##' stextGrob("test")
stextGrob <- function (label, r=0.1, x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
                       just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE, 
                       default.units = "npc", name = NULL, gp = gpar(), vp = NULL){

  let <- textGrob("a")
  
  tg <- textGrob(label=label, x=x, y=y, gp=gpar(col="black"),
                 just = just, hjust = hjust, vjust = vjust, rot = rot,
                 check.overlap = check.overlap, 
                 default.units = default.units)
  
  tgl <- c(lapply(seq(0, 2*pi, length=18), function(theta){

    textGrob(label=label,x=x+cos(theta)*r*grobWidth(let),
             y=y+sin(theta)*r*grobHeight(let), gp=gpar(col="white"),
             just = just, hjust = hjust, vjust = vjust, rot = rot,
             check.overlap = check.overlap, 
             default.units = default.units)
    
    }), list(tg))
  

  g <- gTree(children=do.call(gList, tgl), vp=vp, name=name, gp=gp)

}

##' @export
grid.stext <- function(...){
  g <- stextGrob(...)
  grid.draw(g)
  invisible(g)
}


##' layout a T-matrix 
##'
##' 2x2 blocks with legend
##' @title tmatrix
##' @param t11 t11
##' @param t12 t12
##' @param t21 t21
##' @param t22 t22
##' @param legend logical, add legend
##' @param width block width
##' @param palette see colorbarGrob
##' @param colors see colorbarGrob
##' @param center see colorbarGrob
##' @param legend.params passed to colorbarGrob
##' @param margin axis margin
##' @param spacing spacing between blocks
##' @param at.x see single_panel
##' @param at.y see single_panel
##' @param xaxis see single_panel
##' @param yaxis see single_panel
##' @param labels.x see single_panel
##' @param labels.y see single_panel
##' @param title.x see single_panel
##' @param title.y see single_panel
##' @param gp.frame frame gpar()
##' @param point.negative see single_panel
##' @param debug logical, show layout
##' @param show.names logical, add block names
##' @param ... passed to gTree
##' @family matrix user_level
##' @return gTree
##' @export
##' @author Baptiste Auguie
##' @examples
##' grid.newpage()
##' set.seed(1234)
##' N1 <- 13
##' N2 <- 20
##' N3 <- 15
##' N4 <- 12
##' foo <- identity # abs
##' d1 <- foo(matrix(rnorm(N1*N2), ncol=N1))
##' d2 <- foo(matrix(rnorm(N2*N3), ncol=N3))
##' d3 <- foo(matrix(rnorm(N1*N4), ncol=N1))
##' d4 <- foo(matrix(rnorm(N3*N4), ncol=N3))
##' grid.tmatrix(d1, d2, d3, d4, spacing=unit(1, "mm"), width=unit(5, "cm"))
tmatrixGrob <- function(t11, t12, t21, t22,
                        legend=TRUE,
                        width=unit(2, "cm"),
                        palette = diverging_palette(do.call(c, list(t11, t12, t21, t22)),
                          colors = RColorBrewer::brewer.pal(7,"RdBu"),
                          center=TRUE),
                        legend.params=list(),
                        margin = unit(1, "mm"),
                        spacing = unit(3, "mm"),
                        at.x=NULL,
                        at.y=NULL,
                        xaxis=TRUE, yaxis=TRUE,
                        labels.x=NULL, 
                        labels.y=NULL,
                        title.x = expression(italic(k)),
                        title.y = expression(italic(n)),
                        gp.frame=gpar(fill="transparent"),
                        gp.names= gpar(cex=5),
                        point.negative=TRUE,
                        debug=FALSE, show.names=TRUE, ...){
  
  nrtop <- nrow(t11)
  nrbot <- nrow(t22)
  ncleft <- ncol(t11)
  ncright <- ncol(t22)
  widthleft <- width
  widthright <- ncright/ncleft*width
  heighttop <-  nrtop/ncleft*width
  heightbot <- nrbot/ncright*widthright
  
  pixel <- convertUnit(1/mean(ncleft, ncright) * width, "mm", valueOnly=TRUE)
  
  if(is.null(at.x))  at.x <- list(unique(round(grid.pretty(c(1,ncleft)))),
                                  unique(round(grid.pretty(c(1,ncright)))))
  
  if(is.null(at.y))  at.y <- list(unique(round(grid.pretty(c(1,nrtop)))),
                                  unique(round(grid.pretty(c(1,nrbot)))))
  if(is.null(labels.x))  labels.x <- at.x
  if(is.null(labels.y))  labels.y <- at.y

   
  combined <- do.call(c, list(t11, t12, t21, t22))
  range <- range(combined)
  
  pal <- palette #(range, colors = colors, center=center)
  
  g11 <- single_panel(t11, point.negative=point.negative,
                      palette = pal, at.y = at.y[[1]], at.x=at.x[[1]], width=widthleft,
                      labels.x=labels.x[[1]], labels.y=labels.y[[1]], margin=margin,
                      xaxis=xaxis, yaxis=yaxis, title.y=title.y, title.x=title.x,
                      frame.gp = gpar(fill="transparent", col="grey50", lwd=0.5))
               
  g12 <- single_panel(t12, point.negative=point.negative,
                      palette = pal, at.y = at.y[[1]], at.x=at.x[[2]], width=widthright,
                      labels.x=labels.x[[2]], labels.y=NULL, margin=margin,
                      xaxis=xaxis, yaxis=FALSE, title.y=NULL, title.x=title.x,
                      frame.gp = gpar(fill="transparent", col="grey50", lwd=0.5))
  g21 <- single_panel(t21, point.negative=point.negative,
                      palette = pal, at.y = at.y[[2]], at.x=at.x[[1]],
                      width=widthleft,
                      labels.x=NULL, labels.y=labels.y[[2]], margin=margin,
                      xaxis=FALSE, yaxis=yaxis, title.y=title.y, title.x=NULL,
                      frame.gp = gpar(fill="transparent", col="grey50", lwd=0.5))
  g22 <- single_panel(t22, point.negative=point.negative,
                      palette = pal, at.y = at.y[[2]], at.x=at.x[[2]], width=widthright,
                      labels.x=NULL, labels.y=NULL, margin=margin,
                      xaxis=FALSE, yaxis=FALSE, title.y=NULL, title.x=NULL,
                      frame.gp = gpar(fill="transparent", col="grey50", lwd=0.5))
  
  legend <- if(!legend) nullGrob() else
  do.call(colorbarGrob,
          modifyList(list(d = range, palette=pal,
                          x=unit(0, "npc"), point.negative=point.negative,
                          y = unit(0, "npc"), gap=pixel,
                          height = unit(1, "npc")), legend.params))

  ## define the widths for the layout
  ## trick: embed units in a list to use correct `[<-` replacement functions
  widths <- unit.c(convertUnit(g11$border.x, "cm"),
                   widthleft, spacing, widthright, 2*spacing,
                   convertUnit(grobWidth(legend), "cm"))
  
  annotation.height <- convertUnit(g11$border.y, "cm", "y") 

  ## make a layout
  gl <- grid.layout(ncol=6, nrow=4,
                    width= widths,
                    height = unit.c(annotation.height, heighttop, spacing, heightbot))

  if(debug) grid.show.layout(gl)
  
  matvp <- function(ii, jj, just=c("right", "bottom"))
    viewport(x=1, layout=gl, just=just,
             layout.pos.col = ii, layout.pos.row = jj)

  g11 <- editGrob(g11, vp=matvp(2, 2))
  g12 <- editGrob(g12, vp=matvp(4, 2))
  g21 <- editGrob(g21, vp=matvp(2, 4))
  ## g21 <- editGrob(g21, vp=matvp(3, 4))
  g22 <- editGrob(g22, vp=matvp(4, 4))
  legend <- editGrob(legend, vp=matvp(6, 2:4))
  lg <- if(!show.names) nullGrob() else
  gTree(children=gList(
          stextGrob( expression(bold(T)^11) , vp=matvp(2, 2)),
          stextGrob( expression(bold(T)^12) , vp=matvp(4, 2)),
          stextGrob( expression(bold(T)^21) , vp=matvp(2, 4)),
          stextGrob( expression(bold(T)^22) , vp=matvp(4, 4))  ),
        gp=gp.names)
  
  frame <- rectGrob( vp = matvp(c(2, 4), c(2, 4)), gp=gp.frame)
## print("test")
  
   gTree(children=gList(g11, g12, g21, g22, frame, legend, lg), vp=viewport(layout=gl), ...)
  
}


##' @export
grid.tmatrix <- function(...){
  g <- tmatrixGrob(...)
  grid.draw(g)
  invisible(g)
}


