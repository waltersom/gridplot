library(rgl)
setwd("/Users/auguieba/Dropbox/Public/gridplot/inst/")
euler <- function(alpha=0, # rotation around z
                  beta=0, # rotation around x'
                  gamma=0 # rotation around z''
                  ){

  Ra <- matrix(c(cos(alpha), sin(alpha), 0, -sin(alpha), cos(alpha), 0, 0, 0, 1), ncol=3, byrow=T)
  Rb <- matrix(c(1, 0, 0, 0, cos(beta), sin(beta), 0, -sin(beta), cos(beta)), ncol=3, byrow=T)
  Rc <- matrix(c(cos(gamma), sin(gamma), 0, -sin(gamma), cos(gamma), 0, 0, 0, 1), ncol=3, byrow=T)

  return(Rc%*%Rb%*%Ra)
  
}

rgl.ellipsoid <- function (x=0,y=0,z=0, a = 1,b=1,c=1, phi=0,theta=0,psi=0,
                       subdivide = 3, smooth = TRUE, ...) 
{
  
    sphere <- rgl::subdivision3d(cube3d(...), subdivide)
    class(sphere) <- c("mesh3d","shape3d")

    norm <- sqrt(sphere$vb[1, ]^2 + sphere$vb[2, ]^2 + sphere$vb[3, 
        ]^2)
    for (i in 1:3) sphere$vb[i, ] <- sphere$vb[i, ]/norm
    sphere$vb[4, ] <- 1
    sphere$normals <- sphere$vb
    result <- rgl::scale3d(sphere, a,b,c)
    rotM <- euler(phi,theta,psi)
    result <- rgl::rotate3d(result,matrix=rotM)
    result <- rgl::translate3d(result, x,y,z)
    invisible(result)
}

ellipsoid <- rgl.ellipsoid(a=2)
sphere <- rgl.ellipsoid()
cylinder <- cylinder3d(rbind(c(0,0,0), # start
			 c(0,0,1)), # end
                       radius = 0.4,
                       e1=cbind(0, 0, 1),
                       e2=cbind(1, 0, 0),
                       sides=100)
open3d()
shade3d(ellipsoid, col="grey", specular="black")
rgl.snapshot( "ellipsoid-rgl.png", fmt="png", top=TRUE )
open3d()
shade3d(sphere, col="grey", specular="black")
rgl.snapshot( "sphere-rgl.png", fmt="png", top=TRUE )
open3d()
shade3d(cylinder, col="grey", specular="black")
rgl.snapshot( "cylinder-rgl.png", fmt="png", top=TRUE )
