#' noise.mesh
#' 
#' This function adds noise to a mesh
#' @param mesh triangular mesh stored as object of class "mesh3d"
#' @param noise sd deviation to define vertex noise
#' @param seed seed for random number generator
#' @return mesh_n a 3D model of class "mesh3d" with noise
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @examples
#' #load mesh
#' library(compositions)
#' library(rgl)
#' data("SCP1.mesh")
#' mesh<-SCP1.mesh
#' #add noise
#' noised<-noise.mesh(mesh,noise=0.05)
#' #plot original and mesh with noise added
#' open3d()
#' shade3d(mesh,col=3)
#' shade3d(noised,col=2,add=TRUE)
#' @export

noise.mesh<-function (mesh, noise = 0.025, seed = 123) 
{
    mesh_n <- NULL
    set.seed(seed)
    noise <- matrix(rnorm(3 * dim(mesh[[1]])[2], 0, sd = noise), 
        nrow = 3, ncol = dim(mesh[[1]])[2])
    mesh_n_vb <- rbind(mesh$vb[1:3, ] + noise, rep(1, dim(mesh[[1]])[2]))
    mesh_n <- list(vb = mesh_n_vb, it = mesh$it)
    class(mesh_n) <- "mesh3d"
    return(mesh_n)
}
