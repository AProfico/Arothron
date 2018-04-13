#' bary.mesh
#'
#' This function calculates the barycenter of a matrix or a 3D mesh 
#' @param mesh matrix mesh vertex
#' @return barycenter numeric: x,y,z coordinates of the barycenter of the mesh
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @examples
#' #load an example: mesh, and L set
#' data(SCP1.mesh)
#' sur<-SCP1.mesh
#' bary<-bary.mesh(mesh=sur)
#' @export
bary.mesh<-function(mesh){
  if("mesh3d"%in%class(mesh)){
    mesh<-t(mesh$vb)[,-4]  
  }  
barycenter<-apply(mesh,2,mean)
return(barycenter)  
}

