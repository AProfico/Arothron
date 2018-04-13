#' trasf.mesh
#'
#' This function centers a mesh on the barycenter coordinates
#' @param mesh a 3D mesh of class "mesh3d"
#' @param barycenter numeric: coordinates of the center
#' @return mesh a 3D mesh of class "mesh3d"
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @export

trasf.mesh=function(mesh,barycenter){
mesh$vb[1,]=mesh$vb[1,]+(barycenter[1])
mesh$vb[2,]=mesh$vb[2,]+(barycenter[2])
mesh$vb[3,]=mesh$vb[3,]+(barycenter[3])  
return(mesh)  
}

