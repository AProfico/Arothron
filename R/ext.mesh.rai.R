#' ext.mesh.rai
#'
#' This function returns a 3D mesh with colours based on the vertices visibile from each point of view
#' @param scans an ext.int.mesh 
#' @param mesh matrix mesh vertex (the same of the ext.int.mesh object)
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @export
ext.mesh.rai<-function(scans,mesh){
data<-scans
open3d()
colors<-rainbow(length(data))
list_mesh<-list()
for(j in 1:length(data)){
surface_i<-rmVertex(mesh, data[[j]],keep = TRUE)
list_mesh[[j]]<-surface_i
wire3d(surface_i,col=colors[j],add=TRUE)
}  
}  
  
  
