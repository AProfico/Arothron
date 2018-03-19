#' out.inn.mesh
#'
#' This function separates a 3D mesh subjected to the ext.int.mesh into two 3D models: the visible mesh and the not visible one
#' @param scans an ext.int.mesh 
#' @param mesh matrix mesh vertex (the same of the ext.int.mesh object)
#' @param plot logical: if TRUE the wireframe of the mesh with the visible vertices is plotted
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @examples
#' \dontrun{
#' #CA-LSE tool on Neanderthal tooth
#' #load a mesh
#' data(krd1_tooth)
#' ca_lse_krd1<-ext.int.mesh(mesh= krd1_tooth, views=50, param1=3, default=TRUE, 
#' import_pov = NULL,expand=1, scale.factor=1,num.cores = 4)
#' vis_inv_krd1<-out.inn.mesh(ca_lse_krd1, krd1_tooth, plot=TRUE)
#' inv_mesh<-vcgIsolated(vis_inv_krd1$invisible)
#' open3d()
#' shade3d(inv_mesh,col=2)
#' open3d()
#' shade3d(vis_inv_krd1$visible, col=3)
#' #CA-LSE tool on human malleus
#' #load a mesh
#' data(malleus_bone)
#' ca_lse_malleus<-ext.int.mesh(mesh= malleus_bone, views=50, param1=3, 
#' default=TRUE, import_pov = NULL, expand=1, scale.factor=1)
#' vis_inv_malleus<-out.inn.mesh(ca_lse_malleus, malleus_bone, plot=TRUE)
#' inv_mesh<- vis_inv_malleus$invisible
#' inv_mesh<-ca_lse_malleus$invisible
#' 
#' #AST-3D tool
#' #load a mesh
#' data(human_skull)
#' data(endo_set)
#' ast3d_endocast<-ext.int.mesh(mesh=human_skull, views=50, param1=1.0, default=FALSE, 
#' import_pov = TRUE,expand=1, matrix_pov =endo_set, scale.factor=1,num.cores = 4)
#' vis_inv_endo<-out.inn.mesh(ast3d_endocast,human_skull,plot=TRUE)
#' vis_mesh<-vcgIsolated(vis_inv_endo$visible)
#' open3d()
#' shade3d(vis_mesh,col=3)
#' open3d()
#' shade3d(vis_inv_endo$invisible, col=2)
#' }
#' @export
out.inn.mesh<-function(scans,mesh,plot=TRUE){
data<-scans

indices<-c()
for(j in 1:length(data)){
indices<-c(indices,data[[j]])
}
  
ext.mesh<-rmVertex(mesh, unique(indices),keep = TRUE)
if(plot==TRUE){
open3d()
wire3d(ext.mesh,col="grey")}
inn.mesh<-rmVertex(mesh, unique(indices),keep = FALSE)
if(plot==TRUE){
shade3d(inn.mesh,col="red")  
}  
  
meshes<-list("visible"=ext.mesh,"invisible"=inn.mesh)
}
