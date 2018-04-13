#' compare_check.set
#'
#' This function applyes the Digital Alignment Tool (DTA) on a disarticulated model using a reference landmark configuration
#' @param RM_set_1 matrix: 3D landmark set of the first module acquired on the reference model
#' @param RM_set_2 matrix: 3D landmark set of the second module acquired on the reference model
#' @param DM_set_1 matrix: 3D landmark set of the first module acquired on the disarticulated model
#' @param DM_set_2 matrix: 3D landmark set of the second module acquired on the disarticulated model
#' @param DM_mesh_1 mesh3d: mesh of the disarticulated model (first module)  
#' @param DM_mesh_2 mesh3d: mesh of the disarticulated model (second module)  
#' @return SF1 numeric: scale factor used to scale the reference set (first module)
#' @return SF2 numeric: scale factor used to scale the reference set (second module)
#' @return RM_set_1_sc matrix: scaled 3D reference set (first module)
#' @return RM_set_2_sc matrix: scaled 3D reference set (second module)
#' @return AM_model list: output of the Morpho::rotmesh.onto function 
#' @return dist_from_mesh numeric: mesh distance between the aligned model and the scaled reference set
#' @return eucl_dist_1 numeric: euclidean distance between the landmark configuration of the disarticulated and reference model (first module)
#' @return eucl_dist_2 numeric: euclidean distance between the landmark configuration of the disarticulated and reference model (second module)
#' @return procr_dist numeric: procrustes distance between the landmark configuration of the aligned and reference model 
#' @return procr_dist_1 numeric: procrustes distance between the landmark configuration of the disarticulated and reference model (first module)
#' @return procr_dist_2 numeric: procrustes distance between the landmark configuration of the disarticulated and reference model (second module)
#' @return eucl_dist numeric: euclidean distance between the landmark configuration of the aligned and reference model 
#' @return single_l_1 numeric: euclidean distance between the landmark configuration of the disarticulated and reference model (first module)
#' @return single_l_2 numeric: euclidean distance between the landmark configuration of the disarticulated and reference model (second module)
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @export

compare_check.set<-function(RM_set_1,RM_set_2,DM_set_1,DM_set_2,DM_mesh_1,DM_mesh_2){

  dim1<-dim(RM_set_1)[1]
  dim2<-dim(RM_set_2)[1]
  SF1<-cSize(DM_set_1)/cSize(RM_set_1)
  SF2<-cSize(DM_set_2)/cSize(RM_set_2)
  scale_factor<-(SF1+SF2)/2  
  
  RM_set_1_sc<-RM_set_1*scale_factor
  RM_set_2_sc<-RM_set_2*scale_factor
  
  AM_mesh_1<-rotmesh.onto(DM_mesh_1, DM_set_1,RM_set_1_sc, adnormals = FALSE, scale = FALSE,
                          reflection = FALSE)
  AM_mesh_2<-rotmesh.onto(DM_mesh_2, DM_set_2,RM_set_2_sc, adnormals = FALSE, scale = FALSE,
                          reflection = FALSE)
  AM_mesh<-mergeMeshes(AM_mesh_1$mesh,AM_mesh_2$mesh)
  
  AM_rot<-rotmesh.onto(AM_mesh,rbind(AM_mesh_1$yrot,AM_mesh_2$yrot),
                       rbind(RM_set_1_sc,RM_set_2_sc), adnormals = FALSE, scale = FALSE,
                       reflection = FALSE)
  
  check_dist<-abs(sum(projRead(rbind(RM_set_1_sc,RM_set_2_sc),AM_rot$mesh)$quality))
  procr_dist<-vegan::procrustes(AM_rot$yrot,rbind(RM_set_1_sc,RM_set_2_sc))$ss
  eucl_dist<-sum(sqrt(rowSums((AM_rot$yrot-rbind(RM_set_1_sc,RM_set_2_sc))^2)))
  check_1<-sum(sqrt(rowSums((AM_rot$yrot[1:dim1,]-RM_set_1_sc)^2)))
  check_2<-sum(sqrt(rowSums((AM_rot$yrot[(dim1+1):(dim1+dim2),]-RM_set_2_sc)^2)))
  procr_dist_1<-vegan::procrustes(AM_rot$yrot[1:dim1,],RM_set_1_sc)$ss
  procr_dist_2<-vegan::procrustes(AM_rot$yrot[(dim1+1):(dim1+dim2),],RM_set_2_sc)$ss
  
  single_l_1<-sqrt(rowSums((AM_rot$yrot[1:dim1,]-RM_set_1_sc)^2))
  single_l_2<-sqrt(rowSums((AM_rot$yrot[(dim1+1):(dim1+dim2),]-RM_set_2_sc)^2))
  
  out<-list("SF1"=SF1,"SF2"=SF2,"RM_set_1_sc"=RM_set_1_sc,"RM_set_2_sc"=RM_set_2_sc,
            "AM_model"=AM_rot,"dist_from_mesh"=check_dist,"eucl_dist_1"=check_1,"eucl_dist_2"=check_2,
            "procr_dist"=procr_dist,"procr_dist_1"=procr_dist_1,"procr_dist_2"=procr_dist_2,"eucl_dist"=eucl_dist,"single_l_1"=single_l_1,"single_l_2"=single_l_2)
  return(out)
}
