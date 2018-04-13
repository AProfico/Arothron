#' dta
#'
#' This function applyes the Digital Alignment Tool (DTA) on a disarticulated model using a reference sample
#' @param RM_sample 3D array: 3D landmark configurations of the reference sample 
#' @param mod_1 numeric vector: vector containing the position of which landmarks belong to the first module
#' @param mod_2 numeric vector: vector containing the position of which landmarks belong to the second module
#' @param pairs_1 matrix: a X x 2 matrix containing the indices of right and left landmarks of the first module
#' @param pairs_2 matrix: a X x 2 matrix containing the indices of right and left landmarks of the second module
#' @param DM_mesh_1 mesh3d: mesh of the disarticulated model (first module)  
#' @param DM_mesh_2 mesh3d: mesh of the disarticulated model (second module)  
#' @param DM_set_1 matrix: 3D landmark set of the first module acquired on the disarticulated model
#' @param DM_set_2 matrix: 3D landmark set of the second module acquired on the disarticulated model
#' @param method character: specify method to be used to individuate the best DTA ("euclidean" or "procrustes")
#' @return AM_mesh mesh3d: mesh of the aligned model
#' @return AM_set matrix: landmark configuration of the aligned model
#' @return AM_id character: name of the item of the reference sample resulted as best DTA
#' @return AM_SF_1 numeric: scale factor used to scale the reference set (first module)
#' @return AM_SF_2 numeric: scale factor used to scale the reference set (second module)
#' @return distance numeric: distance between the landmark configuration of the aligned and the reference model 
#' @return tot_proc numeric vector: procrustes distances between aligned and reference models (all DTAs)
#' @return tot_eucl numeric vector: euclidean distances between aligned and reference models (all DTAs)
#' @return setarray 3D array: landmark configurations of the disarticulated model aligned on each item of the reference sample 
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @examples
#' ## Load and plot the disarticulated model of the Homo sapiens case study
#' library(compositions)
#' library(rgl)
#' data(DM_base_sur)
#' data(DM_face_sur)
#' open3d()
#' wire3d(DM_base_sur,col="white")
#' wire3d(DM_face_sur,col="white")
#' ## Load the landmark configurations associated to the DM 
#' data(DM_set)
#' ## Load the reference sample 
#' data(RMs_sets)
#' ## Define the landmarks belonging to the first and second module
#' mod_1<-c(1:17) #cranial base
#' mod_2<-c(18:32) #facial complex
#' ## Define the paired landmarks for each module (optional symmetrization process)
#' pairs_1<-cbind(c(4,6,8,10,12,14,16),c(5,7,9,11,13,15,17))
#' pairs_2<-cbind(c(23,25,27,29,31),c(24,26,28,30,32))
#' ## Run DTA
#' ex.dta<-dta(RM_sample=RMs_sets, mod_1=mod_1, mod_2=mod_2, pairs_1=pairs_1, pairs_2=pairs_2,
#' DM_mesh_1=DM_base_sur,DM_mesh_2=DM_face_sur, DM_set_1= DM_set[mod_1,], DM_set_2=DM_set[mod_2,])
#' ## Print the name of the best RM 
#' ex.dta$AM_id
#' ## Save the mesh and the landmark set of the AM
#' AM_mesh<-ex.dta$AM_mesh
#' AM_set<-ex.dta$AM_set
#' ## Plot the aligned 3D model
#' library(compositions)
#' library(rgl)
#' open3d()
#' wire3d(AM_mesh,col="white")
#' plot3D(AM_set,bbox=FALSE,add=TRUE)
#' @export

dta<-function(RM_sample,mod_1,mod_2,pairs_1,pairs_2,
              DM_mesh_1,DM_mesh_2,DM_set_1,DM_set_2,
              method=c("euclidean")){
  eu_dists<-NULL
  eu_procr<-NULL
  setarray<-array(NA,dim=dim(RM_sample))
  for(i in 1:dim(RM_sample)[3]){
    RM_set_i<-RM_sample[,,i]
    
    if(is.null(pairs_1)==FALSE & is.null(pairs_2)==FALSE){
      pairedLM<-rbind(pairs_1,pairs_2)
      symm.set.i<-symmetrize(RM_set_i,pairedLM=pairedLM)}
    else{
      symm.set.i=RM_set_i
    }  
    RM_set_1<-symm.set.i[mod_1,]  
    RM_set_2<-symm.set.i[mod_2,]
    
    AM_i<-compare_check.set(RM_set_1=RM_set_1,RM_set_2=RM_set_2,
                            DM_set_1=DM_set_1,DM_set_2=DM_set_2,
                            DM_mesh_1=DM_mesh_1,DM_mesh_2=DM_mesh_2)
    
    
    setarray[,,i]=AM_i$AM_model$yrot
    eu_dists[i]<-AM_i$eucl_dist
    eu_procr[i]<-AM_i$procr_dist
    
  }
  if(method=="euclidean"){
    i=which.min(eu_dists)}
  if(method=="procrustes"){
    i=which.min(eu_procr)}
  
  RM_set_i<-RM_sample[,,i]
  pairedLM<-rbind(pairs_1,pairs_2)
  symm.set.i<-symmetrize(RM_set_i,pairedLM=pairedLM)
  
  RM_set_1<-symm.set.i[mod_1,]  
  RM_set_2<-symm.set.i[mod_2,]
  
  AM_project<-compare_check.set(RM_set_1=RM_set_1,RM_set_2=RM_set_2,
                                DM_set_1=DM_set_1,DM_set_2=DM_set_2,
                                DM_mesh_1=DM_mesh_1,DM_mesh_2=DM_mesh_2)
  AM_model<-AM_project$AM_model$mesh
  AM_set<-AM_project$AM_model$yrot
  
  if(method=="euclidean"){
    distance=eu_dists[i]}
  if(method=="procrustes"){
    distance=eu_procr[i]}
  
  
  out<-list("AM_mesh"=AM_model,"AM_set"=AM_set,
            "AM_id"=dimnames(RM_sample)[[3]][i],
            "AM_SF_1"=AM_project$SF1,"AM_SF_2"=AM_project$SF2,"distance"=distance,
            "tot_proc"=eu_procr,"tot_eucl"=eu_dists,"setarray"=setarray)
}