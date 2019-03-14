#' endomaker_dir
#'
#' Build library of endocasts from skull 3D meshes
#' @param dir_path character: path of the folder where the skull meshes are stored
#' @param param1_endo numeric vector: parameter for spherical flipping 
#' @param npovs numeric: number of Points of View used in the endocast construction
#' @param volume logical: if TRUE the volume of the endocast (ECV) is estimated 
#' @param decmesh numeric: decmesh
#' @param alpha_ext numeric: alpha shape for construction external cranial mesh
#' @param alpha_vol numeric: alpha shape for volume calculation 
#' @param ncells numeric: approximative number of cell for 3D grid construction
#' @param nVoxels numeric: number of voxels for estimation endocranial volume
#' @param plotall logical: if TRUE the endocasts are plotted
#' @param colmesh character: color of the mesh to be plotted
#' @param npovs_calse numeric: number of Points of View for construction of skull shell
#' @param save logical: if TRUE the mesh of the endocast is saved
#' @param outpath character: path where save the endocast 
#' @param param1_calse numeric: parameter for calse (construction shell)
#' @param param1_ast numeric: parameter for ast3d (construction row endocast)
#' @param decendo numeric: desired number of triangles (row endocast)
#' @param scalendo numeric: scale factor row endocast (for definition of POVs)
#' @param alpha_end numeric: alpha shape value for concave hull (row endocast) 
#' @param mpovdist numeric vector: mean value between POVs and mesh 
#' @param clus numeric: percentage of numbers of cores to be used in parallel elaboration
#' @return endocasts mesh3d: list of meshes of the extracted endocasts
#' @return volumes numeric: volumes of the endocasts expressed in cc
#' @author Antonio Profico, Alessio Veneziano, Costantino Buzi, Marina Melchionna, Pasquale Raia
#' @export

endomaker_dir<-function(dir_path,param1_endo=1.5,npovs=50,
                        volume=TRUE,alpha_vol=50,nVoxels=100000,
                        decmesh=20000,alpha_ext=30,ncells=50000,npovs_calse=50,
                        param1_calse=3,param1_ast=1.3,decendo=20000,
                        scalendo=0.5,alpha_end=100,mpovdist=10,plotall=FALSE,colmesh="orange",save=FALSE,outpath=tempdir(),clus=0.5)
  {
  timings<-NULL
  volumes=NULL
  endos<-list()
  crania<-list.files(dir_path) 
  for(i in 1:length(crania)){
    path_in<-paste(dir_path,crania[i],sep="/")
    
    if((length(param1_endo)!=length(crania))==TRUE){
      param1_endo<-rep(param1_endo[1],length(crania))  
    }
    if((length(mpovdist)!=length(crania))==TRUE){
      mpovdist<-rep(mpovdist[1],length(crania))  
    }
    if((length(param1_calse)!=length(crania))==TRUE){
      param1_calse<-rep(param1_calse[1],length(crania))    
    }
    if((length(param1_ast)!=length(crania))==TRUE){
      param1_ast<-rep(param1_ast[1],length(crania))  
    }
    
    endo_i<-endomaker(path_in=path_in,param1_endo=param1_endo[i],mpovdist=mpovdist[i],param1_calse=param1_calse[i],param1_ast=param1_ast[i],
                      clus=clus, volume=volume,plot=FALSE,save=FALSE)
    timings[i]<-endo_i$timing
    endos[[i]]<-endo_i$endocast
    volumes[[i]]<-endo_i$volume
    
  }
  
  if(plotall==TRUE){
    for(j in 1:length(endos)){
      open3d()
      shade3d(endos[[j]],col=colmesh)
    }}
  
  if(save==TRUE){
    dir.create(outpath)
    for(j in 1:length(endos)){
      pathout<-paste(outpath,"/_endocast_",j,sep="")
      mesh2ply(endos[[j]],pathout)
    }}
  
  out<-list("endocasts"=endos,"volumes"=volumes)
}

