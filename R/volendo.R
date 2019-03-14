#' volendo
#'
#' Calculate the volume of a mesh by using a voxel-based method
#' @param mesh object of class mesh3d
#' @param alpha_vol numeric: alpha shape for construction external concave hull
#' @param ncells numeric: approximative number of cell for 3D grid construction
#' @return vol numeric: volume of the mesh expressed in cc
#' @author Antonio Profico, Alessio Veneziano, Costantino Buzi, Marina Melchionna, Pasquale Raia
#' @export

volendo<-function(mesh,alpha_vol=100,ncells=100000){
  
#concave hull 
conv_endo<-ashape3d(vert2points(mesh),alpha=alpha_vol,pert=TRUE,eps =1e-06 )
conv_sur<-as.mesh3d(conv_endo)

#grid construction
bbox<-meshcube(mesh)
bbox_w<-sqrt(sum((bbox[1,]-bbox[3,])^2))
bbox_h<-sqrt(sum((bbox[1,]-bbox[2,])^2))
bbox_l<-sqrt(sum((bbox[1,]-bbox[5,])^2))
bbox_c<-prod(bbox_w,bbox_h,bbox_l)
voxelsize<-(bbox_c/ncells)^(1/3)

xbox<-seq(min(bbox[,1]),max(bbox[,1]),by=voxelsize)
ybox<-seq(min(bbox[,2]),max(bbox[,2]),by=voxelsize)
zbox<-seq(min(bbox[,3]),max(bbox[,3]),by=voxelsize)
GRID<-as.matrix(expand.grid(xbox,ybox,zbox))

#points inside mesh
DISCR<-vcgClostKD(GRID, mesh, sign = TRUE)
voxels_inside_1<-GRID[which(DISCR$quality<=0),]

out_endo<-vcgClostKD(voxels_inside_1, conv_sur, sign = TRUE)
voxels_inside_2<-voxels_inside_1[which(out_endo$quality>=0),]

count_cells<-nrow(voxels_inside_2)
vol<-((voxelsize^3)*count_cells)/1000
return(vol)
}

