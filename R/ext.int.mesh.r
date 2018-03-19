#' ext.int.mesh
#'
#' This function finds the vertices visible from a set of points of view 
#' @param mesh object of class mesh3d 
#' @param views numeric: number of points of view 
#' @param dist.sphere numeric: scale factor. This parameter the distance betweem the barycenter of the mesh and the radius of the sphere used to define set of points of view
#' @param param1 numeric: first parameter for spherical flipping (usually ranged from 0.5 to 5, try!)
#' @param param2 numeric second paramter for spherical flipping (don't change it!)
#' @param default logical: if TRUE the points of views are defined automatically, if FALSE define the matrix_pov 
#' @param import_pov logical: if FALSE an interactive 3D plot for the definition of the points of view is returned
#' @param matrix_pov matrix: external set of points of view
#' @param expand numeric: scale factor for the grid for the interactive 3D plot
#' @param scale.factor numeric: scale factor for 
#' @param method character: select "a" or "c" 
#' @param start.points numeric: number of POVs available
#' @param num.cores numeric: number of cores
#' @return position numeric: a vector with vertex number nearest the landmark set
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @export
ext.int.mesh<-function(mesh,views=20,dist.sphere=3,param1=2.5,param2=10,default=TRUE,import_pov,matrix_pov,expand=1,scale.factor,
                      method="ast3d",start.points=250,num.cores=NULL)
{
  
  multiResultClass <- function(result1=NULL,result2=NULL)
  {
    me <- list(
      result1 <- result1,
      result2 <- result2
    )
    class(me) <- append(class(me),"multiResultClass")
    return(me)
  }
  
  if(default==TRUE & is.null(import_pov)==TRUE){ 
    barycenter<-bary.mesh(mesh) 
    sphere<-vcgSphere(2)  
    sphere$vb[1,]=sphere$vb[1,]+(barycenter[1])
    sphere$vb[2,]=sphere$vb[2,]+(barycenter[2])
    sphere$vb[3,]=sphere$vb[3,]+(barycenter[3])
    bbox<-meshcube(mesh)
    pos<-aro.clo.points(t(mesh$vb)[,-4],bbox)$position
    max_dist<-as.matrix(dist(t(mesh$vb)[pos,-4],method = "euclidean"))
    max_dist_pos<-which(max_dist==max(max_dist),arr.ind=TRUE)[1,]
    sphere<-scalemesh(sphere,max_dist[max_dist_pos[1],max_dist_pos[2]]*dist.sphere,center = "mean")
    bounding<-matrix(kmeans(t(sphere$vb)[,-4],views)$centers,ncol=3,byrow = FALSE)
  }
  if(default==FALSE & is.null(import_pov)==TRUE ){   
    grid3D<-grid_pov(mesh=mesh,expand=expand)
    selection<-pov_selecter(mesh,grid3D)
    bounding<-matrix(NA,ncol=3)
    for(i in 1:dim(selection)[1]){
      sphere<-vcgSphere(2)  
      sphere_cen<-trasf.mesh(sphere,selection[i,])
      sphere_cen_sca<-scalemesh(sphere_cen,size=scale.factor,center="mean")
      bounding_temp<-matrix(kmeans(t(sphere_cen_sca$vb)[,-4],views,iter.max = 1000)$centers,ncol=3,byrow = FALSE)
      bounding<-rbind(bounding,bounding_temp)
    }
    bounding<-bounding[-1,]
  }
  if(is.null(import_pov)==FALSE){
    bounding<-matrix_pov
  }  
  
  if (is.null(num.cores)==TRUE){
    visible_ver<-list()
    for(m in 1:dim(bounding)[1]){
      flip<-spherical.flipping(C=bounding[m,],mesh,param1=param1,param2=param2)
      ver.mesh<-t(mesh$vb)[,-4]
      C<-bounding[m,]
      cloud<-rbind(C,flip,ver.mesh)
      ptm <- proc.time()
      tri<-t(convhulln(cloud,options=""))
      subset<-unique(as.vector(tri))
      subset<-subset[which(subset>1 & subset<=(dim(ver.mesh)[1]+1))]
      subset<-subset-1
      visible_ver[[m]]<-subset  
    }}
  
  if (is.null(num.cores)==FALSE){
    registerDoParallel(cores = num.cores)
    
    visible_ver<-foreach(m=1:dim(bounding)[1],
                        .multicombine=TRUE,.packages=c("Arothron","geometry")) %dopar%
    {
      result <- multiResultClass()
            flip<-spherical.flipping(C=bounding[m,],mesh,param1=param1,param2=param2)
      ver.mesh<-t(mesh$vb)[,-4]
      C<-bounding[m,]
      cloud<-rbind(C,flip,ver.mesh)
      tri<-t(convhulln(cloud,options=""))
      subset<-unique(as.vector(tri))
      subset<-subset[which(subset>1 & subset<=(dim(ver.mesh)[1]+1))]
      subset<-subset-1
      
      }

  } 
  return(visible_ver)
}
