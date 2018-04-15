#' twodviews
#' Calculates the PCscores of the 2Dcomp matrix.  an array in a list storing each element of the third dimension of the array (specimen) as element of the list 
#' @param twodlist a list containing the landmark configurations of each anatomical view stored as separated lists
#' @param scale logical: TRUE for shape-space, FALSE for form-space 
#' @param vector numeric vector: defines which views are to be used
#' @return PCscores PC scores
#' @return PCs Pricipal Components (eigenvector matrix)
#' @return Variance table of the explained variance by the PCs
#' @return size vector containing the Centroid Size of each configuration
#' @return mshapes a list containing the mean shape of each 2D configuration
#' @return dims number of landmarks of each 2D configuration
#' @return twodlist the list used as input
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Paolo Piras, Pasquale Raia
#' @examples
#' library(Morpho)
#' #load the 2D primate dataset
#' data("Lset2D_list")
#' #PCA on combined 2D datasets 
#' combin2D<-twodviews(Lset2D_list,scale=TRUE,vector=c(1:5))
#' #plot of the first two Principal Components
#' plot(combin2D$PCscores)
#' text(combin2D$PCscores,labels=rownames(combin2D$PCscores))
#' #load the 3D primate dataset
#' data("Lset3D_array")
#' #GPA and PCA
#' GPA_3D<-procSym(Lset3D_array)
#' #plot of the first two Principal Components
#' plot(GPA_3D$PCscores)
#' text(GPA_3D$PCscores,labels=rownames(GPA_3D$PCscores))
#' @export

twodviews<-function(twodlist,scale=TRUE,vector=c(1:5)){
  
  sizes=matrix(NA,ncol=length(vector),nrow=dim(list2array(twodlist[[1]]))[3])
  mshapes=list()
  dims=NULL
  Rots=NULL
  for(h in vector){
    coo<-list2array(twodlist[[h]])
    dims<-c(dims,dim(coo)[1])
    if(scale==FALSE){
      gpa<-procSym(coo,scale = FALSE,CSinit = FALSE)}else{
        gpa<-procSym(coo,scale=TRUE)
      }
    coo.r<-gpa$orpdata
    Rots<-cbind(Rots,(vecx(coo.r)*sqrt(dim(coo)[1]*2)))
    sizes[,h]<-gpa$size
    mshapes[[h]]<-gpa$mshape
  }
  Rots_pca<-prcomp(Rots, scale.=FALSE)
  values <- 0
  eigv <- Rots_pca$sdev^2
  values <- eigv[which(eigv > 1e-16)]
  lv <- length(values)
  PCs <- Rots_pca$rotation[, 1:lv]
  PCscores<-as.matrix(Rots_pca$x[, 1:lv])
  rownames(PCscores)<-names(twodlist[[1]])
  Variance<-cbind(sqrt(eigv),eigv/sum(eigv),cumsum(eigv)/sum(eigv))*100
  Variance<-Variance[1:lv,]
  size<-rowMeans(sizes)
  out<-list("PCscores"=PCscores,"PCs"=PCs,"Variance"=Variance,"size"=size,"mshapes"=mshapes,"dims"=dims,"twodlist"=twodlist)
  return(out)
}
