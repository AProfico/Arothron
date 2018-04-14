#' twodvarshape 
#' Calculates the shape variation associated to a value of PC scores associated to a specific 2D view
#' @param twodviews_ob object from twodviews()
#' @param scores numeric: the values of the PC scores for which the visualization is called
#' @param PC PC chosen
#' @param view numeric: 2D set to be used for shape variation visualization
#' @return mat matrix of 2D coordinates associated to the called shape variation
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Paolo Piras, Pasquale Raia
#' @examples
#' #load the 2D primate dataset
#' data("Lset2D_list")
#' #combine the 2D datasets and PCA
#' combin2D<-twodviews(Lset2D_list,scale=TRUE,vector=c(1:5))
#' #calculate the shape variation associated to the negative extreme value of PC1
#' min_PC1<-twodvarshape(combin2D,min(combin2D$PCscores[,1]),1,5)
#' plot(min_PC1,asp=1)
#' #calculate the shape variation associated to the positive extreme value of PC1
#' max_PC1<-twodvarshape(combin2D,max(combin2D$PCscores[,1]),1,5)
#' plot(max_PC1,asp=1)
#' @export

twodvarshape<-function(twodviews_ob,scores,PC,view){
  
  pos_pcs<-twodviews_ob$dims*2
  if(view==1){
    sel_pcs<-1:pos_pcs[view]}
  if(view==(length(view))){
    sel_pcs<-(sum(pos_pcs[1:(view-1)])+1): sum(pos_pcs[1:view])}
  if(view!=1 &view!=(length(view))){
    sel_pcs<-(sum(pos_pcs[1:(view-1)])+1):(sum(pos_pcs[1:(view-1)])+pos_pcs[view])
  }
  # mshape<-twodviews_ob$mshapes[[view]]*sqrt(twodviews_ob$dims[view]*2)
  mshape<-twodviews_ob$mshapes[[view]]*sqrt(twodviews_ob$dims[view]*2)
  PCs<-twodviews_ob$PCs[sel_pcs,PC]
  mat<-showPC(scores,PCs,mshape)
  return(mat)
}
