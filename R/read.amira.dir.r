#' read.amira.dir
#'
#' This function reads and stores in an array the coordinated allocated in a folder in separate files (format landmarkAscii)
#' @param path.dir character: path of the folder
#' @param nland numeric: number of landmark sampled in Amira
#' @return array.set numeric: a kx3xn array with landmark coordinates
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @export

read.amira.dir<-function(path.dir,nland){
names<-list.files(path.dir)
if(nland=="auto"){
dims<-c()
for(i in 1:length(names)){
dims[i]<-dim(read.amira.set(paste(path.dir,"/",names[i],sep=""),"auto"))[1]}
array.amira=array(NA,dim=c(as.numeric(names(sort(-table(dims)))[1]),3,length(names)))}
else{array.amira=array(NA,dim=c(nland,3,length(names)))}
for(i in 1:length(names)){
array.amira[,,i]=read.amira.set(paste(path.dir,"/",names[i],sep=""),nland) 
}
if(length(names)==1){
dimnames(array.amira)[[3]]=list(names)}
if(length(names)!=1){
dimnames(array.amira)[[3]]=names}
return(array.amira)
}