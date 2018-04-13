#' export_amira
#'
#' This function exports a list of 3D landmark set in separate files (format landmarkAscii)
#' @param lista list containing 3D landmark sets
#' @param path character: path of the folder where saving the Amira landmark sets
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @examples
#' x<-c(1:20)
#' y<-seq(1,3,length=20)
#' z<-rnorm(20,0.01)
#' vertices<-cbind(x,y,z)
#' set<-list(vertices)
#' example<-export_amira(set,path=tempdir()) 
#' @export
export_amira<-function(lista,path){

if (is.null(names(lista))==TRUE) {
nomi<-paste("set",1:length(lista))
}else{NULL}
  
if (is.null(names(lista))==FALSE){
nomi<-names(lista)
for(i in 1:length(nomi)){
nomi[i]=strsplit(nomi[i],"\\.")[[1]][1]
}
}else{NULL}
   
  for (i in 1:length(lista)){
  while(tolower(paste(nomi[i],".txt",sep=""))%in%list.files(path)!=FALSE){
    m<-1  
    nomi[i]<-paste(nomi[i],"(",m,")",sep="")
    m<-m+1
  }  

cat(paste("# AmiraMesh 3D ASCII 2.0", "\r\n", sep = ""), file =paste(path,"/",nomi[i],".txt",sep=""),append = TRUE, sep = "")
cat(paste("define Markers ",dim(lista[[i]])[1] , "\r\n", sep = ""), file = paste(path,"/",nomi[i],".txt",sep=""), 
            append = TRUE, sep = "")
cat(paste("Parameters {", "\r\n", sep = ""), file= paste(path,"/",nomi[i],".txt",sep=""), 
            append = TRUE, sep = "")
cat(paste('NumSets 1,', "\r\n", sep = ""), file= paste(path,"/",nomi[i],".txt",sep=""), 
            append = TRUE, sep = "")
cat(paste('ContentType "LandmarkSet",', "\r\n", sep = ""), file= paste(path,"/",nomi[i],".txt",sep=""), 
            append = TRUE, sep = "")
cat(paste(" color 0 0 1", "\r\n", sep = ""), file= paste(path,"/",nomi[i],".txt",sep=""), 
            append = TRUE, sep = "")
cat(paste("}", "\r\n", sep = ""), file= paste(path,"/",nomi[i],".txt",sep=""), 
            append = TRUE, sep = "")
cat(paste("Markers { float[3] Coordinates } @1", "\r\n", sep = ""), file= paste(path,"/",nomi[i],".txt",sep=""), 
            append = TRUE, sep = "")
cat(paste("# Data section follows", "\r\n", sep = ""), file= paste(path,"/",nomi[i],".txt",sep=""), 
            append = TRUE, sep = "")
cat(paste("@1", "\r\n", sep = ""), file= paste(path,"/",nomi[i],".txt",sep=""), 
            append = TRUE, sep = "")
  
write.table(format(lista[[i]], scientific = F, trim = T),file= paste(path,"/",nomi[i],".txt",sep=""),
  sep = " ", append = TRUE, quote = FALSE, 
            row.names = FALSE, col.names = FALSE, na = "")  
}
}
