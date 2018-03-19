#' landmark_frm2amira
#'
#' This function converts the .frm files, from Evan Toolbox, stored in a folder into the format landmarkAscii
#' @param path_folder_frm character: path of the folder where the .frm files are stored
#' @param path_amira_folder character: path folder to store the landmarkAscii configurations
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @export
landmark_frm2amira<-function(path_folder_frm,path_amira_folder){
dir.create(path_amira_folder)
for(j in 1:length(list.files(path_folder_frm))){
first<-readLines(paste(path_folder_frm,list.files(path_folder_frm)[j],sep="/"))
first_pos<-which(substr(first,1,9)=="<pointset")+1
first_range<-which(first[(first_pos+1)]=="</pointset>")
out_land<-matrix(NA,ncol=3,nrow=length(first_range))
for(i in 1:length(first_range)){
  second<-strsplit(first[first_pos[first_range]][i],' ')
  third<-unlist(strsplit(second[[1]],"<locations>"))
  fourth<-unlist(strsplit(third,"</locations>"))
  landmark<-as.numeric(fourth)
  out_land[i,]<-landmark
}
ref_set<-out_land
lista<-list(ref_set)
names(lista)<-list.files(path_folder_frm)[j]
export_amira(lista,path_amira_folder)
}
}
