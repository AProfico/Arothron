#' patches_frm2amira
#'
#' This function converts the .frm files, from Evan Toolbox, stored in a folder into the format landmarkAscii (semilandmark patches)
#' @param path_folder_frm character: path of the folder where the .frm files are stored
#' @param path_amira_folder character: path folder to store the landmarkAscii configurations
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @export

patches_frm2amira<-function(path_folder_frm,path_amira_folder){
  dir.create(path_amira_folder)
  for(m in 1:length(list.files(path_folder_frm))){
    first<-readLines(paste(path_folder_frm,list.files(path_folder_frm)[m],sep="/"))
    first_pos<-which(substr(first,1,9)=="<pointset")+1
    first_range<-which(first[(first_pos+1)]=="</pointset>")
    curves_pos<-first_pos[-first_range]
    for(j in 1:length(curves_pos)){
      temp_num<-strsplit(first[curves_pos[j]-1],"")
      temp_num2<-which(is.na(as.numeric(temp_num[[1]]))==FALSE)
      if(length(temp_num2)==1){
        pos_num<-as.numeric(temp_num[[1]][length(temp_num[[1]])-2])}
      if(length(temp_num2)!=1){
        pos_num<-as.numeric(paste(temp_num[[1]][temp_num2],sep=""))
        pos_num<-pos_num[1]*10+pos_num[2]
      }
      # 
      patch_i<-as.matrix(read.table(paste(path_folder_frm,path_amira_folder,sep="/"),
                                   skip=curves_pos[j], nrows=pos_num))
      lista<-list(patch_i)
      names(lista)<-paste("patch",j,list.files(path_folder_frm)[m])
      export_amira(lista,path_amira_folder)
    }
  }
}