#' read.path.amira
#'
#' This function extracts and orders the coordinate matrix from a surface path file from Amira 
#' @param path.name character: path of surface path .ascii extension file
#' @return data numeric: a kxd matrix with xyz coordinates 
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @export
read.path.amira<-function(path.name){
skip_1 <- which(readLines(path.name,  n = 9999999) == "@1")
skip_2 <- which(readLines((path.name), n = 9999999) == "@2")
vertices <- read.table((path.name), skip = skip_2)
numbering <- read.table((path.name), skip = skip_1, nrows = dim(vertices)[1]) 
data<-vertices[(numbering[,1]+1),]
rownames(data)<-c(1:dim(data)[1])
return(data)}