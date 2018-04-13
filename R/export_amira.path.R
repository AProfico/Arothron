#' export_amira.path
#'
#' Convert and save a 3D matrix into a AmiraMesh ASCII Lineset (.am) object
#' @param vertices numeric: a kx3 matrix
#' @param filename character: name of the requested output
#' @param Lines numeric: sequence of the vertices that defines the line
#' @param path character: folder path 
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @examples
#' library(Arothron)
#' x<-c(1:20)
#' y<-seq(1,3,length=20)
#' z<-rnorm(20,0.01)
#' vertices<-cbind(x,y,z)
#' export_amira.path(vertices=vertices,filename="example_line",path=tempdir())
#' @export
export_amira.path<-function(vertices,filename,Lines=c(1:(dim(vertices)[1]-1)-1,-1),path)
{

  while(tolower(paste(filename,".am",sep=""))%in%list.files(path)!=FALSE){
  i<-1  
  filename<-paste(filename,"(",i,")",sep="")
  i<-i+1
  }
  cat(paste("# AmiraMesh 3D ASCII 2.0", "\n\n", sep = ""), 
      file = paste(path, "/", filename, ".am", sep = ""), 
      append = TRUE, sep = "",eol="\n")
  cat(paste("define Lines ", length(Lines), "\n", 
            sep = ""), file = paste(path, "/", filename, ".am", 
                                    sep = ""), append = TRUE, sep = "")
  cat(paste("nVertices ", dim(vertices)[1], "\n\n", 
            sep = ""), file = paste(path, "/", filename, ".am", 
                                    sep = ""), append = TRUE, sep = "")
 
  cat(paste("Parameters {","\n",sep=""), file = paste(path, "/", filename, ".am", 
                                    sep = ""), append = TRUE, sep = "")
  cat(paste('    ContentType "HxLineSet"',"\n",sep=""), file = paste(path, "/", filename, ".am", 
                                   sep = ""), append = TRUE, sep = "")
  cat(paste("}", "\n\n", sep = ""), file = paste(path, "/", filename, ".am", 
                                                  sep = ""), append = TRUE, sep = "")
  cat(paste("Lines { int LineIdx } @1","\n",sep=""), file = paste(path, "/", filename, ".am", 
                                   sep = ""), append = TRUE, sep = "")
  cat(paste("Vertices { float[3] Coordinates } @2", "\n\n", sep = ""), file = paste(path, "/", filename, ".am", 
                                               sep = ""), append = TRUE, sep = "")
  cat(paste("# Data section follows","\n","@1","\n",sep=""), file = paste(path, "/", filename, ".am", 
                                               sep = ""), append = TRUE, sep = "")
  cat(paste(Lines,"\n",sep=" "), file = paste(path, "/", filename, ".am", 
                                                                          sep = ""), append = TRUE, sep = "")
  cat(paste("\n","@2","\n",sep=""), file = paste(path, "/", filename, ".am", 
                                             sep = ""), append = TRUE, sep = "")
  write.table(format(vertices, scientific = F, trim = T), 
              file = paste(path, "/", filename, ".am", sep = ""), 
              sep = " ", append = TRUE, quote = FALSE, row.names = FALSE, 
              col.names = FALSE, na = "")
  cat(paste("\n",sep=" "), file = paste(path, "/", filename, ".am", 
                                                 sep = ""), append = TRUE, sep = "")
  
}
