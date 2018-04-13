#' pov_selecter
#'
#' Internal function to define the points of view 
#' @param mesh object of class mesh3d 
#' @param grid matrix: a 3D grid
#' @param start.points numeric: number of center to be found
#' @param method character: select "a" or "c" for respectively AST-3D and CA-LSE method
#' @return selection numeric: positioning vector of the selected points of the grid
#' @author Antonio Profico, Alessio Veneziano, Marina Melchionna, Pasquale Raia
#' @export

pov_selecter<-function (mesh, grid,start.points=250,method="ast3d"){
  method<-tolower(substr(method,1,1))
  if(method=="c"){
  
  cat("How many point(s) of view do you want to select?", "\n", 
      "select by using right click")
  ans <- readLines(n = 1)
  pov <- as.numeric(ans)
  keep <- NULL
  bounding2 <- as.matrix(meshcube(mesh))
  v1 <- t(as.matrix(apply(bounding2, 2, mean)))
  v2 <- t(as.matrix(apply(bounding2[c(1, 2, 3, 4), ], 2, mean)))
  v3 <- t(as.matrix(apply(bounding2[c(1, 2, 5, 6), ], 2, mean)))
  cut_1 <- cutMeshPlane(mesh, v1 = c(v1), v2 = c(v2), v3 = c(v3), 
                       normal = NULL, keep.upper = FALSE)
  cut_2 <- cutMeshPlane(mesh, v1 = c(v1), v2 = c(v2), v3 = c(v3), 
                       normal = NULL, keep.upper = TRUE)
  decim_mesh <- vcgQEdecim(mesh, percent = 0.8)
  radius.s<- max(dist(bounding2,method="euclidean"))/25
  open3d()
  ids <- plot3d(grid, size = radius.s, col = "red", type = "s", 
                box = FALSE, axes = FALSE, aspect = FALSE, xaxis = NULL, 
                xlab = "", ylab = "", zlab = "")
  wire3d(decim_mesh, col = "green")
  selected<-NULL
  keep<-NULL
  j <- 1
  repeat {
    print(j)
    if (j == (pov + 1)) {
      break
    }
    selected <- selectpoints3d(ids["data"], value = FALSE, 
                               closest = TRUE, button = "right")
    keep <- c(keep, selected[2])
    spheres3d(grid[keep, ], col = "green", radius = radius.s*1.01)
    
    confirm <- NULL
    cat("Do you confirm this lanmdark?", "\n", "'Y' to confirm, 'N' to discard")
    confirm <- tolower(readLines(n = 1))
    if (confirm == "n") {
      j <- j + 1
      keep <- keep[-length(keep)]
      j <- j - 1
      rgl.close()
      open3d()
      ids <- plot3d(grid, size = radius.s, col = "red", type = "s", 
                    box = FALSE, axes = FALSE, aspect = FALSE, xaxis = NULL, 
                    xlab = "", ylab = "", zlab = "")
      wire3d(decim_mesh, col = "green")
      spheres3d(grid[keep, ], col = "green", radius = radius.s*1.01)
    }
    if (confirm == "y") {
      j <- j + 1
    }
  }
  selection <- grid[keep, ]
  rgl.close()
  return(selection)
  }
  if(method=="a"){
    
    cat("How many point(s) of view do you want to select?", "\n", 
        "select by using right click")
    ans <- readLines(n = 1)
    pov <- as.numeric(ans)
    keep <- NULL
    barycenter<-bary.mesh(mesh) 
    sphere<-vcgSphere(4)  
    sphere$vb[1,]=sphere$vb[1,]+(barycenter[1])
    sphere$vb[2,]=sphere$vb[2,]+(barycenter[2])
    sphere$vb[3,]=sphere$vb[3,]+(barycenter[3])
    bbox<-meshcube(mesh)
    pos<-aro.clo.points(t(mesh$vb)[,-4],bbox)$position
    max_dist<-as.matrix(dist(t(mesh$vb)[pos,-4],method = "euclidean"))
    max_dist_pos<-which(max_dist==max(max_dist),arr.ind=TRUE)[1,]
    sphere<-scalemesh(sphere,max_dist[max_dist_pos[1],max_dist_pos[2]]*1.5,center = "mean") #dist.sphere = 1.5
    bounding<-matrix(kmeans(t(sphere$vb)[,-4],start.points,iter.max = 100000)$centers,ncol=3,byrow = FALSE)
    grid<-bounding
    decim_mesh <- vcgQEdecim(mesh, percent = 0.8)
    radius.s<-max_dist[max_dist_pos[1],max_dist_pos[2]]/25
    open3d()
    ids <- plot3d(grid, radius = radius.s, col = "red", type = "s", 
                  box = FALSE, axes = FALSE, aspect = FALSE, xaxis = NULL, 
                  xlab = "", ylab = "", zlab = "")
    wire3d(decim_mesh, col = "green")
    selected<-NULL
    keep<-NULL
    j <- 1
    repeat {
      print(j)
      if (j == (pov + 1)) {
        break
      }
      selected <- selectpoints3d(ids["data"], value = FALSE, 
                                 closest = TRUE, button = "right")
      keep <- c(keep, selected[2])
      spheres3d(grid[keep, ], col = "green", radius = radius.s*1.01)
      
      confirm <- NULL
      cat("Do you confirm this lanmdark?", "\n", "'Y' to confirm, 'N' to discard")
      confirm <- tolower(readLines(n = 1))
      if (confirm == "n") {
        j = j + 1
        keep = keep[-length(keep)]
        j = j - 1
        rgl.close()
        open3d()
        ids <- plot3d(grid, size = radius.s, col = "red", type = "s", 
                      box = FALSE, axes = FALSE, aspect = FALSE, xaxis = NULL, 
                      xlab = "", ylab = "", zlab = "")
        wire3d(decim_mesh, col = "green")
        spheres3d(grid[keep, ], col = "green", radius = radius.s*1.01)
      }
      if (confirm == "y") {
        j = j + 1
      }
    }
    selection <- grid[keep, ]
    rgl.close()
    return(selection)
  }
}

