#' Converts a 3d object with x, y, z coordinates to a stl file.
#'
#'
#' @param x a matrix
#' @param y a matrix
#' @param z a matrix
#' @param object.name what is the name of the 3d object
#' @param plot should the object be plotted as well
#' @param file.name name for file, should end with ".stl"
#' @param file.path where should the file be saved
#'
#'
#' @return an stl file
#'
#' @examples
#' a = 0.94; d = 0.56; c = 0.34; b = sqrt(a^2 - c^2)
#' uv <- mesh(x=seq(0,2*pi,length.out = 30),y=seq(0,2*pi,length.out = 30))
#' x <- (d*(c - a*cos(uv$x)*cos(uv$y)) + b^2*cos(uv$x)) / (a - c*cos(uv$x)*cos(uv$y))
#' y <- (b*sin(uv$x)*(a - d*cos(uv$y))) / (a - c*cos(uv$x)*cos(uv$y))
#' z <- (b*sin(uv$y)*(c*cos(uv$x) - d)) / (a - c*cos(uv$x)*cos(uv$y))
#' mesh2stl(x, y, z)
#'
#' @export

mesh2stl<-function(x,y,z, plot=TRUE,file.path=NULL, file.name=NULL, object.name="object"){
  if(is.null(file.path)){
    file.path<-paste0(getwd(), "/")
  }
  if(is.null(file.name)){
    fp<-paste0(file.path, object.name, ".stl")
  }else{
    fp<-paste0(file.path, object.name)
  }

  triangleNormal <- function(v1,v2,v3) {
    x <- v2 - v1
    y <- v3 - v2
    z <- cbind(x[,2]*y[,3] - x[,3]*y[,2],
               x[,3]*y[,1] - x[,1]*y[,3],
               x[,1]*y[,2] - x[,2]*y[,1])
    z / sqrt(rowSums(z^2))
  }

  nx<-dim(z)[1]
  ny<-dim(z)[2]

  top.left.A<-expand.grid(row=1:(ny-1),col=1:(nx-1))
  bottom.left.A<-expand.grid(row=2:ny,col=1:(nx-1))
  bottom.right.A<-expand.grid(row=2:ny,col=2:nx)

  top.left.B<-expand.grid(row=1:(ny-1),col=1:(nx-1))
  bottom.right.B<-expand.grid(row=2:ny,col=2:nx)
  top.right.B<-expand.grid(row=1:(ny-1),col=2:nx)

  v1<-rbind(t(sapply(1:length(top.left.A$row),function(i){c(x[top.left.A$row[i], top.left.A$col[i]],
                                                            y[top.left.A$row[i], top.left.A$col[i]],
                                                            z[top.left.A$row[i], top.left.A$col[i]])})),
            t(sapply(1:length(top.left.B$row),function(i){c(x[top.left.B$row[i], top.left.B$col[i]],
                                                            y[top.left.B$row[i], top.left.B$col[i]],
                                                            z[top.left.B$row[i], top.left.B$col[i]])})))



  v2<-rbind(t(sapply(1:length(bottom.left.A$row),function(i){c(x[bottom.left.A$row[i], bottom.left.A$col[i]],
                                                               y[bottom.left.A$row[i], bottom.left.A$col[i]],
                                                               z[bottom.left.A$row[i], bottom.left.A$col[i]])})),
            t(sapply(1:length(bottom.right.B$row),function(i){c(x[bottom.right.B$row[i], bottom.right.B$col[i]],
                                                                y[bottom.right.B$row[i], bottom.right.B$col[i]],
                                                                z[bottom.right.B$row[i], bottom.right.B$col[i]])})))



  v3<-rbind(t(sapply(1:length(bottom.right.A$row),function(i){c(x[bottom.right.A$row[i], bottom.right.A$col[i]],
                                                                y[bottom.right.A$row[i], bottom.right.A$col[i]],
                                                                z[bottom.right.A$row[i], bottom.right.A$col[i]])})),
            t(sapply(1:length(top.right.B$row),function(i){c(x[top.right.B$row[i], top.right.B$col[i]],
                                                             y[top.right.B$row[i], top.right.B$col[i]],
                                                             z[top.right.B$row[i], top.right.B$col[i]])})))




  mytri<-triangleNormal(v1,v2,v3)

  toremove<-which(is.na(mytri[,1])|is.na(mytri[,1])|is.na(mytri[,1]))
  if(length(toremove)>0){
    mytri<-mytri[-toremove,]
    v1<-v1[-toremove,]
    v2<-v2[-toremove,]
    v3<-v3[-toremove,]
  }

  myverts<-paste0("  facet normal ",apply(formatC(mytri, format="f",digits=6),1,paste, collapse=" "),
                  "\n   outer loop\n   vertex ",
                  apply(formatC(v1, format="f",digits=6),1,paste, collapse=" "),
                  "\n   vertex ",apply(formatC(v2, format="f",digits=6),1,paste, collapse=" "),
                  "\n   vertex ",apply(formatC(v3, format="f",digits=6),1,paste, collapse=" "),
                  "\n   endloop\n  endfacet")



  write(sprintf('solid %s', object.name), file=fp)
  write(paste( myverts, collapse = "\n"), file=fp,append = T)
  write(sprintf('endsolid %s', object.name), file=fp,append = T)
  print(paste0("file complete and located at ",fp))
  if(plot){
    surf3D(x, y, z,
           bty="b", ticktype="detailed", scale=FALSE, col = "grey", lighting=TRUE)

  }
}
