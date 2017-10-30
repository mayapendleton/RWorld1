#' Make an elevational grid with optional lakes
#'
#' A light wrapper around \code{land.fill}
#' @param n Size of grid will be a 2^n +1 grid (default = 4; a 17X17 grid)
#' @param lakes logical whether to make all terrain lower than -3
#' height underwater (default: TRUE)
#' @return a terrain matrix; numeric elements indicate heights, and
#' NAs indicate cells filled with water
#' @examples
#' big.terrain <- make.terrain(4)
#' image(big.terrain)
#' @export
make.terrain <- function(n, lakes=TRUE){
  start.terrain <- function(n){
    mat <- matrix(nrow=((n^2)+1), ncol=((n^2)+1))
    mat[1, ncol(mat)] <- rnorm(1, 0, 10)
    mat[nrow(mat), 1] <- rnorm(1, 0, 10)
    mat[nrow(mat), ncol(mat)] <- rnorm(1, 0, 10)
    mat[1, 1] <- rnorm(1, 0, 10)
    return(mat)
  }
  diamond.step <- function(mat){
    corners <- c(mat[1,1], mat[1,ncol(mat)], mat[nrow(mat),1], mat[nrow(mat), ncol(mat)])
    mat[((nrow(mat)/2)+1), ((ncol(mat)/2)+1)] <- mean(corners)
    return(mat)
  }
  square.step <- function(mat){
    mid.row <- ((nrow(mat)/2)+1)
    mid.col <- ((ncol(mat)/2)+1)
    top.left <- mat[1,1]
    top.right <- mat[1,ncol(mat)]
    bottom.left <- mat[nrow(mat),1]
    bottom.right <- mat[nrow(mat),ncol(mat)]
    center <- mat[mid.row,mid.col]
    top.mid <- c(top.left,top.right,center)
    right.mid <- c(top.right, center, bottom.right)
    bottom.mid <- c(bottom.right, center, bottom.left)
    left.mid <- c(bottom.left, center, top.left)
    mat[1,mid.col] <- mean(top.mid)
    mat[mid.row,ncol(mat)] <- mean(right.mid)
    mat[nrow(mat),mid.col] <- mean(bottom.mid)
    mat[mid.row,1] <- mean(left.mid)
    return(mat)
  }
  land.fill<-function(n){
    mat<-start.terrain(n)
    for (i in 2^(n:1)) {
      x<-seq(1, nrow(mat), by=i)
      y <- length(x)-1
      for (j in 1:y){
        for (k in 1:y){
          mat[x[j]:x[j+1],x[k]:x[k+1]]<-diamond.step(mat[x[j]:x[j+1],x[k]:x[k+1]])
          mat[x[j]:x[j+1],x[k]:x[k+1]]<-square.step(mat[x[j]:x[j+1],x[k]:x[k+1]])
      }
    }
    if(lakes==TRUE){
      return(replace(mat, mat < -3, NA))
    }
    return(mat)
    }
  }
  land.fill(n)
}





