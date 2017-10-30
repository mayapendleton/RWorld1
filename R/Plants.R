#' Makes a matrix of character values with different plant species that have the ability
#' to survive, reproduce(spread), and compete with each other through time.
#'
#' A wrapper around \code{run.plant.ecosystem}
#' @param repro a vector of probability values (0-1) equal to the length of the number of species
#' @param survive a vector of probability values (0-1) equal to the length of the number of species
#' @param comp.mat a matrix that has dimensions the same length as the number of species. Contains the probabilities of each species
#' @param names assigns names to the species (default "a" and "b")
#' @param terrain an elevational grid generated from \code{make.terrain} on which plant.matrix will be placed
#' @param timesteps number of times to loop the plant population to simulate time (default = 50). The third value in the plant.matrix array
#' @param seed.fracs random values to start off the first plant.matrix
#' @return plants
#' @examples
#' plant.pop <- run.plant.ecosystem(terrain, timesteps=50, seed.fracs=c(0.1, 0.1, 0.1, 0.1),
#' repro=c(0.75,0.25), survive=c(0.25, .75), comp.mat=matrix(c(0.75,0.25,0.25,0.75), 2))
#' @export
run.plant.ecosystem <- function(terrain, timesteps=50, seed.fracs=c(0.1, 0.1, 0.1, 0.1), repro=c(0.4,0.6), survive=c(0.6,0.4), comp.mat=matrix(c(0.75,0.25,0.25,0.75), nrow=2,ncol=2), names=NULL){
  setup.plants <- function(repro, survive, comp.mat, names=NULL){
    if(is.null(names))
      names <- letters[seq_along(repro)]
    if(length(repro) != length(survive))
      stop("Reproduction and survival parameters needed for all species")
    if(nrow(comp.mat) != length(survive))
      stop("Competition Matrix Must Include all Species")
    repro <- setNames(repro, names)
    survive <- setNames(survive, names)
    rownames(comp.mat) <- colnames(comp.mat) <- names
    return(list(repro=repro, survive=survive, comp.mat=comp.mat,
                names=names))
  }
  p.info <- setup.plants(repro, survive, comp.mat)
  terrain <- make.terrain(4)
  plant.timestep <- function(plant.matrix, terrain, p.info=p.info){ #plants is a character matrix, but hasn't been setup yet
    survive <- function(row, col, plant.matrix, p.info=p.info){
      if(is.na(plant.matrix[row,col])==TRUE){
        return(NA)
      }
      if(plant.matrix[row,col]==""){
        return("")
      }
      if(runif(1) <= p.info$survive[plant.matrix[row,col]])
        return(plant.matrix[row,col])
      return("")
    }
    reproduce <- function(row, col, plant.matrix, p.info=p.info){
      possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))+1
      r <- sample(nrow(possible.locations), 1)
      c <- sample(ncol(possible.locations), 1)
      x <- plant.matrix[r,c]
      if(is.na(plant.matrix[r,c])==FALSE && plant.matrix[r,c] == ""){
        plant.matrix[r,c] <- plant.matrix[row,col] #some way of turning x into the same plant as the old cell
      }
      if(plant.matrix[r,c] != "" && is.na(plant.matrix[r,c])==FALSE){
        fight <- function(comp.mat, plant1, plant2){
          win <- sample(p.info$names, 1, prob=c(p.info$comp.mat[plant1,plant2],(1-p.info$comp.mat[plant1,plant2])))
          plant.matrix[r,c] <- win
          return(win)
        }
        plant.matrix[r,c] <- fight(p.info$comp.mat, plant.matrix[r,c], plant.matrix[row,col])
      }
      return(plant.matrix)
    }
    for(i in 1:nrow(plant.matrix)){
      for(j in 1:ncol(plant.matrix)){
        plant.matrix[i,j] <- survive(i, j, plant.matrix, p.info)
        if(plant.matrix[i,j] != "" && is.na(plant.matrix[i,j])==FALSE){
          plant.matrix <- reproduce(i, j, plant.matrix, p.info)
        }
      }
    }
    return(plant.matrix)
  }
  plants <- array("", dim=c(dim(terrain), timesteps+1))
  for(i in seq_len(dim(plants)[3]))
    plants[,,i][is.na(terrain)] <- NA
  seed.options <- c(p.info$names, "")
  seed.fracs=c(0.1, 0.1, 0.1)
  plants[,,1] <- sample(seed.options, length(plants[,,1]), replace = TRUE, prob=seed.fracs)
  for(i in seq_len(dim(plants)[3])){
    plants[,,i][is.na(terrain)] <- NA
  }
  for(i in seq(2,timesteps+1)){
    plants[,,(i)] <- plant.timestep(plants[,,i-1], terrain, p.info)
  }
  return(plants)
}
