#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p) {
  samples <- sample(c(0, 1, 2), size=r*c, replace=TRUE, prob=c(1-p, p/2, p/2))
  m <- matrix(samples, nrow=r, ncol=c)
  return(m)
}


#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m) {
  r <- nrow(m)
  c <- ncol(m)
  grid.new <- FALSE

  for (i in 1:r) {
    red.row <- m[i,] == 1
    if (!all(red.row)) {
      while (any(red.row)) {
        red.col <- which(red.row, arr.ind=TRUE)
        for (j in red.col) {
          if (j == c) {
            next.j <- 1
          } else {
            next.j <- j+1
          }
          if (m[i, next.j] == 0) {
            m[i, j] <- 0
            m[i, next.j] <- 1
            grid.new <- TRUE
            red.row[j] <- FALSE  # moved
          } else if (m[i, next.j] == 2 || (m[i, next.j] == 1 && red.row[next.j] == FALSE)) {
            red.row[j] <- FALSE  # blocked
          }
        }
      }
    }
  }
  
  for (j in 1:c) {
    blue.col <- m[,j] == 2
    if (!all(blue.col)) {
      while (any(blue.col)) {
        blue.row <- which(blue.col, arr.ind=TRUE)
        for (i in blue.row) {
          if (i == 1) {
            next.i <- r
          } else {
            next.i <- i-1
          }
          if (m[next.i, j] == 0) {
            m[i, j] <- 0
            m[next.i, j] <- 2
            grid.new <- TRUE
            blue.col[i] <- FALSE # moved
          } else if (m[next.i, j] == 1 || (m[next.i, j] == 2 && blue.col[next.i] == FALSE)) {
            blue.col[i] <- FALSE # blocked
          }
        }
      }
    }
  }
  
  return(list(m, grid.new))
}


#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){

}
