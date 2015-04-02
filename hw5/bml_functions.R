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

#   for (i in 1:r) {
#     red.row <- m[i,] == 1
#     if (!all(red.row)) {
#       while (any(red.row)) {
#         red.col <- which(red.row, arr.ind=TRUE)
#         for (j in red.col) {
#           if (j == c) {
#             next.j <- 1
#           } else {
#             next.j <- j+1
#           }
#           if (m[i, next.j] == 0) {
#             m[i, j] <- 0
#             m[i, next.j] <- 1
#             grid.new <- TRUE
#             red.row[j] <- FALSE  # moved
#           } else if (m[i, next.j] == 2 || (m[i, next.j] == 1 && red.row[next.j] == FALSE)) {
#             red.row[j] <- FALSE  # blocked
#           }
#         }
#       }
#     } else {
#       grid.new <- TRUE
#     }
#   }
#   
#   for (j in 1:c) {
#     blue.col <- m[,j] == 2
#     if (!all(blue.col)) {
#       while (any(blue.col)) {
#         blue.row <- which(blue.col, arr.ind=TRUE)
#         for (i in blue.row) {
#           if (i == 1) {
#             next.i <- r
#           } else {
#             next.i <- i-1
#           }
#           if (m[next.i, j] == 0) {
#             m[i, j] <- 0
#             m[next.i, j] <- 2
#             grid.new <- TRUE
#             blue.col[i] <- FALSE # moved
#           } else if (m[next.i, j] == 1 || (m[next.i, j] == 2 && blue.col[next.i] == FALSE)) {
#             blue.col[i] <- FALSE # blocked
#           }
#         }
#       }
#     } else {
#       grid.new <- TRUE
#     }
#   }
  
  m.new <- m[,]

  if (c > 1) {
    red <- m.new * (m.new == 1)
    not.red <- m.new * (m.new != 1)
    blocked <- m.new[, c(2:c, 1)] != 0
    red.blocked <- red * blocked
    red.moved <- (red * !blocked)[, c(c, 1:c-1)]
    m.new <- not.red + red.blocked + red.moved
  }
  if (r > 1) {
    blue <- m.new * (m.new == 2)
    not.blue <- m.new * (m.new != 2)
    blocked <- m.new[c(r, 1:r-1), ] != 0
    blue.blocked <- blue * blocked
    blue.moved <- (blue * !blocked)[c(2:r, 1), ]
    m.new <- not.blue + blue.blocked + blue.moved
  }
  
  grid.new <- any(m != m.new)
  m <- m.new
  
  return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : [num.steps] : number of steps taken to hit gridlock

bml.sim <- function(r, c, p) {
  n <- 1000
  m <- bml.init(r, c, p)
  
  # image(t(m[r:1,]), axes=FALSE, col=c("white", "red", "blue"))
  for (num.steps in 1:n) {
    m.next <- bml.step(m)
    if (m.next[[2]]) {
      m <- m.next[[1]]
      # image(t(m[r:1,]), axes=FALSE, col=c("white", "red", "blue"))
    } else {
      image(t(m[r:1,]), axes=FALSE, col=c("white", "red", "blue"))
      return(num.steps)
    }
  }
  
  image(t(m[r:1,]), axes=FALSE, col=c("white", "red", "blue"))
  return(num.steps)
}

