#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

source('bml_functions.R')

run <- 6
r <- 100
c <- 100
p.values <- c(0.2, 0.3, 0.4, 0.6, 0.8)

steps <- matrix(NA, nrow=length(p.values), ncol=run)
for (i in 1:length(p.values)) {
  par(mfrow=c(3, 2), mar=c(1, 1, 1, 1), oma=c(1, 1, 1, 1))
  steps[i,] <- sapply(1:run, function(x) bml.sim(r, c, p.values[i]))
}
rownames(steps) <- c("p=0.2", "p=0.3", "p=0.4", "p=0.6", "p=0.8")
colnames(steps) <- paste("Run", 1:run)


run <- 6
r.values <- c(50, 100, 200, 1000)
c.values <- c(50, 100, 200, 10)
p <- 0.3

steps <- matrix(NA, nrow=length(r.values), ncol=run)
for (i in 1:length(r.values)) {
  par(mfrow=c(3, 2), mar=c(1, 1, 1, 1), oma=c(1, 1, 1, 1))
  steps[i,] <- sapply(1:run, function(x) bml.sim(r.values[i], c.values[i], p))
}
rownames(steps) <- c("r=50, c=50", "r=100, c=100", "r=200, c=200", "r=1000, c=10")
colnames(steps) <- paste("Run", 1:run)