# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)
  has_adopted <- matrix(NA, nrow=n.doctors, ncol=n.days)
  has_adopted[, 1] <- initial.doctors

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p
  for (i in 2:n.days) {
    sample.doctors <- sample(1:n.doctors, 2)
    has_adopted[, i] <- has_adopted[, i-1]
    if (has_adopted[sample.doctors, i] == c(0, 1) || has_adopted[sample.doctors, i] == c(1, 0)) {
      if (runif(1) < p) {
        has_adopted[sample.doctors, i] <- c(1, 1)
      } 
    }
  }
  
  # return the output
  return(has_adopted)
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
n.doctors <- 9
n.days <- 60
initial.doctors <- sapply(1:n.doctors, function(x) as.integer(runif(1) < 0.1))
p.values <- seq(1, 0.2, by=-0.2)
col.values <- c("red", "green", "purple", "blue", "orange")
for (i in 1:length(p.values)) {
  has_adopted <- sim.doctors(initial.doctors, n.doctors, n.days, p.values[i])
  n.has_adopted <- apply(has_adopted, 2, sum)
  if (i == 1) {
    plot(1:n.days, n.has_adopted, type="l", col=col.values[i], xlab="Days", ylab="Number of Doctors have adopted the drug")
  } else {
    lines(1:n.days, n.has_adopted, col=col.values[i])
  }
}

