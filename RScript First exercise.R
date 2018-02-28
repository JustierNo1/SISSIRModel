###
#Author: Patrick Glettig, Nico Wohlgemuth
#Date: 24.02.2018
#Description: This Script imports the exchange Rate data and prepares the data for analysis.

#get libraries
library(igraph)

#define functions

#This function is used to create a basis population with some infected individuals

initialize <- function(Size, n_0 = 5){
  population <- rep(0, Size)
  infected <- sample(Size, size = n_0)
  population[infected] <- 1
  return(population)
}

#Let's test if the function works
example <- initialize(5000, 5)
sum(example==1)

#The counting functions

get_number_infected <- function(x){
  sum(x==1)
}

get_number_recovered <- function(x){
  sum(x==2)
}

get_number_susceptibles <- function(x){
  sum(x==0)
}

#The Sir evolve algorithm
sir_evolve <- function(n, b , g){
  size <- length(n)
  pos_i <- which(n==1)
  for (i in 1:length(pos_i)){
    if (runif(1) < (g/(b+g))){
      #recovery
      i <- sample(pos_i, 1)
      n[i] <- 2
    } else {
      #infection
      i <- sample(pos_i, 1)
      j <- sample(1:size,1)
        if (n[i] == 0){
          n[j] <- 1
          }
      }
    }
}

#Testing the algorithm. Be careful about infinite loops.
iter <- 0
while (get_number_recovered(example) < length(example)){
  sir_evolve(example,b = 20, g = 1)
  print(get_number_infected(example))
  iter <- iter +1
}
print(iter)

N_0 <- 1 #initial number


#Parameters

b <- seq(from = 0.1, to = 2, by=0.01) #infection probability
g <- 1 #recovery probability
N <- 5*10^3 #system size


R_0 <- b/g #basic reproduction rate

