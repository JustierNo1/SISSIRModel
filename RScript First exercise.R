###
#Author: Patrick Glettig, Nico Wohlgemuth
#Date: 24.02.2018
#Description: This Script imports the exchange Rate data and prepares the data for analysis.

#get libraries
library(igraph)


pop <- 5000
n_0 <- 5
beta <- 5
gamma <- 1
recovered <- c()
infected <- c()
susceptible <- c()

s <- rep(0, pop)

for (i in 1:n_0){
  s[sample(pop, size = 1)] <- 1
}

iter <- 0
while ((sum(s==2) < pop)){
  pos_i <- which(s==1)
  recovered[i] <- sum(s==2)#recovered
  infected[i] <- sum(s==1)#infected
  susceptible[i] <- sum(s==0)#susceptible
  for (i in pos_i){
    if (runif(1) < (gamma/(beta+gamma))){
      #recovery
      s[i] <- 2
    } else {
      #infection
      j <- sample(pop-1,1)
      if (s[j] == 0){
        s[j] <- 1
      }
    }
  }
  print(c(sum(s==0),sum(s==1),sum(s==2)))
  iter <- iter+1
}
print(iter)

my_int <- beta/gamma
Recovered_divided <- c()
for (i in 1:length(recovered)){
  Recovered_divided[i] <- recovered[i]/my_int
}

