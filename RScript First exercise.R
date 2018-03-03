###
#Author: Patrick Glettig, Nico Wohlgemuth
#Date: 24.02.2018
#Description: This Script imports the exchange Rate data and prepares the data for analysis.
###


#get libraries
library(reshape2)
library(ggplot2)

#Initialize Parameters
pop <- 5000
n_0 <- 5
beta <- 20
gamma <- 1


s <- rep(0, pop)

#Infect a random sample of the size N_0
for (i in 1:n_0){
  s[sample(pop, size = 1)] <- 1
}

#SIR Algorithm Loop
iter <- 0
recovered <- c()
infected <- c()
susceptible <- c()
while ((sum(s==2) < pop)&&(iter < 1000)){
  pos_i <- which(s==1)
<<<<<<< HEAD
  recovered <- c(recovered, sum(s==2))#recovered
  infected <- c(infected, sum(s==1))#infected
  susceptible <- c(susceptible, sum(s==0))#susceptible
    for (i in pos_i){
=======
  recovered[i] <- sum(s==2)#recovered
  infected[i] <- sum(s==1)#infected
  susceptible[i] <- sum(s==0)#susceptible
  for (i in pos_i){
>>>>>>> d2caee9787f49223ce7831860d7ff364d962afd0
    if (runif(1) < (gamma/(beta+gamma))){
      #recovery
      s[i] <- 2
    } else {
      #infection
      j <- sample(pop,1)
      if (s[j] == 0){
        s[j] <- 1
      }
    }
  }
  iter <- iter+1
  print(c(sum(s==0),sum(s==1),sum(s==2)))
}
print(iter)

#Calculate beta/gamma for the task
my_int <- beta/gamma
Recovered_divided <- c()
for (i in 1:length(recovered)){
  Recovered_divided[i] <- recovered[i]/my_int
}

#Use Qplot for the first plot, recovered individuals as a function of Beta/Gamma
qplot(x= seq_along(Recovered_divided),y = Recovered_divided,geom = "line")+
  labs(title = "Recovered Individuals as a function of Beta/Gamma")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

#make a dataframe of results for easier ploting in ggplot
evolution <- data.frame(susceptible,infected, recovered)#add the vectors together
evolution$counter <- seq_along(evolution$susceptible)-1#how many interactions/iterations

evolution <- melt(evolution, id.vars = "counter")#reshape the dataframe from wide to long

#GGPlot that shows the evolution of the population
ggplot(evolution, aes(x = counter, y = value, colour = variable)) +
  geom_line(size = 1)+
  scale_colour_hue(l=30)  + #use darker colors
  labs(title = "Evolution of the Infection", y = "Number of Individuals")+
  theme(axis.title.x=element_blank(),#remove x labels and legend title
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank())

