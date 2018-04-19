#title: "SIR Algorithm with Parameter Estimation based on Real-word data"
#author: "Patrick Glettig & Nicolas Wohlgemuth"
#date: "14 April 2018"

#we need these libraries to plot the SIR evolution
library(ggplot2)
library(reshape2)
library(zoo)

#First we need some data, more generally a dataframe with Susceptible, Infected and Recovered.
#We find this by product sales data. Susceptible is the total potential market. I_0 the first movers. R will be 0
#in the beginning.
## S(t) susceptible, I(t) infected, R(t) recovered at time t
## Probabilistic model in discrete time:
## S(t+1) = S(t) - DeltaS(t)
## I(t+1) = I(t) + DeltaS(t) - DeltaR(t)
## R(t+1) = R(t) + DeltaR(t)
## DeltaR(t) ~ Binomial(I(t), gamma) >= 0
## DeltaS(t) ~ Binomial(S(t), 1 - (1 - beta)^I(t)) >= 0

# Load the data
sales <- read.csv('RawDataTesla.csv', header = TRUE)
# Check data
str(sales)
#we need to do some grooming, the numbers with thousands separator are read as decimals.
sales[is.na(sales)] <- 0 #first we set the missing values to zeros

for (i in 2:ncol(sales)){#Checks whether value was entered in single format or in thousands format.
  for (j in 1:nrow(sales)){
    if (sales[j,i] < 100){
      sales[j,i] <- sales[j,i]*1000
    }
  }
}
#now we need to turn this sales in long format
sales <- melt(sales,id.vars = "Month", variable.name = "Year", value.name = "Sales")
#The x in front of the year is legacy, we remove it using substring
sales$Year <- substr(sales$Year,2,5)
sales$Year <- as.numeric(sales$Year)#now we can also convert it to be numeric
#we add a column of the cumulative sales
sales$TotalSales <- cumsum(sales$Sales)
#we add a time period variable, where the release month is period 1
sales$t <- seq_along(sales$Month)
#we create a Date column
sales$Date <- as.yearmon(paste(sales$Month, sales$Year), "%b %Y")
#we delete rows for which we do not yet have sales data (nulls)
sales <- sales[sales$Sales>0,]
#we check the result of the groomed data
ggplot(data=sales, aes(x=Date,y=TotalSales))+
  geom_line(size=1.1)+ theme_bw()

#We now need to build a dataframe in the SIR-Format, so a time column, a susceptible, infected and recovered column.
df <- sales[sales$t,]
df$I <- sales$TotalSales
# Calculate recovered: we assume that the number of people who change to other cars increases over time.
# For now I use a random function that increases recovered. I divide by four because I assume that the Model will
# be longer on the market.
df$R <- sort(sample.int(max(sales$TotalSales)/20,nrow(df)))
df[1,"R"] <- 0#we reset the recovered ones to zero
# Infected = Lagged Infected + New Sales - Recovered


n <- 200000 #total market size
df$S <- n-df$I-df$R

#Check the evolution of susceptible (potential market), infected (product owners) and recovered (left product)
df_melted <- melt(df, id.vars="t")
p <- (ggplot(df_melted, aes(x=t, y=value, color=variable)) +
        geom_line(size=1.1) + theme_bw() +
        xlab("time") +
        theme(legend.key=element_blank()) +
        theme(panel.border=element_blank()))
p

#With this in place, we want to get the MLE estimates

#Write a function that normalizes the data.
inverse_logit <- function(x) {
  p <- exp(x) / (1 + exp(x))  # Maps R to [0, 1]
  return(p)
}
curve(inverse_logit, -10, 10)  # Sanity check

#loglikelihood function
loglik <- function(logit_beta_gamma, df) {
  stopifnot(length(logit_beta_gamma) == 2)#if we do not have two starting values with which we optimize
  beta <- inverse_logit(logit_beta_gamma[1])#normalize the beta parameter suggested
  gamma <- inverse_logit(logit_beta_gamma[2])#normalize the gamma parameter suggested
  dS <- -diff(df$S)#calculate delta susceptible
  dR <- diff(df$R)#calculate delta recovered
  n <- nrow(df)#get number of observations
  pr_dS <- 1 - (1-beta)^df$I[seq_len(n-1)]  #probability of observing dS given the data
  return(sum(dbinom(dS, size=df$S[seq_len(n-1)], prob=pr_dS, log=TRUE) +#takes normaldist and returns number of success
               dbinom(dR, size=df$I[seq_len(n-1)], prob=gamma, log=TRUE)))
}
#optimize loglikelihood
get_estimates <- function(df) {
  mle <- optim(par=c(-4, 0), fn=loglik, control=list(fnscale=-1), df=df)#find ideal, namely maximize successes
  beta_gamma_hat <- inverse_logit(mle$par)
  names(beta_gamma_hat) <- c("beta", "gamma")
  return(beta_gamma_hat)
}

###
#The SIR Algorithm
###

#Create a function that gives us a SIR Algorithm
SIR <- function(pop=100, S1=100, I1=10, R1=0, beta=0.005, gamma=0.10) {
  stopifnot(pop > 0)#check if population larger 0
  stopifnot(beta >= 0 && beta <= 1)#our SIR algorithm works with probability, because of the LOG-likelihood estimates
  stopifnot(gamma >= 0 && gamma <= 1)
  total_pop <- S1 + I1 + R1
  df <- data.frame(t=seq_len(pop))#create time numbers
  df[, c("S", "I", "R")] <- NA
  for(t in seq_len(pop)) {
    if(t == 1) {
      df$S[t] <- S1
      df$I[t] <- I1
      df$R[t] <- R1
      next
    }
    DeltaS <- rbinom(n=1, size=df$S[t-1], prob=1 - (1-beta)^df$I[t-1])
    DeltaR <- rbinom(n=1, size=df$I[t-1], prob=gamma)
    df$S[t] <- df$S[t-1] - DeltaS
    df$I[t] <- df$I[t-1] + DeltaS - DeltaR
    df$R[t] <- df$R[t-1] + DeltaR
    stopifnot(df$S[t] + df$I[t] + df$R[t] == total_pop)  # Sanity check
  }
  return(df)
}

###
#Check our estimates
###

#we use the values estimated as our beta and gamma
beta <- get_estimates(df)['beta']
gamma <- get_estimates(df)['gamma']

#Create the simulated dataframe
simulated <- SIR(beta = beta, gamma = gamma)

#Now we benchmark this against the real population
simulated_melted <- melt(simulated, id.vars="t")
s <- ggplot(NULL, aes(x=t,y=value, color=variable))+
        geom_line(data=df_melted,size=1.1,linetype=4) + theme_bw() +
        geom_line(data=simulated_melted,size=1.1)+
        xlab("time") +
        theme(legend.key=element_blank()) +
        theme(panel.border=element_blank())
s

