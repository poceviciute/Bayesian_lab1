#lab 1

#Question 1

#a)
y1<-rbeta(20, 2+14, 2+6)
#true_mean <- 14/20
#true_sd <- sqrt(true_mean*(1-true_mean))
alpha <- 2+14
beta <- 2+6
t_mean <- alpha/(alpha+beta)
t_sd <- sqrt(alpha*beta/((alpha+beta)^2*(alpha+beta+1)))
samples = seq(20,10000,by=200)
result = data.frame()
for(i in 1:length(samples)){
  temp = rbeta(samples[i], 2+14, 2+6)
 result[i,1] = mean(temp)
 result[i,2] <- sd(temp)
}
result
plot(result[,1], ylab="Mean", pch=19)
lines(1:length(samples), rep(t_mean, 50))
plot(result[,2], ylab="Standard deviation", pch=19)
lines(1:length(samples), rep(t_sd, 50))

#b)
y<-rbeta(10000, alpha, beta)
prob <- length(y[y<0.4])/length(y)
true_prob <- pbeta(0.4, alpha, beta)

#c)
logodds <- log(y/(1-y))
hist(logodds, freq = FALSE)
lines(density(logodds))
hist(y)
density(logodds)
plot(density(logodds))
hist(rnorm(10000, mean(logodds), sd(logodds)))
#looks normal

#Question 2

#a)
x<-c(14, 25, 45, 25, 30, 33, 19, 50, 34, 67)
mu <- 3.5
n <- length(x)
tau2 <- sum((log(x)-mu)^2)/n

sim_sigma2 <- rchisq(10000,n)

sigma2 <- n*tau2/sim_sigma2

s_mean<-mean(sigma2)
s_sd<-sd(sigma2)
#Theoretical values for scaled inv-chi (wikipedia):
inv_mean<-n*tau2/(n-2)
inv_sd <- sqrt(2*inv_mean^2/(n-4))

hist(sim_sigma2)

#b)

G <- 2*pnorm(sqrt(sigma2)/sqrt(2))-1

lognorm = rlnorm(10000, mean(G), sd(G))
hist(G, freq=FALSE)
lines(density(G))
dens_G<-density(G)

#c)
perc = 0.025*10000
perc2 <- floor(0.025*length(dens_G$y))
lowertail <- G[order(G, decreasing = FALSE)[perc+1]]
uppertail <- G[order(G, decreasing = FALSE)[10000-perc-1]]

dn <- cumsum(dens_G$y)/sum(dens_G$y)
li <- which(dn>=0.05)[1]
ui <- which(dn>=0.95)[1]
dens_G$x[c(li,ui)]

hist(G, freq=FALSE)
lines(density(G))
lines(c(lowertail, uppertail), c(0,0), col="red", lwd=5)
lines(dens_G$x[c(li,ui)], c(1,1), col="grey", lwd=5)

#3
#a)
z <- c(-2.44, 2.14, 2.54, 1.83, 2.02, 2.33, -2.79, 2.23, 2.07, 2.02)
mu2 <- 2.39
lambda <- 1

posterior <- function(k){
  result <- exp(k*sum(cos(z-mu2))-sum(z))/(2*pi*besselI(k,0))
  return(result)
}

ks <- seq(0,1,by=0.001)
post <- posterior(ks)
hist(post, freq = FALSE)
