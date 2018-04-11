#lab 1

#Question 1

#a)
alpha <- 2+14
beta <- 2+6
y1<-rbeta(20, alpha, beta)

t_mean <- alpha/(alpha+beta)
t_sd <- sqrt(alpha*beta/((alpha+beta)^2*(alpha+beta+1)))
samples = seq(20,10000,by=200)
result = data.frame()
for(i in 1:length(samples)){
  set.seed(12345)
  temp <- rbeta(samples[i], alpha, beta)
  result[i,1] <- mean(temp)
  result[i,2] <- sd(temp)
}
colnames(result)<-c("Mean", "Sd")

plot(samples, result$Mean, ylab="Mean", xlab="Sample size", main="Mean", pch=19)
lines(samples, rep(t_mean, length(samples)))
plot(samples, result$Sd, ylab="Standard deviation", xlab="Sample size", main="Standard deviation", pch=19)
lines(samples, rep(t_sd, length(samples)))

#b)
set.seed(12345)
y<-rbeta(10000, alpha, beta)
prob <- length(y[y<0.4])/length(y)
true_prob <- pbeta(0.4, alpha, beta)

#c)
logodds <- log(y/(1-y))
hist(logodds, freq = FALSE)
lines(density(logodds))

# hist(y)
# density(logodds)
# plot(density(logodds))
# set.seed(12345)
# hist(rnorm(10000, mean(logodds), sd(logodds)))
#looks normal

#Question 2

#a)
x<-c(14, 25, 45, 25, 30, 33, 19, 50, 34, 67)
mu <- 3.5
n <- length(x)
tau2 <- sum((log(x)-mu)^2)/n
set.seed(12345)
sim_sigma2 <- rchisq(10000,n)

sigma2 <- n*tau2/sim_sigma2

s_mean<-mean(sigma2)
s_sd<-sd(sigma2)
#Theoretical values for scaled inv-chi (wikipedia):
inv_mean<-n*tau2/(n-2)
inv_sd <- sqrt(2*inv_mean^2/(n-4))

hist(sim_sigma2)

#b)
set.seed(12345)
G <- 2*pnorm(sqrt(sigma2)/sqrt(2))-1
dens_G<-density(G)
lognorm = rlnorm(10000, mean(G), sd(G))
hist(G, freq=FALSE)
lines(dens_G)


#c)
perc = 0.025*10000
lowertail <- G[order(G, decreasing = FALSE)[perc+1]]
uppertail <- G[order(G, decreasing = FALSE)[10000-perc-1]]

dataframe <- data.frame(y=dens_G$y,x=dens_G$x)
dataframe <- dataframe[order(dataframe$y,decreasing = TRUE),]
dataframe$dens <- cumsum(dataframe$y)/sum(dataframe$y)

dfx<-dataframe$dens<0.95
low<-which.min(dataframe$x[dfx])
upp<-which.max(dataframe$x[dfx])
x_low <- dataframe$x[low]
x_upp <- dataframe$x[upp]

hist(G, freq=FALSE)
lines(density(G), lwd=1)
lines(c(lowertail, uppertail), c(0.3,0.3), col="grey48", lwd=5)
lines(c(x_low, x_upp), c(1,1), col="grey20", lwd=5)
legend(x = 0.6, y=5, c("HPD", "Equal tail"), col=c("grey20", "grey48"), lwd = 3)

#Question 3

#a)
z <- c(-2.44, 2.14, 2.54, 1.83, 2.02, 2.33, -2.79, 2.23, 2.07, 2.02)
len_z <- length(z)
mu2 <- 2.39
lambda <- 1

posterior <- function(k){
  result <- lambda*exp(k*sum(cos(z-mu2))-lambda*sum(z))/(2*pi*besselI(k,0))
  return(result)
}

ks <- seq(0,1,by=0.001)
post <- posterior(ks)
hist(post, freq = FALSE)
lines(density(post))
dens_post <- density(post)
plot(dens_post)


#b)
post_mode <- max(dens_post$x)
