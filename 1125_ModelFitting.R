#read and plot the data
Ringlets = read.csv("E:/1Lund Lectures/BIOS13_Modelling/Exercise/20221125/Ringlets.csv")
head(Ringlets) #year and number
plot(Ringlets, type='b')
plot(Ringlets$year, log(Ringlets$N), type='b')

#prediction
Nt = Ringlets$N[1:23]
Ntplus1 = Ringlets$N[2:24]
plot(Nt, Ntplus1, xlab = 'N(t)', ylab = 'N(t+1)')

#nls function (non-linear least squares)
Rickerfit = nls(Ntplus1 ~ Nt*exp(r0*(1-Nt/K)),
                data = list(Nt=Nt),
                start = list(r0=1, K=100))
summary(Rickerfit)
points(Nt, fitted(Rickerfit), col='red')
#alarming dependence
plot(Nt, residuals(Rickerfit))
#population growth rate
rt = log(Ntplus1/Nt)
plot(Nt, rt)
rt[1] #outlier caused by the severe drought in 1976

#remove the outlier
Nt_new = Ringlets$N[2:23]
Ntplus1_new = Ringlets$N[3:24]
rt_new = log(Ntplus1_new/Nt_new)
plot(Nt_new, Ntplus1_new, xlab = 'N(t)', ylab = 'N(t+1)')
plot(Nt_new, rt_new)

#nls predicting rt
rt_fit = nls(rt_new ~ r0*(1-Nt_new/K),
             data = list(Nt_new=Nt_new),
             start = list(r0=1, K=100))
summary(rt_fit)
points(Nt_new, fitted(rt_fit), col='red')
plot(Nt_new, residuals(rt_fit))
AIC(rt_fit) #19.45281

#Hassel model
##rt = log(lambda)-b*log(1+a*Nt)
rt_h_fit = nls(rt_new ~ log(lambda)-b*log(1+a*Nt_new),
               data = list(Nt_new=Nt_new),
               start = list(lambda=1.1, a=0.001, b=1))
summary(rt_h_fit)
AIC(rt_h_fit) #21.31359
## the AIC for Ricker is smaller than Hassel, so Ricker model is better

#Monte-Carlo simulations
Ringlet_dynamic = function(Parameter, tmax) {
  year_vec = seq(1999,(1999+tmax))
  Nt_vec = rep(0, tmax+1) #to contain Nt
  Nt_vec[1] = Ringlets$N[24]
  
  for (i in 1:tmax) {
    lambda_t = Parameter$r0*(1-Nt_vec[i]/Parameter$K)+rnorm(1,mean=0,sd=Parameter$sd)
    Nt_vec[i+1] = Nt_vec[i]*exp(lambda_t)
  }
  
  output = list(year_vec=year_vec, Nt_vec=Nt_vec)
  return(output)
}
#test
Parameter = list(r0=1.004, K=311, sd=0.3445)
output = Ringlet_dynamic(Parameter, 100)
plot(output$year_vec, output$Nt_vec,
     xlab = 'Year', ylab = 'Nt', type='l', pch=1)

#first 20 simulations
tmax = 100
Parameter = list(r0=1.004, K=311, sd=0.3445)
#create an empty plot
plot(NA, xlab = 'Year', ylab = 'Nt',
     xlim = c(1999, (1999+tmax)),
     ylim = c(100,1000))
for (iter in 1:20) {
  output = Ringlet_dynamic(Parameter, tmax)
  lines(output$year_vec, output$Nt_vec, col=iter)
}
