rm(list= ls())
#Exercise 1
##a) dndt = r0 * n * (1-(n/K)) - hn
##b) n = K * (1 - h/r0)
##c) h = r0
##d) n >0, h-r0 < 0, stable 
##e) hn = h * K * (1-h/r0)
##f) h = r0 / 2
##g) (h-r0) * x with the same x, the higher the h, it returns to the equilibrium slower

#Exercise 2
#2a)
n = seq(0,200)
r0 = 1
h = 0.5
K = 100
dndt = r0 * n * (1-n/K) - h*n
plot(n, dndt, type = 'l')

#2b)
Growth = function(t, n, P) {
  dndt = P$r0 * n * (1-n/P$K) - P$h * n
  return(list(dndt))
}
timevec = seq(0,100, by=0.1)
P = list(r0=1, K=100, h=0.5)
n0 = 1
output = ode(y=n0, func=Growth, times = timevec, parms = P)
plot(output, main='Fish population growth', ylab = 'Fish population',
     ylim=c(0,100))

#2c)
n_e = P$K * (1 - P$h/P$r0)
abline(n_e, 0, col='red')
n0 = n_e + 20 #choose the start value that is higher than the n_e
output = ode(y=n0, func=Growth, times = timevec, parms = P)
lines(output, col='blue')
abline(v=12.5, lty=2)

#2d)
P = list(r0=1, K=100, h=0.8) #set the h higher at 0.8
n0 = 1
output = ode(y=n0, func=Growth, times = timevec, parms = P)
plot(output, main='Fish population growth', ylab = 'Fish population',
     ylim=c(0,100))
n_e = P$K * (1 - P$h/P$r0)
abline(n_e, 0, col='red') #n_e as the intercept and 0 as the slope
n0 = n_e + 20 #choose the start value that is higher than the n_e
output = ode(y=n0, func=Growth, times = timevec, parms = P)
lines(output, col='blue')

#Exercise 3
#3a) Yes. Because dc/dt = -(c/(Kn+c))*n*K_max, when c goes to infinity, c/(Kn+c) approaches 1, and n is 1 as per cell
#3b) Yes. When c=0, (dc/dt)' = -n*K_max*(1/Kn) <0, so it is stable.
library(deSolve)
CCintake = function(t,c,P) {
  dcdt = -P$n * P$K_max*c/(P$Kn+c)
  return(list(dcdt))
}
P = list(K_max=0.1, Kn=0.5, n=10)
c0 = 1
timevec = seq(0,20,by=0.1)
result = ode(y=c0, func = CCintake, times = timevec, parms = P)
plot(result, main="MM_model", ylab="c")

#3c)dc/dt = I - uc -(c/(Kn+c))*n*K_max
#3d)
CCintake2 = function(t,c,P) {
  dcdt = P$I - P$u*c -P$n * P$K_max*c/(P$Kn+c)
  return(list(dcdt))
}
P = list(K_max=0.1, Kn=0.01, n=100, I=200, u=0.5)
c0 = 1
timevec = seq(0,20,by=0.1)
result = ode(y=c0, func = CCintake2, times = timevec, parms = P)
plot(result, main="ChemoMM_model", ylab="c")
#3e) Yes.