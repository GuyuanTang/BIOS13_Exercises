#Exercise 1 Lotka-Volterras predator-prey equations
##1.a)
LV_isoclines = function(r,a,mu) {
  #prey isocline
  abline(v=0, col='green', lwd=2) #n=0
  abline(h=(r/a), col='green', lwd=2) #p=r/a
  #predator isocline
  abline(h=0, col='orange', lwd=2) #p=0
  abline(v=(mu/a), col='orange', lwd=2) #n=mu/a
  #adding legend
  legend('topright', legend = c('prey isocline', 'predator isocline'),
         lty='solid', col=c('green', 'orange'),
         bty='n', lwd=2, cex = 0.5)
}
LV_isoclines(1,1,2)

##1.b)
LV_sys = function(t,np,Parameters) {
  #extract vector content
  n = np[1]
  p = np[2]
  #calculate the two growth rates
  dndt = Parameters$r*n - Parameters$a*n*p
  dpdt = Parameters$a*n*p - Parameters$mu*p
  return(list(c(dndt, dpdt)))
}
np0 = c(2,1)
Parameters = list(r=10, a=2, mu=10)
timevec = seq(0,20, by=0.1)
result = ode(y=np0, func=LV_sys, times=timevec, parms=Parameters)
#plot the prey densities in blue
plot(result[,1], result[,2], type='l', col='blue',
     xlab = 'time', ylab='density')
#add the predator densities in red
lines(result[,1], result[,3], col='red')
#add legend
legend('topright', legend=c('prey', 'predator'), lty='solid', col=c('blue', 'red'), cex=0.6, bty='n')
#the phase-plane plot
plot(result[,2], result[,3], type='l',
     xlab='prey density', ylab='predator density')
LV_isoclines(Parameters$r, Parameters$a, Parameters$mu)

##1c)
J_matrix = function(r, a, mu) {
  out_matrix = matrix(c(0, r, -mu, 0), nrow=2)
  return(out_matrix)
}
Jac_Matrix = J_matrix(Parameters$r, Parameters$a, Parameters$mu)

##1d)
E = eigen(Jac_Matrix)
E$values
#period of cycles ??? the real part is 0 for LV
CycleT = 2*pi/abs(Jac_Matrix[1,1] + Jac_Matrix[2,2])

###########################
#Exercise 2 Stabilising Lotka-Volterra
##2a)
#prey isoclines: n=0 or p=r0/a*(1-n/K)
#predator isoclines: p=0 or n=mu/a

##2b)
#n<K, then in the prey isoclines, p could be positive at the equilibrium

##2c)
LV_isoclines2 = function(r,a,mu,K) {
  #prey isocline
  abline(v=0, col='green', lwd=2) #n=0
  abline(r/a, -r/(a*K), col='green', lwd=2) #p=r/a*(1-n/k)
  #predator isocline
  abline(h=0, col='orange', lwd=2) #p=0
  abline(v=(mu/a), col='orange', lwd=2) #n=mu/a
  #adding legend
  legend('topright', legend = c('prey isocline', 'predator isocline'),
         lty='solid', col=c('green', 'orange'),
         bty='n', lwd=2, cex = 0.5)
}

##2d)
LV_sys2 = function(t,np,Parameters) {
  #extract vector content
  n = np[1]
  p = np[2]
  #calculate the two growth rates
  dndt = Parameters$r*n*(1-n/Parameters$K) - Parameters$a*n*p
  dpdt = Parameters$a*n*p - Parameters$mu*p
  return(list(c(dndt, dpdt)))
}
np0 = c(2,1)
Parameters = list(r=5, a=2, mu=10, K=50)
timevec = seq(0,20, by=0.1)
result = ode(y=np0, func=LV_sys, times=timevec, parms=Parameters)
#plot the prey densities in blue
plot(result[,1], result[,2], type='l', col='blue',
     xlab = 'time', ylab='density')
#add the predator densities in red
lines(result[,1], result[,3], col='red')
#add legend
legend('topright', legend=c('prey', 'predator'), lty='solid', col=c('blue', 'red'), cex=0.6, bty='n')
#the phase-plane plot
plot(result[,2], result[,3], type='l',
     xlab='prey density', ylab='predator density')
LV_isoclines2(Parameters$r, Parameters$a, Parameters$mu, Parameters$K)

##2e)
J_matrix2 = function(r, a, mu, K) {
  out_matrix = matrix(c(-r*mu/(a*K), -mu, r-r*mu/(a*K), 0), nrow=2)
  return(out_matrix)
}
Jac_Matrix2 = J_matrix2(Parameters$r, Parameters$a, Parameters$mu, Parameters$K)
E2 = eigen(Jac_Matrix2)
E2$values 
#[1] -0.25+6.703544i -0.25-6.703544i both the real parts are negative, so the equilibrium is stable

