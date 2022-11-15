#Exercise 1.
# Script plotting the logistic growth function
# Key model parameters:
r0 = 1 # intrinsic growth rate
K = 100 # carrying capacity
n = seq(0,200)
dndt = r0 * n * (1 - n/K)
plot(n, dndt, type='l', xlab = 'n', ylab='dn/dt', ylim=c(-20,30))
lines(n,rep(0,201))

#Exercise 2.
par(mfrow=c(1,2))
n0 = 1
t = seq(0,20, by=0.1)
n = K / (1+ (K/n0-1)*exp(-r0*t))
plot(t,n, type='l', xlab='time', ylab='n', ylim=c(0,100))

#Exercise 3.
par(mfrow=c(1,1))
m = diff(n)/0.1 #n is the n in exercise2
lines(n[-1],m, col='blue')

#Exercise 4.
r0 = 1
x = 1 #x0
K = 100
t = 0
dt = 0.1
x_list = c(1)
t_list = c(0)
while (t<= 20) { #repeat calculating f(t,x)
  dxdt = r0 * x * (1 - x/K)
  x = x + dxdt*dt
  t = t + dt
  x_list = c(x_list, x)
  t_list = c(t_list, t)
}
lines(t_list, x_list, col='red')

#Exercise 5.
ode_solve = function(x0,t_end, dt,dxdt) {
  x = x0
  t = 0
  x_list = c(x0)
  t_list = c(0)
  while (t <= t_end) {
    y = dxdt(t,x)
    x = x + y * dt
    t = t + dt
    x_list = c(x_list, x)
    t_list = c(t_list, t)
  }
  result = list(t=t_list, x=x_list)
  return(result)
}
#test 1
dxdt = function(t,x) {
  y = 1 * x * (1 - x/100)
  return(y)
}
result = ode_solve(1,20,0.1,dxdt)
plot(result$t, result$x, type = 'l', xlab='time', ylab='x')
x_exact = 100 / (1+ (100/1-1)*exp(-1*result$t))
lines(result$t, x_exact, col='red')
#test 2
dxdt = function(t,x) {
  y = 0.1*x
  return(y)
}
result = ode_solve(1,20,0.1,dxdt)
plot(result$t, result$x, type = 'l', xlab='time', ylab='x')
x_exact = 1 * exp(0.1*result$t)
lines(result$t, x_exact, col='red')
#test 3 
dxdt = function(t,x) {
  y = cos(t) #be careful! it is the function based on t
  return(y)
}
result = ode_solve(0,20,0.1,dxdt)
plot(result$t, result$x, type = 'l', xlab='time', ylab='x')
x_exact = 0 + sin(result$t)
lines(result$t, x_exact, col='red')
