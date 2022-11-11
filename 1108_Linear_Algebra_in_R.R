#Exercise 1
u = c(3,6,7)
v = c(12,13,14)
## a)
u+v #[15,19,21]
## b)
u %*% v #212
## c)
sqrt(u%*%u) #9.69536
## d)
sqrt(v%*%v) #22.56103
## e)
sqrt((u+v)%*%(u+v)) #32.0468

#Exercise 2
B = array(c(1,2,3,0,0,0,4,5,6),dim = c(3,3))
## a)
B%*%u
## b)
B%*%v
## c)
B %*% (u+v)
## d)
B %*% B %*% u

#Exercise 3
X = array(c(1,1,2,0), dim = c(2,2))
## a)
det(X) #-2
## b)
E = eigen(X)
E$values
## c)
det(X) == E$values[1] * E$values[2] #TRUE
## d)
E$vectors
v1 = E$vectors[,1]
v2 = E$vectors[,2]
## e)
X %*% v1 == E$values[1] * v1 #TRUE
X %*% v2 == E$values[2] * v2 #TRUE

#Exercise 4
I = matrix(c(1,0,0,0,1,0,0,0,1), 3,3)
v3 = matrix(c(8,2,-6),3,1)
I %*% v3 == v3 #TRUE

#Exercise 5
EI = eigen(I)
EI$values # 1 1 1

#Exercise 6
I = matrix(c(1,0,0,1),2,2)
va = matrix(c(12,8),2,1)
vb = matrix(c(6,4),2,1)
0.5 * I %*% va == vb #TRUE

#Exercise 7
scal_EI = 0.5*I
EIS = eigen(scal_EI)
EIS$values == 0.5 #TRUE TRUE

#Exercise 8
draw_a_tree = function() {
  x = c(0,0,0.7,1.5,NA,0.7,0.8,NA,0,-0.6,-1.2,NA,-0.6,-0.5)
  y = c(0,1,1.3,1.4,NA,1.3,1.8,NA,1.0,1.4,1.7,NA,1.4,2.0)
  plot(x,y,type = 'l', col='brown', lwd=8)
}
draw_a_tree()

#Exercise 9
draw_a_small_tree = function() {
  draw_a_tree()
  x1 = 0.5 * c(0,0,0.7,1.5,NA,0.7,0.8,NA,0,-0.6,-1.2,NA,-0.6,-0.5)
  y1 = 0.5 * c(0,1,1.3,1.4,NA,1.3,1.8,NA,1.0,1.4,1.7,NA,1.4,2.0)
  lines(x1,y1,col='green',lwd=8)
}
draw_a_small_tree()

#Exercise 10
draw_a_tree()
draw_a_small_tree()

#Exercise 12
draw_a_tree = function() {
  x = c(0,0,0.7,1.5,NA,0.7,0.8,NA,0,-0.6,-1.2,NA,-0.6,-0.5)
  y = c(0,1,1.3,1.4,NA,1.3,1.8,NA,1.0,1.4,1.7,NA,1.4,2.0)
  plot(x,y,type = 'l', col='brown', lwd=8, xlim=c(-2,2), ylim=c(-2,2))
}
