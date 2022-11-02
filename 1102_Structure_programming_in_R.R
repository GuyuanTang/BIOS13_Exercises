#Exercise 1
mymax <- function(a,b) {
  a = scan(nmax=1)
  b = scan(nmax=1)
  if (a>b) {
    return(a)
  } else {
    return(b)
  }
}
cat("The largest value is ",mymax())

#Exercise 2
#input the vectors
num_count = as.numeric(readline("Please enter the length of the vector"))
v = scan(nmax=num_count)
w = scan(nmax=num_count)
#define the function
weight_count = function(v,w) {
  weighted_v = v*w
  sum_w_v = sum(weighted_v)
  return(sum_w_v)
}
cat(weight_count(v,w))

#Exercise 3
fiboseq <- function(n) {
  n = as.numeric(readline("Enter the number of the Fibonacci"))
  fibo = NULL
  if (n==1) {
    fibo[1] = 1
    cat(fibo)
    plot(1:n, fibo)
  } 
  else if (n==2) {
    fibo = c(1,1)
    cat(fibo)
    plot(1:n, fibo)
  }
  else {
    fibo[1] = 1
    fibo[2] = 1
    for (i in 3:n) {
      fibo[i] = fibo[i-2] + fibo[i-1]
    }
    cat(fibo)
    plot(1:n,fibo)
  }
}
fiboseq()
#Extend the function above
fiboseq_gold <- function(n) {
  n = as.numeric(readline("Enter the number of the Fibonacci"))
  fibo = NULL
  gold_ratio = (1+sqrt(5))/2
  gold = NULL
  if (n==1) {
    fibo[1] = 1
    gold[1] = gold_ratio/sqrt(5)
    cat(fibo)
    plot(1:n, fibo, col='red')
    points(1:n, gold, col='blue')
  } 
  else if (n==2) {
    fibo = c(1,1)
    gold = rep(gold_ratio/sqrt(5), 2)
    cat(fibo)
    plot(1:n, fibo, col='red')
    points(1:n, gold, col='blue')
  }
  else {
    fibo[1] = 1
    fibo[2] = 1
    for (i in 3:n) {
      fibo[i] = fibo[i-2] + fibo[i-1]
      gold[i] = gold_ratio^i/sqrt(5)
    }
    cat(fibo)
    plot(1:n,fibo, col='red')
    points(1:n, gold, col='blue')
  }
}
fiboseq_gold()

#Exercise 4
plotmax = function(n) {
  v = runif(n)
  plot(v, type='l', col='blue')
  max_v = NULL
  max_position = NULL
  for (i in 1:length(v)) {
    if (i==1 && v[1]>v[2]) {
      points(i,v[i], pch=4, col='red')
      max_v = c(max_v,v[i])
      max_position = c(max_position, i)
    } else if (i > 1 && i<length(v)) { #from 2 to length(v)-1
      if (v[i-1]<v[i] && v[i]>v[i+1]) {
        points(i,v[i], pch=4, col='red')
        max_v = c(max_v,v[i])
        max_position = c(max_position, i)
      } 
    } else if (i==length(v)) { #check for the last point, now i=length(v)
      if (v[i]>v[i-1]) {
        points(i, v[i], pch=4, col='red')
        max_v = c(max_v,v[i])
        max_position = c(max_position, i)
      }
    }
  }
  outlist = list(max_v=max_v, max_position=max_position)
  return(outlist)
}

n = as.numeric(readline('Please enter the length:'))
plotmax(n)
  
#Exercise 5
is_palindrome = function(my_str) {
  len = nchar(my_str)
  for (i in 1:len) {
    if (substr(my_str,i,i) == substr(my_str,len-i+1, len-i+1)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
my_str = readline("Please enter a string")
is_palindrome(my_str)

#Exercise 6
n_v1 = as.numeric(readline('Enter the length of vector 1'))
v1 = scan(nmax = n_v1)
n_v2 = as.numeric(readline('Enter the length of vector 2'))
v2 = scan(nmax=n_v2)
merge_vector = function(v1, v2) {
  #sorting two vectors
  v1 = sort(v1)
  v2 = sort(v2)
  cat("sorted v1:", v1, '\nsorted v2:', v2)
  
  #create a new vector to contain the final sorted one
  v_final = c(v1, v2)
  v_final = sort(v_final)
  cat("\n") #adjust the printing format
  return(v_final)
}
merge_vector(v1, v2)

#Exercise 7
op = par(mfrow=c(1,1)) #set the default value
par(mfrow = c(2,2)) #print 2 plots at one row
par(op) #return to the old default setting
