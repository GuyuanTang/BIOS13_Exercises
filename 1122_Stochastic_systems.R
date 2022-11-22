#Exercise: Random walk
randomWalk = function(N, tmax) {
  x = rep(0,tmax)
  #get an empty plot to start with
  plot(NA, type='n', xlim=c(0,tmax), ylim=c(-2*sqrt(tmax), 2*sqrt(tmax)))
  ends = rep(0, N) #record the endpoints
  for (iter in 1:N) {
    for (i in 1:(tmax-1)) {
      x[i+1] = x[i] + rnorm(1)
    }
    ends[iter] = x[tmax]
    #ploting the random walks
    lines(1:tmax, x, col='black')
  }
  #plot the endpoint of all random walks as a histogram
  hist(ends)
}

randomWalk(100, 500) #the tmax will affect the mean of the endpoint

#Exercise: A queue
runQ = function(Pin, Pout, T_queue) {
  #create the in and out vectors
  in_vec = c(0, 1)
  out_vec = c(-1, 0)
  #create an empty queue
  queue_len = rep(0, T_queue)
  for (t in 1:(T_queue-1)) {
    #sampling with probability
    q_num = sample(in_vec, 1, prob = c(1-Pin, Pin)) + sample(out_vec, 1, prob = c(Pout, 1-Pout))
    queue_len[t+1] = queue_len[t] + q_num
  }
  #plot the length of the queue over time
  plot(1:T_queue, queue_len, type = 'b',
       xlab = 'queue time(min)',
       ylab = 'length of the queue')
}

runQ(Pin=0.6, Pout=0.4, T_queue=120)

#Exercise: The Moran Process
runMoran = function(s, tmax) {
  #create the start population with only single copy of A
  pop = rep("B", s-1)
  pop[s] = "A"
  for (iter in 1:tmax) {
    num_vec = seq(1, s)
    i = sample(num_vec, 1)
    j = sample(num_vec[-i], 1)
    pop[i] = pop[j] #replace the individual with other copy
  }
  return(length(pop[pop=="A"])) #return the final number of A
}
runMoran(10, 20)

Pfixation = function(s, repeats) {
  n = 1 #to record the times for reaching the fixation
  repeat {
    fix_num = runMoran(s, repeats)
    if (fix_num == 6) {
      break
    }
    n = n+1
  }
  out_p = 1/n
  return(out_p)
}
Pfixation(10,100)

runMoran2 = function(s, tmax) {
  #create the start population with only single copy of A
  pop = rep("B", s-1)
  pop[s] = "A"
  num_vec = seq(1, s)
  weight_vec = c(rep(1,s-1), 1+s)
  #create a dataframe to contain all the information and their changes
  dat = data.frame(pop, weight_vec)
  for (iter in 1:tmax) {
    i = sample(num_vec, 1)
    j = sample(num_vec[-i], 1, prob = dat$weight_vec[-i])
    dat$pop[i] = dat$pop[j] #replace the individual with other copy
    dat$weight_vec[i] = dat$weight_vec[j]
  }
  return(length(dat$pop[dat$pop=="A"])) #return the final number of A
}
runMoran2(10,50)
