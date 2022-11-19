#Ploting exercises
plot(0:5, 0:5*2, pch = 9)

#Plot type
plot(0:5, 0:5*2, type='b')

#Line type
plot(0:5, 0:5*2, type='o', lty='dotdash')

#Plot with color
plot(0:5, 0:5*2, type='b', col='yellowgreen')
colors() #could view all the color names

#Add axis labels and a title
plot(0:5, 0:5*2, type='o', xlab='X-axis',
     ylab='Y-axis', main='Main title')

#Adding more on existed plot
plot(0:5, 0:5*2, type='b', col='violetred', lty='solid')
lines(0:5, 0:5*3, type='b', col='green', lty='dashed')

#Adding a legend
legend('topleft', legend = c('2 times x', '3 times x'),
       lty=c('solid','dashed'), pch=1, col = c('violetred', 'green'))

#Creating an empty plot
plot(NA, type='n', xlim=c(0,100), ylim=c(-2,2),
     xlab='your x-axis', ylab='your y-axis')

#Palettes
n = 32
palette(rainbow(n))
plot(NA,type='n',xlim=c(0,1),ylim=c(0,n),xlab='',ylab='')
for (i in 1:n) {
  lines(c(0,1), c(i,i), col=i, lwd=4)
}
palette('default')
