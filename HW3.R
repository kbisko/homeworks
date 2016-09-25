getwd()
setwd("//ihtech.com/fs01/home/alp2/KBiskobing/Desktop/School/MSA8190")

# 1
f<-function(x){
  y<-x
  pts<-length(x)
  for(i in 1:pts){
    ifelse(x[i]<(-2), y[i]<-0,
           ifelse(x[i]<2, y[i]<-0.25,
                  ifelse(x[i]>=2, y[i]<-1
                  )))
  }
  return(y)
}

integrate(f, lower=-2, upper=1.8)
integrate(f, lower=-1.4, upper=2)
integrate(f, lower=-2, upper=2)
integrate(f, lower=-1, upper=1)


# 2
D(expression(1-exp(-2*x)), 'x')

0
D(expression(0.2*x),'x')
D(expression(0.04*x+0.64),'x')
1

# 3 
a<-function(x){
  y<-x
  pts<-length(x)
  for(i in 1:pts){
    y[i]<-integrate(function(x) (1.5*x^2), lower=-1, upper=1)$val
  }
  return(y)
}
curve(a, from=-1, to=1, n=1001)
integrate(function(x) x*(1.5*x^2), lower=-1, upper=1)
integrate(function(x) x^2*(1.5*x^2), lower=-1, upper=1)

# 4
(1.5+2.2)/2
((2.2-1.5)^2)/12
(2-1.5)/(2.2-1.5)

0  
(x-1.5)/(0.7)
1

#5
qnorm(0.9)
qnorm(0.5)
qnorm(0.1, lower.tail=FALSE)
qnorm(0.9, lower.tail=FALSE)

qnorm(0.8+pnorm(-1.1))
pnorm(1.519376)-pnorm(-1.1) #validate answer

#6
qnorm(0.05)
qnorm(0.95)

qnorm(0.005)
qnorm(0.995)

qnorm(0.151)
qnorm(0.849)

qnorm(0.00135)
qnorm(0.99865)

#7
pnorm(12,12.4, 0.1)
pnorm(12.1,12.4, 0.1)+pnorm(12.6,12.1, 0.1, lower.tail=FALSE)
-2.575*0.1+12.4
2.575*0.1+12.4
#(12.1425, 12.6575)

#8
1-0.999
0.001*5000 #since =5 can't use normal approximation
pbinom(10, 5000, 0.001, lower.tail=FALSE)

#9
0.4*1000
ppois(350, 400, lower.tail=FALSE)
350-400/sqrt(400)
pnorm(330)

#10
pexp(10000, 0.0003, lower.tail=FALSE)
pexp(7000, 0.0003)

#11 ###?????
dexp(0, 2*(1/15))
pexp(1, (2/3)*(1/15), lower.tail=FALSE) 
pexp(1, (2/3)*(1/15))-pexp(1, (1/3)*(1/15))
qexp(0.05, 1/15)
qexp(0.05, 1/15, lower.tail=FALSE)
#(0.76 mins - 44.93 mins)

#12
30/60
#1 message every 2 seconds
5*2 #10 secs for a packet to form mean & variance
ppois(10, 10)
ppois(5, 10)

#13
100/20 #5 minutes
(80/20)-(50/20)
ppois(3, 5 ,lower.tail=FALSE)

#14
mu<-700*gamma(1+(1/2))
700^2*gamma(1+(2/2))-700^2*gamma(1+(1/2))^2
pweibull(mu,2,700,lower.tail=FALSE)

#15
#P(x>10)
plnorm(10, 0.5, 1, lower.tail=FALSE)
qlnorm(0.5, 0.5, 1)
exp((0.5+1)/2)
exp(2*0.5+1)*(exp(1)-1)
