Problem 1
Consider the following function:

x<-c(1, 1.5, 1.5, 2.5, 3)
y<-c(1, 2, 3, 4, 5)
fxy<-c(1/4, 1/8, 1/4, 1/4, 1/8)

(a) Show that fXY (x, y) satisfies the properties of a joint probability mass function.
Determine the following:
  1. fxy<0, fxy>1
  2. sum(fxy)
  3. (fxy[1]/0.25)=1 & P(y=y,x=1)=1
     (fxy[2]/0.375+fxy[3]/0.375)=1 & P(y=y,x=1.5)=1
     (fxy[4]/0.25)=1 & P(y=y,x=2.5)=1
     (fxy[5]/0.125)=1 & P(y=y,x=3)=1
(b) P(X < 2.5, Y < 3) 
  1/4+1/8 
(c) P(X < 2.5)
  1/4+1/8+1/4 
(d) P(Y < 3)
  1/4+1/8 
(e) P(X > 1.8, Y > 4.7)
  1/8
(f) The marginal probability distribution of the random variable X.
  fx<-c(1/4, (1/8)+(1/4), 1/4, 1/8)
  fy<-fxy
(g) The conditional probability distribution of Y given that X = 1.5.
  (1/8)/0.375, (1/4)/0.375
(h) The conditional probability distribution of X given that Y = 2.
  1
(i) E(Y|X = 1.5)
  ((1/8)/0.375)*2+((1/4)/0.375)*3
(j) Are X and Y independent?
  no, P(Y=y, X=1.5) = (1/3, 2/3) <> (1/8, 1/4) fy(y)
(k) Cov(x, y)
  cov(x, y)
(l) rho x,y
  cor(x,y)
  
  
Problem 2
Consider the joint probability mass function fXY (x, y) = c(x + y) over the nine points with
x <- c(1, 2, 3, 1, 2, 3, 1, 2, 3) 
y <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
Determine
(a) c
  1=sum(x+y)*c
  sum(x+y)
  c<-1/36
  fxy<- 1/36*(x+y) 
(b) P(X = 1, Y < 4)
  fxy[1]+fxy[4]+fxy[7]
(c) P(X = 1)
  fxy[1]+fxy[4]+fxy[7]
(d) P(Y = 2)
  fxy[4]+fxy[5]+fxy[6]
(e) P(X <= 2; Y <= 2)
  fxy[1]+fxy[2]+fxy[4]+fxy[5]
(f) E(X)
  ex<-sum(x*fxy)
(g) E(Y)
  ey<-sum(y*fxy)
(h) Var(X)
  sum(((x-ex)^2)*fxy)
(i) Var(Y)
  sum(((y-ey)^2)*fxy)  
(j) The marginal probability distribution of the random variable X.
  fx<-c((fxy[1]+fxy[4]+fxy[7]), (fxy[2]+fxy[5]+fxy[8]), (fxy[3]+fxy[6]+fxy[9]))
  fy<-c((fxy[1]+fxy[2]+fxy[3]), (fxy[4]+fxy[5]+fxy[6]), (fxy[7]+fxy[8]+fxy[9]))  
(k) The conditional probability distribution of Y given that X = 1.
  y_x1<-c(fxy[1]/fx[1], fxy[4]/fx[1], fxy[7]/fx[1])
(l) The conditional probability distribution of X given that Y = 2.
  fxy[4]/fy[2], fxy[5]/fy[2], fxy[6]/fy[2]
(m) E(Y|X = 1)
  sum(y_x1 * c(y[1], y[4], y[7]))
(n) Are X and Y independent?
  c((fx[1]*fy[1]),(fx[1]*fy[2]),(fx[1]*fy[3]),(fx[2]*fy[1]),(fx[2]*fy[2]),(fx[2]*fy[3]),(fx[3]*fy[1]),(fx[3]*fy[2]),(fx[3]*fy[3]))
  fxy
  no, fxy<>fx*fy for all x&y
(o) Cov(x, y)
  cov(x,y)
(p) rhox,y
  cor(x,y)

Problem 3
A manufacturing company employs two inspecting devices to sample a fraction of their
output for quality control purposes. The rst inspection monitor is able to accurately detect
99.3% of the defective items it receives, whereas the second is able to do so in 99.7% of the
cases. Assume that four defective items are produced and sent out for inspection. Let X
and Y denote the number of items that will be identied as defective by inspecting devices
1 and 2, respectively. Assume the devices are independent. 

a<-.993
b<-.997
n<-4
x<-c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4)
y<-c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4)
Determine
(a) fXY (x, y)
  fxy<-list()
  for (i in 1:25)  {
    fxy[i]<-dbinom(x[i],4,a)*dbinom(y[i],4,b)
  }
  fxy

(b) fX(x)
  fx<-list()
  i=1
  j=0
  while (i<26) {
    fx[i-4*j]=(fxy[[i]]+fxy[[i+1]]+fxy[[i+2]]+fxy[[i+3]]+fxy[[i+4]])
    i=i+5
    j=j+1
  }
  fx

(c) E(X)
  ex<-0
  for (i in 1:25){
    ex<-ex+(fxy[[i]]*x[[i]]) 
  }
  ex

(d) fY|x=2(y)
  fyx2<-c(fxy[[11]]/fx[[3]], fxy[[12]]/fx[[3]], fxy[[13]]/fx[[3]], fxy[[14]]/fx[[3]], fxy[[15]]/fx[[3]])
  fyx2
  
(e) E(Y|X = 2)
  eyx2<-0
  for (i in 1:5){
    eyx2<-eyx2+fyx2[i]*y[i+10]
  }
  eyx2

(f) Var(Y|X = 2)
  vyx2<-0
  for (i in 1:5){
    vyx2<-vyx2+(fyx2[i]*(y[i+10])^2)
  }
  vyx2<-vyx2-(eyx2^2)
  vyx2

(g) Are X and Y independent?
  fy<-list()
  i=1
  while (i<6) {
    fy[i]=(fxy[[i]]+fxy[[i+5]]+fxy[[i+10]]+fxy[[i+15]]+fxy[[i+20]])
    i=i+1
  }
  fy
  
  k=1
  indep<-list()
  for (i in 1:5){
    for (j in 1:5){
      indep[k]<-(fx[[i]]*fy[[j]])
      k=k+1
    }
  }
  indep
  fxy
  yes, fxy=indep for all x&y

(h) Cov(x, y)
  cov(x,y)
(i) rhox,y
  cor(x,y)

Problem 4
Suppose the random variables X, Y , and Z have the following joint probability distribution:
x<-c(1, 1, 1, 1, 2, 2, 2, 2)
y<-c(1, 1, 2, 2, 1, 1, 2, 2)
z<-c(1, 2, 1, 2, 1, 2, 1, 2)
fxyz<-c(0.05, 0.10, 0.15, 0.20, 0.20, 0.15, 0.10, 0.05)

Determine the following:
(a) P(X = 2)
  fxyz[5]+fxyz[6]+fxyz[7]+fxyz[8]
(b) P(X = 1, Y = 2)
  fxyz[5]+fxyz[6]
(c) P(Z < 1.5)
  fxyz[1]+fxyz[3]+fxyz[5]+fxyz[7]
(d) P(X = 1 or Z = 2)
  fxyz[1]+fxyz[2]+fxyz[3]+fxyz[4]+fxyz[6]+fxyz[8]
(e) E(X)
  sum(x*fxyz)
(f) P(X = 1|Y = 1)
  (fxyz[1]+fxyz[2])/(fxyz[1]+fxyz[2]+fxyz[5]+fxyz[6])
(g) P(X = 1, Y = 1|Z = 2)
   fxyz[2]/(fxyz[2]+fxyz[4]+fxyz[6]+fxyz[8])
(h) P(X = 1|Y = 1,Z = 2)
  (fxyz[2])/(fxyz[2]+fxyz[6])
(i) Conditional probability distribution of X given that Y = 1 and Z = 2.
   fxyz[2]/(fxyz[2]+fxyz[6]), fxyz[6]/(fxyz[2]+fxyz[6])
(j) Cov(x, y)
  cov(x,y)
(k) Cov(x, z)
  cov(x,z)
(l) Cov(y, z)
  cov(y,z)

Problem 5
A marketing company performed a risk analysis for a manufacturer of synthetic fibers and
concluded that new competitors present no risk 13% of the time (due mostly to the diversity of fibers manufactured)
, moderate risk 72% of the time (some overlapping of products),
and very high risk (competitor manufactures the exact same products) 15% of the time.
It is known that 12 international companies are planning to open new facilities for the
manufacture of synthetic fibers within the next three years. Assume the companies are
independent. Let X, Y , and Z denote the number of new competitors that will pose no,
moderate, and very high risk for the interested company, respectively.
n<-12
nr<-.13
mr<-.72
hr<-.15
Determine
(a) What is the range of the joint probability distribution of X, Y , and Z?
    (0-1)
(b) Determine P(X = 1, Y = 3, Z = 1).
    0, since the x+y+z=n
(c) Determine P(Z <= 2).
    pbinom(2,12,0.15)
    
Problem 6
Consider the joint probability density function fXY (x, y) = c(x+y) over the range 
0 < x < 3
x < y < x + 2

Determine
(a) c
  c<-1/24 #by integrating over x & y, solving for fxy(x,y)=1
  fxy<- function(x,y) (1/24)*(x+y) 
(b) P(X < 1, Y < 2)
  1/8 ## show integral?
(c) P(1 < X < 2) 
  1/3 ## show integral?
(d) P(Y > 1)
  5/6 ## show integral?
(e) P(X < 2, Y < 2)
  1/3 ## show integral?
(f) E(X)
  ex<-45/24
  ex #1.875 ## show integral?
(g) Marginal probability distribution of x
  (x/8)+(1/12) ## show integral?
  (1/24)*(16/7) (1/12)*(16/7)  24*7
(h) Conditional probability distribution of Y given that X = 1 fy|x=1(y)/fx(1) 
   f(1,y)/fx(1)=1/5+1/5y
(i) E(Y|X = 1) 
  integral from 1 to 3 of y/5+y^2/5 = (8/10)+(26/15)  ## show integral? 
(j) P(Y > 2|X = 1)
  0.1458
(k) Conditional probability distribution of X given that Y = 2 
  f(x,2)/fy(2)=2x/21+4/21
(l) Cov(x, y)
  ey<-2.958
  cov(x,y) = E(xy)-Ex*Ey
  cov<-6.125-(ex*ey)
(m) rhox,y 
  cor(x,y) = Cov/sigmax*sigmay
  
Problem 7
Suppose the random variables X, Y , and Z have the joint probability density function
fXYZ(x, y, z) = c over the cylinder x2 + y2 < 4 and 0 < z < 4. Determine the following.
  x=rho*cos(phi) 
  y=rho*sin(phi)
  rho=sqrt(x^2+y^2) ... 0 < rho < 2 
  phi  0, 2z
  frpz(rho, phi, z)
(a) c
  c=1/32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
(b) P(X2 + Y2 < 2)
  
(c) P(Z < 2)
  1/4
(d) E(X)
  
(e) P(X < 1|Y = 1)
 
(f) P(X2 + Y2 < 1|Z = 1)

(g) Conditional probability distribution of Z given that X = 1 and Y = 1

(h) Cov(x; y)

(i) Cov(x; z)

(j) Cov(y; z)

Problem 8
Let X and Y represent two dimensions of an injection molded part. Suppose X and Y have
a bivariate normal distribution with simgaX = 0.04, sigmaY = 0.08, muX = 3.00, muY = 7.70, and rho = 0.
Determine P(2.95 < X < 3.05, 7.60 < Y < 7.80).

mu<-c(3.00, 7.70)
sigma<-matrix(c(0.04^2, 0, 0, 0.08^2))
install.packages("mvtnorm")
library("mvtnorm")
pmvnorm(lower=c(2.95, 7.60), upper=c(3.05, 7.80), mean=mu, sigma=sigma)
?pmvnorm

Problem 9
A U-shaped component is to be formed from the three parts A, B, and C. The picture is
shown in Fig. 5-20. The length of A is normally distributed with a mean of 10 millimeters
and a standard deviation of 0.1 millimeter. The thickness of parts B and C is normally
distributed with a mean of 2 millimeters and a standard deviation of 0.05 millimeter. Assume
all dimensions are independent.
mua<-10
sda<-0.1
mub<-2
sdb<-0.05

(a) Determine the mean and standard deviation of the length of the gap D. #length of A minus length of B and C
mud<-mua-2*mub
sdd<-sda+2*sdb
mud
sdd
(b) What is the probability that the gap D is less than 5.9 millimeters?
pnorm(5.9, mud, sdd)
