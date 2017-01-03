Problem 1
Consider the following function:

x<-c(1, 1.5, 1.5, 2.5, 3)
y<-c(1, 2, 3, 4, 5)
fxy<-c(1/4, 1/8, 1/4, 1/4, 1/8)

(a) Show that fXY (x, y) satisfies the properties of a joint probability mass function.
Determine the following:
  1. fxy<0, fxy>1
  2. sum(fxy)
  3. ?????????
(b) P(X < 2.5, Y < 3)  ## fxy(<2.5, <3) ????
  1/4+1/8 
  sum(fxy[x<2.5 & y<3])
(c) P(X < 2.5)
  1/4+1/8+1/4 
  sum(fxy[x<2.5])
(d) P(Y < 3)
  1/4+1/8 
  sum(fxy[y<3])
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
  no, P(Y=y, X=1.5) = 1/3, 2/3 <> 1/8, 1/4 fy(y)
(k) Cov(x, y)
  mux<-sum(x*fxy)
  muy<-sum(y*fxy)
  covxy<-sum((x-mux)*(y-muy)*fxy)
(l) rho x,y
  sigmax<-sqrt(sum((x-mux)^2*fxy))
  sigmay<-sqrt(sum((y-muy)^2*fxy))
  rhoxy<-covxy/(sigmax*sigmay)  
  
Problem 2
Consider the joint probability mass function fXY (x, y) = c(x + y) over the nine points with
x = c(1, 2, 3, 1, 2, 3, 1, 2, 3) 
y = c(1, 1, 1, 2, 2, 2, 3, 3, 3)
fxy<-x+y
c<-1/sum(fxy)
fxy<-c*fxy
Determine
(a) c
  c<-1/36
(b) P(X = 1, Y < 4)
  fxy[1]+fxy[4]+fxy[7]
  sum(fxy[x==1 & y<4])
(c) P(X = 1)
  fxy[1]+fxy[4]+fxy[7]
  sum(fxy[x==1])
(d) P(Y = 2)
  fxy[4]+fxy[5]+fxy[6]
(e) P(X <= 2; Y <= 2)
  fxy[1]+fxy[2]+fxy[4]+fxy[5]
(f) E(X)
  ex<-sum(x*fxy)
(g) E(Y)
  ey<-sum(y*fxy)
(h) Var(X)
  varx<-sum(((x-ex)^2)*fxy)
(i) Var(Y)
  vary<-sum(((y-ey)^2)*fxy)  
(j) The marginal probability distribution of the random variable X.
  fx<-c((fxy[1]+fxy[4]+fxy[7]), (fxy[2]+fxy[5]+fxy[8]), (fxy[3]+fxy[6]+fxy[9]))
  fy<-c((fxy[1]+fxy[2]+fxy[3]), (fxy[4]+fxy[5]+fxy[6]), (fxy[7]+fxy[8]+fxy[9]))  
(k) The conditional probability distribution of Y given that X = 1.
  y_x1<-c(fxy[1]/fx[1], fxy[4]/fx[1], fxy[7]/fx[1])
(l) The conditional probability distribution of X given that Y = 2.
  c(fxy[4]/fy[2], fxy[5]/fy[2], fxy[6]/fy[2])
(m) E(Y|X = 1)
  sum(y_x1 * c(y[1], y[4], y[7]))
(n) Are X and Y independent?
  c((fx[1]*fy[1]),(fx[1]*fy[2]),(fx[1]*fy[3]),(fx[2]*fy[1]),(fx[2]*fy[2]),(fx[2]*fy[3]),(fx[3]*fy[1]),(fx[3]*fy[2]),(fx[3]*fy[3]))
  fxy
  no, fxy<>fx*fy for all x&y
(o) Cov(x, y)
  covxy<-sum((x-ex)*(y-ey)*fxy)
  
(p) rhox,y
  rhoxy<-covxy/sqrt(varx*vary)

Problem 3
A manufacturing company employs two inspecting devices to sample a fraction of their
output for quality control purposes. The First inspection monitor is able to accurately detect
99.3% of the defective items it receives, whereas the second is able to do so in 99.7% of the
cases. Assume that four defective items are produced and sent out for inspection. Let X
and Y denote the number of items that will be identiFied as defective by inspecting devices
1 and 2, respectively. Assume the devices are independent. 

a<-.993
b<-.997
n<-4
x<-c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4)
y<-c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4)
?pbinom
Determine
(a) fXY (x, y)
  fxy<-c()
  i=1
  j=1
  for (x[i] in x  ){
    fxy<-(dbinom(x,4,a)*dbinom(y,4,b))
    i=i+1
  }
  fxy
(b) fX(x)
  fx<-c()
  for fxy[i] in fxy{
    fx=c(fx, fxy[i]+fxy[i+1]+fxy[i+2]+fxy[i+3]+fxy[i+4])
    i=i+5
  }
  fx
(c) E(X)
  sum(x*fxy)
(d) fY|x=2(y)
  0.993^2*0.007^2*((0.003^4)+(0.997^1*0.003^3)+(0.997^2*0.003^2)+(0.997^3*0.003^1)+(0.997^4*0.003^0))
(e) E(Y|X = 2)
  independent so just E(Y)=sum(y*fxy)

(f) Var(Y|X = 2)
  independent so sum(y*fxy*)
  
(g) Are X and Y independent?
  yes
(h) Cov(x, y)
  0 since independent
(i) rhox,y
  0 since independent
  
Problem 4
Suppose the random variables X, Y , and Z have the following joint probability distribution:
x<-c(1, 1, 1, 1, 2, 2, 2, 2)
y<-c(1, 1, 2, 2, 1, 1, 2, 2)
z<-c(1, 2, 1, 2, 1, 2, 1, 2)
fxyz<-c(0.05, 0.10, 0.15, 0.20, 0.20, 0.15, 0.10, 0.05)

Determine the following:
(a) P(X = 2)
  fxyz[5]+fxyz[6]+fxyz[7]+fxyz[8]
  sum(fxyz[x==2])
(b) P(X = 1, Y = 2)
  fxyz[5]+fxyz[6]
  sum(fxyz[x==1&y==2])
(c) P(Z < 1.5)
  fxyz[1]+fxyz[3]+fxyz[5]+fxyz[7]
  sum(fxyz[z<1.5])
(d) P(X = 1 or Z = 2)
  sum(fxyz[x==1&z==2])
  fxyz[1]+fxyz[2]+fxyz[3]+fxyz[4]+fxyz[6]+fxyz[8]
(e) E(X)
  sum(x*fxyz)
(f) P(X = 1|Y = 1)
  (fxyz[1]+fxyz[2])/(fxyz[1]+fxyz[2]+fxyz[5]+fxyz[6])
  sum(fxyz[y==1&x==1])/sum(fxyz[y==1])
(g) P(X = 1, Y = 1|Z = 2)
   fxyz[2]/(fxyz[2]+fxyz[4]+fxyz[6]+fxyz[8])
   sum(fxyz[y==1&x==1&z==2])/sum(fxyz[z==2])
(h) P(X = 1|Y = 1,Z = 2)
  (fxyz[2])/(fxyz[2]+fxyz[6])
   sum(fxyz[y==1&x==1&z==2])/sum(fxyz[y==1&z==2])
(i) Conditional probability distribution of X given that Y = 1 and Z = 2.
   c(fxyz[2]/(fxyz[2]+fxyz[6]), fxyz[6]/(fxyz[2]+fxyz[6]))
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
manufacture of synthetic Fibers within the next three years. Assume the companies are
independent. Let X, Y , and Z denote the number of new competitors that will pose no,
moderate, and very high risk for the interested company, respectively.
n<-12
nr, mr, hr

(a) What is the range of the joint probability distribution of X, Y , and Z?
    (0-12, 0-12-x, 0-12-x-y)
(b) Determine P(X = 1, Y = 3, Z = 1).
    0, since the x+y+z=n
(c) Determine P(Z <= 2).
    pbinom(2,12,0.15)
    
    
Problem 6
Consider the joint probability density function fXY (x, y) = c(x+y) over the range 
0 < x < 3
x < y < x + 2
fxy<- function(x,y) (1/24)*(x+y)

Determine
(a) c
  c<-1/24
(b) P(X < 1, Y < 2)
  1/8
(c) P(1 < X < 2)
  
(d) P(Y > 1)

(e) P(X < 2, Y < 2)
  1/3
(f) E(X)
  45/24
(g) Marginal probability distribution of X
  integral from 0-3 (x/8)+(1/12)
(h) Conditional probability distribution of Y given that X = 1
  integral from 1-3 (1/24)+(1/24)y dy
(i) E(Y|X = 1)
  38/72
(j) P(Y > 2|X = 1)

(k) Conditional probability distribution of X given that Y = 2

(l) Cov(x, y)
  cov(x,y)
(m) rhox,y
  cor(x,y)
  
Problem 7
Suppose the random variables X, Y , and Z have the joint probability density function
fXYZ(x, y, z) = c over the cylinder x2 + y2 < 4 and 0 < z < 4. Determine the following.
(a) c

(b) P(X2 + Y 2 < 2)

(c) P(Z < 2)

(d) E(X)

(e) P(X < 1jY = 1)

(f) P(X2 + Y 2 < 1jZ = 1)

(g) Conditional probability distribution of Z given that X = 1 and Y = 1.

(h) Cov(x; y)

(i) Cov(x; z)

(j) Cov(y; z)

Problem 8
Let X and Y represent two dimensions of an injection molded part. Suppose X and Y have
a bivariate normal distribution with simgaX = 0.04, sigmaY = 0.08, muX = 3.00, muY = 7.70, and rho = 0.
Determine P(2.95 < X < 3.05, 7.60 < Y < 7.80).

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
