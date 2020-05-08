###(1) random number generator with two modulus###
IM1<- 2147483563 
IM2<- 2147483399
AM<- (1.0/IM1) 
IMM1<- (IM1-1)
IA1<- 40014 
IA2<- 40692
IQ1<- 53668 
IQ2<- 52774 
IR1<- 12211 
IR2<- 3791 
NTAB<- 32
NDIV<- (1+IMM1/NTAB) 
EPS<- 1.2e-7 
RNMX<- (1.0-EPS)
idum<- -10
idum2<- 123456789
iy<- 0
iv<- rep(0,NTAB)

ran2 <- function(idum,n=1){
  random<-rep(0,n)
  for(i in 1:n){
    if(idum<=0){
      idum<- max(-idum,1)
      idum2=idum
      j<-NTAB+8
      while(j>0){
        k=as.integer(idum/IQ1)
        idum<-IA1*(idum-k*IQ1)-k*IR1
        if(idum<0) {idum = idum + IM1}
        if(j <= NTAB) {iv[j] <- idum}
        j<- j-1
      }
      iy=iv[1]
    }
    k=as.integer(idum/IQ1)
    idum=IA1*(idum-k*IQ1)-k*IR1
    if(idum<0) {idum = idum + IM1}
    k=as.integer(idum2/IQ2)
    idum2=IA2*(idum2-k*IQ2)-k*IR2
    if(idum2<0) {idum2 = idum2 + IM2}
    j=as.integer(iy/NDIV)+1
    iy=iv[j]-idum2
    iv[j] = idum
    if(iy<1) {iy = iy + IMM1}
    if(AM*iy<RNMX){random[i]<-AM*iy}
    else {random[i]<-RNMX}
  }
  return(random)
}
#100 random numbers between 1 and 1000
random1_1000<- 1+999*ran2(idum,100)
hist(random1_1000[1:100])

#10000 random numbers between 0 and 1
random0_1<- ran2(idum,10000)
hist(random0_1[1:10000])

### (2) Roulette ###
#fair roulette
rlt36<-floor(1+ran2(idum,10000)*36) #10000 integers >=1, <=36
hist(rlt36,breaks =c(0:36)) #uniform distribution

P_L<- function(rlt,n){ #roulette P&L
  PL<-rep(0,n)
  for(i in 1:n){ #if 36 is drawn, get a profit of 35
    if(rlt[i]==36) PL[i] = 35 
    else PL[i] = -1
  }
  return(PL)
}

rlt36_10<-floor(1+ran2(idum,10)*36)
PL10<-P_L(rlt36_10,10)
exp10<-mean(PL10)
std10<-sd(PL10)

rlt36_100<-floor(1+ran2(idum,100)*36)
PL100<-P_L(rlt36_100,100)
exp100<-mean(PL100)
std100<-sd(PL100)

rlt36_1000<-floor(1+ran2(idum,1000)*36)
PL1000<-P_L(rlt36_1000,1000)
exp1000<-mean(PL1000)
std1000<-sd(PL1000)

#unfair roulette
rlt37<-floor(ran2(idum,10000)*37) #10000 integers >=0, <=36
hist(rlt37,breaks=c(-1:36))#uniform distribution

rlt37_10<-floor(ran2(idum,10)*37)
unPL10<-P_L(rlt37_10,10)
unexp10<-mean(unPL10)
unstd10<-sd(unPL10)

rlt37_100<-floor(ran2(idum,100)*37)
unPL100<-P_L(rlt37_100,100)
unexp100<-mean(unPL100)
unstd100<-sd(unPL100)

rlt37_1000<-floor(ran2(idum,1000)*37)
unPL1000<-P_L(rlt37_1000,1000)
unexp1000<-mean(unPL1000)
unstd1000<-sd(unPL1000)

###(3) draws from a normal distribution###
a0=2.50662823884
a1=-18.61500062529
a2=41.39119773534
a3=-25.44106049637
b0=-8.47351093090
b1=23.08336743743
b2=-21.06224101826
b3=3.13082909833
c0=0.3374754822726147
c1=0.9761690190917186
c2=0.1607979714918209
c3=0.0276438810333863
c4=0.0038405729373609
c5=0.0003951896511919
c6=0.0000321767881768
c7=0.0000002888167364
c8=0.0000003960315187


u<-ran2(idum,10000)
invnor<- function(uni,n){
  invnor<-rep(0,n)
  for(i in 1:n){
    y=uni[i]-0.5
    if(abs(y)<0.42){
      r<-y*y
      x<-y*(((a3*r+a2)*r+a1)*r+a0)/((((b3*r+b2)*r+b1)*r+b0)*r+1)
    }
    else{
      r<-uni[i]
      if(y>0) r<- 1-uni[i]
      r<- log(-log(r))
      x<- c0+r*(c1+r*(c2+r*(c3+r*(c4+r*(c5+r*(c6+r*(c7+r*c8)))))))
      if(y<0) x<- -x
    }
    invnor[i]<- x
  }
  return(invnor)
}

Gaus<-invnor(u,10000)
hist(Gaus)


###(4) Option Price ###
#I choose X as the stock, data range: 3/27/2018 - 3/27/2019
X <- read.csv(file="/Users/xuan/Downloads/X.csv", header=TRUE, sep=",")
da<-X[,6]
n <- length(da)
return<-rep(0,n-1)
for(i in 1:n-1){return[i] = log(da[i+1]/da[i])}

S0<-19.15
K<-19
vol<-sd(return)*sqrt(251)
rf<-0.0246
t <- as.numeric(as.Date('2019/11/15','%Y/%m/%d')-as.Date('2019/03/27','%Y/%m/%d'))/365

Div_Date <- '2019/08/08'
D<- 0.05
d<-as.numeric(as.Date(Div_Date,'%Y/%m/%d')-as.Date(Today,'%Y/%m/%d'))/365

D<-D*exp(-rf*d)
S <- S0-D

BS_C<-function(S,K,vol,t,r){ #using Black-Scholes model to get the call price
  d1=(log(S/K)+(r+0.5*vol*vol)*t)/(vol*sqrt(t))
  d2=d1-vol*sqrt(t)
  C = S*pnorm(d1)-K*exp(-r*t)*pnorm(d2)
  return(C)
}

BS_P<-function(S,K,vol,t,r){
  d1=(log(S/K)+(r+0.5*vol*vol)*t)/(vol*sqrt(t))
  d2=d1-vol*sqrt(t)
  P = K*exp(-r*t)*pnorm(-d2)-S*pnorm(-d1)
  return(P)
}

BS_C(S,K,vol,t,rf) #2.720664
BS_P(S,K,vol,t,rf) #2.324627
#Last Price: Call = 2.62, Put = 2.46
#The prices from Black-Scholes model are close to last traded price.

###Monte-Carlo Call###

count<-1
numran<-100000
Opt<-0
St<-rep(0,numran)
Ret<-rep(0,numran)
Option<-rep(0,numran)
ran<-ran2(idum,100000)
rannorm<-invnor(ran,100000)
while(count<=numran){
  ST<-S*(exp((rf-0.5*(vol^2))*t+vol*sqrt(t)*rannorm[count]))
  St[count]<-ST
  Ret[count]<-log(ST/S0)
  Payoff<- ST-K
  Option[count]<-0
  if(Payoff>0) {
    Option[count]<-exp(-rf*t)*Payoff
  }
  count = count +1       
}

mean(Option[1:100000]) #2.722516

###Monte-Carlo Put###
count<-1
while(count<=numran){
  ST<-S*(exp((rf-0.5*(vol^2))*t+vol*sqrt(t)*rannorm[count]))
  St[count]<-ST
  Ret[count]<-log(ST/S0)
  Payoff<- K-ST
  Option[count]<-0
  if(Payoff>0) {
    Option[count]<-exp(-rf*t)*Payoff
  }
  count = count +1       
}

mean(Option[1:100000]) #2.331159
#The simulated prices matche both the traded prices and the Black-Scholes prices.















