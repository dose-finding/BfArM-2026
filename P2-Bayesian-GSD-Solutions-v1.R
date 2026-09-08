############################################
### Practical 1 (Solutions)
############################################

# Uploading packages
# install.packages("rmutil")
library("rmutil")

# Setting the parameters
N<-40 # total number of patients
p0<-0.30 # response rate under the null
p1<-0.50 # response rate under the alterantive
nsims<-10^5 # Number of simulations

###############   Point (a)  #################### 
# Fixed-sample (non-adaptive design)

X<-rbinom(n=nsims,size=N,prob=p0)

post.prob<-pbeta(p0,shape1=X+1,shape2=N-X+1,lower.tail=F)

c<-0.959
mean(post.prob>c)

c<-0.958
mean(post.prob>c)


X<-rbinom(n=nsims,size=N,prob=p1)

post.prob<-pbeta(p0,shape1=X+1,shape2=N-X+1,lower.tail=F)

c<-0.959
mean(post.prob>c)


###############   Point (b)  #################### 
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.1<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=F)
post.prob.2<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.959
mean(post.prob.1>c | post.prob.2>c)

c<-0.974
mean(post.prob.1>c | post.prob.2>c)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.1<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=F)
post.prob.2<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.974
mean(post.prob.1>c | post.prob.2>c)


###############   Point (c)  #################### 

# 75%
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.959
t<-0.75
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


# 50%
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.959
t<-0.50
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


# 25%
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.959
t<-0.25
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)



###############   Point (d)  #################### 
pbeta(p0,shape1=17+1,shape2=N-17+1,lower.tail=F)
pbeta(p0,shape1=18+1,shape2=N-18+1,lower.tail=F)


nsims<-10^5
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
CP<-c()
for(i in 1:nsims){
  CP[i]<-sum(dbinom(x=(18-X1[i]):20,size=20,prob=0.5))
}
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)


c<-0.959
l<-0.20
mean(CP>l & post.prob.efficacy>c)
mean(CP<l)

X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
CP<-c()
for(i in 1:nsims){
  CP[i]<-sum(dbinom(x=(18-X1[i]):20,size=20,prob=0.5))
}
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
mean(CP>l & post.prob.efficacy>c)
mean(CP<l)




###############   Point (e)  #################### 
library("rmutil")

nsims<-10^5
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
PP<-c()
for(i in 1:nsims){
  PP[i]<-sum(dbetabinom((18-X1[i]):20, 20, (X1[i]+1)/(20+1+1), 20+1+1))
}
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)

c<-0.959
l<-0.01
mean(PP>l & post.prob.efficacy>c)
mean(PP<l)


nsims<-10^5
X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
PP<-c()
for(i in 1:nsims){
  PP[i]<-sum(dbetabinom((18-X1[i]):20, 20, (X1[i]+1)/(20+1+1), 20+1+1))
}
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)

c<-0.959
l<-0.01
mean(PP>l & post.prob.efficacy>c)
mean(PP<l)


###############   Point (f)  #################### 
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.futility<-pbeta(p0,shape1=X1+3.2,shape2=N/2-X1+(7.5-3.2),lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+3.2,shape2=N-X1-X2+(7.5-3.2),lower.tail=F)
c<-0.959
t<-0.50
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.futility<-pbeta(p0,shape1=X1+3.2,shape2=N/2-X1+(7.5-3.2),lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+3.2,shape2=N-X1-X2+(7.5-3.2),lower.tail=F)
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)



X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.futility<-pbeta(p0,shape1=X1+3.2,shape2=N/2-X1+(7.5-3.2),lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+3.2,shape2=N-X1-X2+(7.5-3.2),lower.tail=F)
c<-0.965
t<-0.50
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.futility<-pbeta(p0,shape1=X1+3.2,shape2=N/2-X1+(7.5-3.2),lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+3.2,shape2=N-X1-X2+(7.5-3.2),lower.tail=F)
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)

###############   Point (g)  #################### 
# Different sample size
N<-38
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.1<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=F)
post.prob.2<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.959
mean(post.prob.1>c | post.prob.2>c)

c<-0.976
mean(post.prob.1>c | post.prob.2>c)



N<-42
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.1<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=F)
post.prob.2<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.959
mean(post.prob.1>c | post.prob.2>c)

c<-0.966
mean(post.prob.1>c | post.prob.2>c)


###############   Point (h)  #################### 
# different null
N<-40
p0<-0.15
X<-rbinom(n=nsims,size=N,prob=p0)

post.prob<-pbeta(p0,shape1=X+1,shape2=N-X+1,lower.tail=F)

c<-0.959
mean(post.prob>c)

c<-0.965
mean(post.prob>c)



#different null
N<-40
p0<-0.50
X<-rbinom(n=nsims,size=N,prob=p0)

post.prob<-pbeta(p0,shape1=X+1,shape2=N-X+1,lower.tail=F)

c<-0.959
mean(post.prob>c)

c<-0.965
mean(post.prob>c)
