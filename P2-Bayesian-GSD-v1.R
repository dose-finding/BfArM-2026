############################################
### Practical 1
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

# Generating number of rresponses under the null in all nsims simulations trials
X<-rbinom(n=nsims,size=N,prob=p0)
# Finding posterior probability of (P[p>0.30])
post.prob<-pbeta(p0,shape1=X+1,shape2=N-X+1,lower.tail=F)
# Comparing the posterior probability found above to a pre-specified critical values
c<- # input several choices here and find
mean(post.prob>c) # estimate of the type I error

# Evaluate the charactersitics under the alterantive now
X<-rbinom(n=nsims,size=N,prob=p1)

post.prob<-pbeta(p0,shape1=X+1,shape2=N-X+1,lower.tail=F)

mean(post.prob>c) # estimate of power


###############   Point (b)  #################### 
# Two-stage design with the stopping for efficacy only

X1<-rbinom(n=nsims,size=N/2,prob=p0) # Stage 1 data generation
X2<-rbinom(n=nsims,size=N/2,prob=p0) # Stage 2 data generation
post.prob.1<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=F) # posterior probability at interim
post.prob.2<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F) # posterior probability at final

# Posterior probabiltiy is crossed at either of the analyses
# (type I error estimate across two analyses)
mean(post.prob.1>c | post.prob.2>c)

# Choose new threshold to control the type I error
c<- # input your value here
mean(post.prob.1>c | post.prob.2>c)


# Operating characteristics under the null
X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.1<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=F)
post.prob.2<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<- # input your value here
mean(post.prob.1>c | post.prob.2>c)


###############   Point (c)  #################### 
# Inclusion of the futility analysis based on the posterior probability
# (no efficacy early stop)

# Futility threshold of x%
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<- # input value from above
t<- # input value of x%
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


###############   Point (d)  #################### 
# Inclusion of the futility analysis based on the conditional power
# (no efficacy early stop)


# Find which number of responses (x) is needed at the final analysis to cross the efficacy bound 
# found in point (a)
pbeta(p0,shape1=x+1,shape2=N-x+1,lower.tail=F)


nsims<-10^5
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
CP<-c()
for(i in 1:nsims){
  CP[i]<-sum(dbinom(x=(18-X1[i]):20,size=20,prob=0.5))
}
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)


c<- # use the value of efficacy criterion found above
l<- # try different values of the futility criterion
mean(CP>l & post.prob.efficacy>c)
mean(CP<l)


###############   Point (e)  #################### 
# Inclusion of the futility analysis based on the predictive power
# (no efficacy early stop)

library("rmutil")

nsims<-10^5
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
PP<-c()
for(i in 1:nsims){
  PP[i]<-sum(dbetabinom((18-X1[i]):20, 20, (X1[i]+1)/(20+1+1), 20+1+1))
}
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)

c<-# use the value of efficacy criterion found above
l<- # try different values of the futility criterion
mean(PP>l & post.prob.efficacy>c)
mean(PP<l)


###############   Point (f)  #################### 
# Informative prior


###############   Point (g)  #################### 
# Various Sample Sizes