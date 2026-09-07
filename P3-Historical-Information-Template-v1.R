#########################################################################################
# Practical 3 - Information Borrowing (using an example of historical information borrowing)
#########################################################################################


#### CLEAN WORKING ENVIRONMENT ##########################################################

rm(list=setdiff(ls(), c()))


#### WORKING DIRECTORY ##################################################################

set.seed(567)

#### IMPORT LIBRARIES  ###################################################################

library(bayesplot)
library(ggplot2)
library(RBesT)

#############################################################
## Import data
#############################################################

dat = crohn
crohn_sigma = 88
dat$y.se = crohn_sigma/sqrt(dat$n)

attach(dat)
print(dat)

#############################################################
## Point 1: Derivation of the MAP Prior
#############################################################

map_mcmc = gMAP(cbind(y, y.se) ~ 1 | study, 
                weights=n,data=dat,
                family=gaussian,
                beta.prior=cbind(0, crohn_sigma),
                tau.dist="HalfNormal",tau.prior=cbind(0,crohn_sigma/2))

## Outputs
summary(map_mcmc)
forest_plot(map_mcmc) 


# Approximation of MAP Prior using a Mixture Distribution
map = automixfit(map_mcmc)
map
summary(map)
plot(map)$mix

# Effective Sample size
ess(map, method="moment")
ess(map, method="morita")
ess(map, method="elir")

# Robustification of MAP Prior
rmap  = robustify(map, weight=0.2, mean=-50)
rmap
summary(rmap)
plot(rmap)

ess(rmap, method="elir")

##########################################################################
## Point (2) and Point (3): Analysis with Current Data and Robust MAP prior for control
##########################################################################

## Study aggregated data
y.act = -100
n.act = 39
y.act.se = crohn_sigma/sqrt(n.act)
# Estimates with current data only
y.act
round(y.act+qnorm(0.975)*y.act.se,2)
round(y.act-qnorm(0.975)*y.act.se,2)

y.pbo = -60
n.pbo = 19
y.pbo.se = crohn_sigma/sqrt(n.pbo)
# Estimates with Current Data only
y.pbo
round(y.pbo+qnorm(0.975)*y.pbo.se,2)
round(y.pbo-qnorm(0.975)*y.pbo.se,2)


# Prior for active group
weak_prior = mixnorm(c(1,-50,1), sigma=crohn_sigma, param = 'mn')

# Treatment effect with current data only
diff=y.act-y.pbo
se_diff=sqrt(y.act.se^2 + y.pbo.se^2)
diff
round(diff+qnorm(0.975)*se_diff,2)
round(diff-qnorm(0.975)*se_diff,2)

pt(diff/se_diff,df=39+19-2)

## Posterior distributions
post_act = postmix(weak_prior, m=y.act, se=y.act.se)
summary(post_act)
post_pbo = postmix(rmap, m=y.pbo, se=y.pbo.se)
summary(post_pbo)
# Treatment effect
mean(rmixdiff(post_act, post_pbo,10^6))
qmixdiff(post_act, post_pbo, c(0.5, 0.025, 0.975))

## then calculate probability for the criteria
## and compare to the predefined threshold values
p = pmixdiff(post_act, post_pbo, 0)
round(p*100,1)
print(p>0.975)

# Plot posterior distribution in each arm
nsim=100000
post_pbo_sample=rmix(post_pbo, nsim)
post_act_sample=rmix(post_act, nsim)

par(mar=c(4, 4, 1, 1))
plot(density(post_pbo_sample), lwd=3, xlim=c(-200, 100), ylim=c(0,0.04), xlab="Effect", col="red", main="")
lines(density(post_act_sample), lwd=3, col="forestgreen")
legend("topright", c("Placebo", "Active"), col=c("red","forestgreen"), 
       lwd=c(3, 3, 3, 3),  bty="n", cex=0.8)

# Plot Posteror, MAP prior and study data for the placebo arm
samples1=rmix(post_pbo, 1E6)
samples2=rmix(map, 1E6)
samples3=rnorm(1E6, mean=y.pbo, sd=y.pbo.se)

par(mar=c(4, 4, 1, 1))
plot(density(samples1), lwd=3, xlim=c(-200, 100), ylim=c(0,0.04), xlab="Placebo effect", col="red", main="")
lines(density(samples2), lwd=3, col="black")
lines(density(samples3), lwd=3, col="blue")
legend("topright", c("Posterior distribution", "MAP", "Study data"), col=c("red","black", "blue"), 
       lwd=c(3, 3, 3, 3),  bty="n", cex=0.8)



##########################################################################
##  Operating characteristics
##########################################################################

##########################################################################
##  Type 1 error
##########################################################################


poc = decision2S(pc=c(0.975), qc=c(0), lower.tail=TRUE)
print(poc)

weak_prior = mixnorm(c(1,-50,1), sigma=crohn_sigma, param = 'mn')

n_act = 40
n_pbo = 20

design_noprior = oc2S(weak_prior, weak_prior, n_act, n_pbo, poc,
                         sigma1=crohn_sigma, sigma2=crohn_sigma)
design_rob = oc2S(weak_prior, rmap, n_act, n_pbo, poc,
                     sigma1=crohn_sigma, sigma2=crohn_sigma)

# the range for true values
cfb_truth = seq(-120, -40, by=1)
typeI1 = design_noprior(cfb_truth, cfb_truth)
typeI2 = design_rob(cfb_truth, cfb_truth)

par(mar=c(4, 4, 1, 1))

plot(cfb_truth, typeI1, type='l', col="blue", lwd=3, ylim=c(0, 0.2), 
     xlab=expression(paste('True value of change from baseline ', mu[act] == mu[pbo])),
     ylab="Type I error")
lines(cfb_truth, typeI2, type='l', col="gold", lwd=3)

legend("topright", c("40:20 with weak priors",
                     "40:20 with robust MAP prior for placebo"), 
       col=c("blue", "gold"), lty=c(1, 1), lwd=c(3, 3), bty="n")
