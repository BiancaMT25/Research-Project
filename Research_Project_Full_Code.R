library(nnet)
library(e1071)
library(latentnet)
library(MEclustnet)
library(ergm)

# Any figures/tables/results found in the paper are ordered and labeled 
# according to where they are seen in the paper

## SECTION 2: METHODOLOGY ##################################################

## Subsection 2.3: Data Collection #########################################

# Note: Methods to collect the data are in seperate file

# Load the data in:

women_gov <- read.delim("~/women_gov.txt") # covariate data
women_adj <- read.table("~/women_adj.txt", quote="\"", comment.char="") # adjacency matrix
women_adj <- as.matrix(women_adj) # store as matrix

### Figure 1: Image of the Data ########

par(mar=c(5, 4, 4, 2) + 0.1)
women_ordered <- women_gov[order(women_gov$name),]  # order by party
women_ordered <- women_gov[order(women_gov$Party),] # order aphabetically

indexes <- as.numeric(rownames(women_ordered))  #store the order
ordered_adj <- women_adj[indexes,indexes]       #order the adjacency matrix to match

image(ordered_adj, axes=F, col=c(0,1))  #image of adjacency matrix

## Subsection 2.4: Defining the Model Space ################################

#### Figure 2: Selecting the number of clusters to use via BIC ############# 

# Fit an ergmm model (latent position cluster model with no covariates) for
# two to five clusters and plot the BIC
res2 = ergmm(as.network(women_adj) ~ euclidean(d=2, G=2), control=ergmm.control(burnin=1000, sample.size=10000, interval=10))
res3 = ergmm(as.network(women_adj) ~ euclidean(d=2, G=3), control=ergmm.control(burnin=1000, sample.size=10000, interval=10))
res4 = ergmm(as.network(women_adj) ~ euclidean(d=2, G=4), control=ergmm.control(burnin=1000, sample.size=10000, interval=10))
res5 = ergmm(as.network(women_adj) ~ euclidean(d=2, G=5), control=ergmm.control(burnin=1000, sample.size=10000, interval=10))
res6 = ergmm(as.network(women_adj) ~ euclidean(d=2, G=6), control=ergmm.control(burnin=1000, sample.size=10000, interval=10))

bics <- c(bic.ergmm(res2)$overall,bic.ergmm(res3)$overall,bic.ergmm(res4)$overall,
          bic.ergmm(res5)$overall,bic.ergmm(res6)$overall)

plot(2:6, bics, type = 'b', main = '', ylim=c(2475,2510), xlab = 'Number of Clusters', ylab = 'BIC')
bics <- round(bics, 0)
text(2:6, bics, labels = bics, pos = c(4,1,4,4,2), cex = 1.2)

###### Selecting an optimal set of covariates #######

library(nnet)
Kmean = summary(res2)$pmean$Z.K
res = formatting.covars(women_gov, c(1,5:9), c(1,5:9), nrow(women_adj))
fitmn = multinom(Kmean~res$x.mix[,1]+res$x.mix[,2]+res$x.mix[,3]+res$x.mix[,4]+res$x.mix[,5]+res$x.mix[,6]+res$x.mix[,7]+res$x.mix[,8]-1)

# MIXING PROPORTIONS:

mnstep = step(fitmn)
summary(mnstep)
mix.vars = c(1,2,3,7,8)
colnames(res$x.mix)[mix.vars]    
# "intercept"  "Role"  "Age"    "Party2"     "Party2"  


# LINKS:

z = summary(res2)$pmean$Z

delta = c(-as.matrix(dist(z)))
fitglm = glm(c(women_adj)~res$x.link[,1]+res$x.link[,2]+res$x.link[,3]+res$x.link[,4]+res$x.link[,5]+res$x.link[,6]+delta-1,family="binomial")
glmstep = step(fitglm)
summary(glmstep)
link.vars <- c(1,3) 
colnames(res$x.link)[link.vars] 
# "intercept" "Age"



## SECTION 3: RESULTS ######################################################

## Subsection 3.1: Model Fitting ###########################################

# Fit each of the four models
# Note: The MEclustnet function was adjusted to use the posterior mean values from
#       the ergmm function as starting values instead of the posterior mode values. 


####### Model A #######

# No covariates in the link or cluster membership probabilities
link.vars_a = c(1) # intercept only
mix.vars_a = c(1)  # intercept only 

# Fit model A:
mod_a = MEclustnet_pmean(women_adj, women_gov, link.vars_a, mix.vars_a, G=3, d=2, itermax = 5000, burnin = 4000, uphill = 100, thin=10)
# Get model A summary:
summ_a = summaryMEclustnet(mod_a, women_adj) 
# Plot model A:
plot(summ_a$zmean, col=summ_a$Kmode, xlab="Dimension 1", ylab="Dimension 2", pch=summ_a$Kmode, main = "Posterior mean latent location for each node (Model A)")


####### Model B #######

# Covariates in the link probabilities only
link.vars_b = c(1,6) # Intercept and Age
mix.vars_b = c(1)    # Intercept only

# Fit model B:
mod_b = MEclustnet_pmean(women_adj, women_gov, link.vars_b, mix.vars_b, G=3, d=2,itermax = 1000, burnin = 100, uphill = 1, thin=10)
# Get summary of model B:
summ_b = summaryMEclustnet(mod_b, women_adj)  #Compute posterior summaries
# Plot model B: 
plot(summ_b$zmean, col=summ_b$Kmode, xlab="Dimension 1", ylab="Dimension 2", pch=summ_b$Kmode, main = "Posterior mean latent location for each node (Model B).")


####### Model C #######

# Covariates in the cluster membership probabilities only
link.vars_c = c(1)      # Intercept only
mix.vars_c = c(1,5,6,9) # Intercept, Role, Age, Party

# Fit model C:
mod_c = MEclustnet_pmean(women_adj, women_gov, link.vars_c, mix.vars_c, G=3, d=2,itermax = 1000, burnin = 50, uphill = 1, thin=10)
# Get summary of model C:
summ_c= summaryMEclustnet(mod_c, women_adj)  #Compute posterior summaries
# Plot model C: 
plot(summ_c$zmean, col=summ_d$Kmode, xlab="Dimension 1", ylab="Dimension 2", pch=summ_c$Kmode, main = "Posterior mean latent location for each node (Model C).")


####### Model D #######

# Covariates in the link probabilties AND cluster membership probabilities 
link.vars_d = c(1,6)      # Intercept and Age
mix.vars_d = c(1,5,6,9) # Intercept, Role, Age, Party

# Fit model D:
mod_d = MEclustnet_pmean(women_adj, women_gov, link.vars_d, mix.vars_d, G=3, d=2,itermax = 1000, burnin = 50, uphill = 1, thin=10)
# Get summary of model B:
summ_d = summaryMEclustnet(mod_d, women_adj)  #Compute posterior summaries
# Plot model B: 
plot(summ_d$zmean, col=summ_d$Kmode, xlab="Dimension 1", ylab="Dimension 2", pch=summ_d$Kmode, main = "Posterior mean latent location for each node (Model D)")


#### Table 2: Cross Tabulations With Party ####

#Model A:
tab_a <- table(summ_a$Kmode, women_gov$Party2)
tab_a
classAgreement(tab_a) # Get Rand Index

#Model B:
tab_b <- table(summ_b$Kmode, women_gov$Party2)
tab_b
classAgreement(tab_b) # Get Rand Index

#Model C:
tab_c <- table(summ_c$Kmode, women_gov$Party2)
tab_c
classAgreement(tab_c) # Get Rand Index

#Model D:
tab_d <- table(summ_d$Kmode, women_gov$Party2)
tab_d
classAgreement(tab_d) # Get Rand Index


#BICM and AICM plots

Bs <- round(c(summ_a$BICM, summ_b$BICM, summ_c$BICM, summ_d$BICM ),2)
As <- round(c(summ_a$AICM, summ_b$AICM, summ_c$AICM, summ_d$AICM ),2)

plot(1:4, Bs, type ='b', xlab = 'Covariates', ylab = 'BICM', xaxt = 'n')
axis(at =1:4,side =1,labels = c('None', 'Link', 'Cluster', 'Both'))
text(1:4, Bs, labels = Bs, pos = c(4,3,1,2), cex = 1)

plot(1:4, As, type ='b', xlab = 'Covariates', ylab = 'AICM', xaxt = 'n')
axis(at =1:4,side =1,labels = c('None', 'Link', 'Cluster', 'Both'))
text(1:4, As, labels = As, pos = c(4,3,1,2), cex = 1)


#### Figure 4 : Plot the resulting latent spaces, with uncertainties ####
plotMEclustnet(mod_a, women_adj,link.vars_a, mix.vars_a)
plotMEclustnet(mod_b, women_adj,link.vars_b, mix.vars_b)
plotMEclustnet(mod_c, women_adj,link.vars_c, mix.vars_c)
plotMEclustnet(mod_d, women_adj,link.vars_d, mix.vars_d)



#### Table 3 : Parameter estimates for each model ######

# Model A:
round(summ_a$betamean,2) 
round(summ_a$betasd,2)   
round(summ_a$taumean,2) 
round(summ_a$tausd,2)   
round(summ_a$mumean,2) 
round(summ_a$musd,2)   
round(summ_a$sigma2mean,2)
round(summ_a$sigma2sd,2)   
summ_a$AICM 
summ_a$BICM

# Model B:
round(summ_b$betamean,2) 
round(summ_b$betasd,2)   
round(summ_b$taumean,2) 
round(summ_b$tausd,2)   
round(summ_b$mumean,2) 
round(summ_b$musd,2)   
round(summ_b$sigma2mean,2) 
round(summ_b$sigma2sd,2) 
summ_b$AICM 
summ_b$BICM


# Model C:
round(summ_c$betamean,2) 
round(summ_c$betasd,2)   
round(summ_c$taumean,2) 
round(summ_c$tausd,2)  
round(summ_c$mumean,2) 
round(summ_c$musd,2)   
round(summ_c$sigma2mean,2) 
round(summ_c$sigma2sd,2)   
summ_c$AICM 
summ_c$BICM 

# Model D:
round(summ_d$betamean,2) 
round(summ_d$betasd,2) 
round(summ_d$taumean,2) 
round(summ_d$tausd,2)  
round(summ_d$mumean,2) 
round(summ_d$musd,2)   
round(summ_d$sigma2mean,2) 
round(summ_d$sigma2sd,2)   
summ_d$AICM 
summ_d$BICM 


## Subsection 3.2: Model Assessment ######################################

### Figure 5 ####
# uncertainty in cluster membership plots:

N = nrow(women_covars)

# Model A:
tabs_a = apply(mod_a$Kstore,2,table)/nrow(mod_a$Kstore)
uncert_a = 1 - apply(tabs,2,max)
plot(uncert_a, type = 'h', main = 'Model A', xlab = 'Politician', ylab = 'Uncertainty',ylim = c(0.0,0.65))

# Model B:
tabs_b = apply(mod_b$Kstore,2,table)/nrow(mod_b$Kstore)
uncert_b = 1 - apply(tabs_b,2,max)
plot(uncert_b, type = 'h', main = 'Model B', xlab = 'Politician', ylab = 'Uncertainty',ylim = c(0.0,0.65))


#function to calculate lambda(w_ig) for models C and D:
lambda = function(sum, g, w, G){
  taus = sum$taumean
  denom = 0
  for(i in 1:length(taus))
    num = exp(sum(taus[g,]*w))
  for(g_star in 1:G){
    sum = exp(sum(taus[g_star,]*w))
    denom = denom + sum
  }
  return(num/denom)
}

# Model C:
res_c = formatting.covars(women_gov, link.vars_c, mix.vars_c, nrow(women_adj))
lambda_c = matrix(NA,N,3)
G = 3

for(i in 1:N){
  for(g in 1:G){
    lambda_c[i,g] = lambda(summ_c, g, res_c$x.mix[i,], G)
  }
}

uncert_c = rep(NA,N)
for(i in 1:N){
  uncert_c[i] = 1 - max(lambda_c[i,])
}

plot(1:N, uncert_c,type = 'h', xlab = 'politician', ylab = 'uncertainty',main = 'Model C', ylim = c(0.0,0.65))


# Model D:
res_d = formatting.covars(women_gov, link.vars_d, mix.vars_d, nrow(women_adj))
lambda_d = matrix(NA,N,3)
G = 3

for(i in 1:N){
  for(g in 1:G){
    lambda_d[i,g] = lambda(summ_d, g, res_d$x.mix[i,], G)
  }
}

uncert_d = rep(NA,N)
for(i in 1:N){
  uncert_d[i] = 1 - max(lambda_d[i,])
}

plot(1:N, uncert_d,type = 'h', xlab = 'politician', ylab = 'uncertainty', main = 'Model D', ylim = c(0.0,0.65))



## Subsection 3.3: Model Convergence ######################################

### Figure 6: parameter convergence plots for model D ###
#cluster mean (dimension 1):
matplot(t(mod_d$mustore[,1,]), type="l", main = 'mu 1',xlab="Iteration", ylab="Parameter")
#cluster mean (dimension 2):
matplot(t(mod_d$mustore[,2,]), type="l", main = 'mu 2',xlab="Iteration", ylab="Parameter")
#beta intercept:
matplot((mod_d$betastore[,1]), type="l", main = 'beta 1',xlab="Iteration", ylab="Parameter")
#beta age:
matplot((mod_d$betastore[,2]), type="l", main = 'beta age',xlab="Iteration", ylab="Parameter")


## Subsection 3.4: Model Exploration ######################################

### Figure 7 ###
#plot of number of friends vs age

#get friends count:
friends_count_adj <- matrix(0, N, N)

for(i in 1:N){
  friends_count_adj[i,] <- ifelse(women_adj[i,] == 1 & women_adj[,i] == 1, 1,0)
} 
friends_count_adj <- as.matrix(friends_count_adj)

#Model d labels:
Kmode_d = as.numeric(apply(mod_d$Kstore, 2, function(v) {
  names(sort(-table(v)))[1]
}))

#plot it:
plot(women_covars$Age,friends_count, col = Kmode_d, pch = Kmode_d,
     xlab = 'age', ylab = 'Number of Friends', main = '')

#Poisson Regression:
age_glm <- glm(friends_count ~ women_covars$Age, family = poisson(link='log'))
summary(age_glm)


### Figure 8 ###
#Note: plotMEclustnet was modified to plot the politicians initials as nodes and colour
#      the nodes according to political party, it was called myplot.
myplot(mod_d, women_adj, link.vars_d, mix.vars_d) 


######### Appendix ##########################################################################

# A: Table of Full Data Set:

library(xtable)

women_ordered <- women_gov[order(women_gov$name),]  # order by party 
women_ordered <- women_gov[order(women_gov$Party),] # order alphabetically

women_ordered <- women_ordered[,c(2,9,4:8)] # only use necessary collumns

print(xtable(women_ordered, type = "latex"), file = "women_ordered.tex") # make latex table


# B: Figure 9: Trace plots for fitted paramters of the rest of the models

# Model A:
matplot(t(mod_a_mean$mustore[,1,]), type="l", main = 'mu 1 (mod a)', xlab="Iteration", ylab="Parameter")
matplot(t(mod_a_mean$mustore[,2,]), type="l", main = 'mu 2 (mod a) ', xlab="Iteration", ylab="Parameter")
matplot((mod_a_mean$betastore), type="l", main = 'beta 1 (mod a)',xlab="Iteration", ylab="Parameter")

# Model B:
matplot(t(mod_b_mean$mustore[,1,]), type="l", main = 'mu 1 (mod a)',xlab="Iteration", ylab="Parameter")
matplot(t(mod_b_mean$mustore[,2,]), type="l", main = 'mu 2 (mod b)',xlab="Iteration", ylab="Parameter")
matplot((mod_c_mean$betastore[,1]), type="l", main = 'beta 1 (mod b)',xlab="Iteration", ylab="Parameter")
matplot((mod_b_mean$betastore[,2]), type="l", main = 'beta age (mod b)',xlab="Iteration", ylab="Parameter")

# Model C:
matplot(t(mod_c_mean$mustore[,1,]), type="l", main = 'mu 1 (mod c)',xlab="Iteration", ylab="Parameter")
matplot(t(mod_c_mean$mustore[,2,]), type="l", main = 'mu 2 (mod c)',xlab="Iteration", ylab="Parameter")
matplot((mod_c_mean$betastore), type="l", main = 'beta 1 (mod c)',xlab="Iteration", ylab="Parameter")





# C: Figure 10: Plots of the age v.s. non mutual link types for all four models:

#Model d labels:
Kmode_d = as.numeric(apply(mod_d_mean$Kstore, 2, function(v) {
  names(sort(-table(v)))[1]
}))

# get count of number of people wach politician follows
following_count <- apply(women_adj, 1, sum)
# plot it
plot(women_covars$Age,following_count, col = Kmode_d, pch = Kmode_d,
     xlab = 'age', ylab = 'Number Following', main = '')

#Poisson Regression:
following_glm <- glm(following_count ~ women_covars$Age, family = poisson(link='log'))
summary(following_glm) #significant negative effect of age


# get count of number of followers each politician has
followedby_count <- apply(women_adj, 2, sum)
# plot it
plot(women_covars$Age, followedby_count, col = Kmode_d, pch = Kmode_d,
     xlab = 'age', ylab = 'Number of Followers', main = '')

#Poisson Regression:
followed_by_glm <- glm(followedby_count ~ women_covars$Age, family = poisson(link='log'))
summary(followed_by_glm) #insignificant effect of age






