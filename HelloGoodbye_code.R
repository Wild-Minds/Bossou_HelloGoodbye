## load necessary packages
library(brms)
library(bayesplot)
library(rstanarm)
library(rstan)
library(tidyverse)
library(car)
library(loo)

## load datasets
load('data.RData')


##### Model 1: How does communication vary between fission and fusion events?

## Test for multicollinearity using Variance Inflation Factors
vif(lm(data=t.data.1,communication ~
         type_event+ poss_joint_travel +
         period +
         level_threat + z.focal_rank + focal_position + 
         kinship + rank_relation + 
         presence_males + z.group_size))


## Set priors
m1priors <-c(
  prior (cauchy(0,10), class= "Intercept"),
  prior (cauchy(0,2.5), class="b"))

## Bayesian analysis
model1 = brm(
  data = t.data.1,
  communication ~ 1 +
    type_event*poss_joint_travel +
    period +
    level_threat + z.focal_rank + focal_position + 
    kinship + rank_relation + 
    presence_males + z.group_size + 
    (1+ type_event*poss_joint_travel|| focal) +
    (1+ type_event*poss_joint_travel|| partner) +
    (1| event_number),
  prior = m1priors,
  warmup = 500,
  iter = 3000,
  cores = 10, 
  chains = 2, 
  family = bernoulli(link = 'logit'),
  seed = 123,
  control = list(adapt_delta = 0.95)
)

## Posterior predictive checks
# Assessing model fit (convergence)
pp_check(model1)
# Identifying outliers and influencial points
plot(loo(model1))

## Results
# Coefficients
summary(model1)$fixed
# Odds
odds.model1 = exp(fixef(model1))
# Effect size
r2.communication1 = bayes_R2(model1)



##### Model 2: Which social features affect the probability of communicating during fission and fusion events?

## Test for multicollinearity using Variance Inflation Factors
vif(lm(data=t.data.2,communication ~
         type_event +
         period +
         level_threat + z.focal_rank + focal_position + 
         kinship + rank_relation + 
         presence_males + z.group_size))


## Set priors
m2priors <-c(
  prior (cauchy(0,10), class= "Intercept"),
  prior (cauchy(0,2.5), class="b"))

## Bayesian analysis
model2= brm(
  data = t.data.2,
  communication ~ 1 + period +
    type_event*level_threat + 
    type_event*z.focal_rank + 
    type_event*focal_position + 
    type_event*kinship + 
    type_event*rank_relation + 
    type_event*presence_males + 
    type_event*z.group_size + 
    (1| focal) +
    (1| partner) +
    (1| event_number),
  prior = m2priors,
  warmup = 500,
  iter = 3000, 
  cores = 10, 
  chains = 2, 
  family = bernoulli(link = 'logit'),
  seed = 123,
  control = list(adapt_delta = 0.95) 
)

## Posterior predictive checks
# Assessing model fit (convergence)
pp_check(model2)
# Identifying outliers and influencial points
plot(loo(model2))

## Results
# Coefficients
summary(model2)$fixed
# Odds
odds.model2 = exp(fixef(model2))
# Effect size
r2.communication2 = bayes_R2(model2)



##### Model 3: What determines the channel of communication during fissions and fusions?

## Set priors
m3priors <-c(
  prior (cauchy(0,10), class= "Intercept"),
  prior (cauchy(0,2.5), class="b"))

## Bayesian analysis
model3 = brm(
  data = t.data.3,
  modality_communication ~ 
    1 + type_event + 
    level_threat + 
    kinship + 
    rank_relation + 
    presence_males + 
    z.group_size +
    (1|focal),
  prior = m3priors,
  warmup = 500,
  iter = 7000,
  cores = 10, 
  chains = 2, 
  family = categorical(refcat = 'Voc'),
  seed = 123,
  control = list(adapt_delta = 0.999)
)

## Posterior predictive checks
# Assessing model fit (convergence)
pp_check(model3)
# Identifying outliers and influencial points
plot(loo(model3))

## Results
# Coefficients
summary(model3)$fixed
# Odds
odds.model3 = exp(fixef(model3))
# Effect size
r2.communication3 = bayes_R2(model3)