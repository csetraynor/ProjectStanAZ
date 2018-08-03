library(rstanarm)
library(methods)
library(rstan)
library(survival)
library(magrittr)
library(plyr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Read arguments listed on command line
args = (commandArgs(TRUE))
i = as.integer(args[1])

print(i)

#source gen_stan_dat function
source("misc.R")


#Load data --------------
mc_samp <- readRDS("data-raw/mc_samp.RDS")

x <- mc_samp$splits[[i]] #choose splits
x <- as.data.frame(x$data)[x$in_id,]

long_x <- gen_stan_dat(x)

#Bayesian clinical model with intclust----------
surv_form <- c( "age_std", "npi_std", "her2pos", "erpos", "factor(intclust)")

form <- as.formula( paste0(c( "status~1+offset(log_dtime)+s(time)", paste(surv_form, collapse = "+")), collapse = ""))

# loop object through splits

bayes_test <- rstanarm::stan_gamm4(form, data = long_x ,
                                   #set likelihood (poisson)
                                   family= poisson(),
                                   #set priors (default)
                                   prior = normal(),
                                   prior_intercept = normal(),
                                   prior_smooth = exponential(autoscale = FALSE),
                                   prior_aux = exponential(),
                                   prior_covariance = decov(),
                                   prior_PD = FALSE,
                                   #arguments passed to sampling algorithm
                                   cores = 5, seed = 7,
                                   iter = 12000, warmup = 2000, thin = 10, refresh = 0, chains = 5,
                                   control = list(adapt_delta = 0.99)
                                   )

saveRDS(bayes_test, paste0("bayes_model_rstan_clinical_full_with_intclust", i, ".RDS") )


rm(list=ls())

