## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(admtools)

## -----------------------------------------------------------------------------
h_min = 10 # height of tie point 1
h_max = 85 # height of tie point 2

## -----------------------------------------------------------------------------
h_tp = function(){
  return(c("h1" = h_min, 
           "h2" = h_max))
}

## -----------------------------------------------------------------------------
#Evaluate stratigraphic positions of tie points
h_tp()

## -----------------------------------------------------------------------------
t_tp = function(){
  # first tie point
  t1 = rnorm(n = 1, mean = 0, sd = 0.5)
  
  # second tie point
  d1 = rbinom(n = 1, size = 2, prob = 0.5) # fair coin flip for mixture
  if (d1 == 0){
    t2 = rnorm(n = 1, mean = 5, sd = 0.1)
  } else {
    t2 = rnorm(n = 1, mean = 7, sd = 0.3)
  }
  return(c("t1" = t1, "t2" = t2))
}

## ----fig.show="hold", out.width="50%"-----------------------------------------
no_of_samples = 10000
# Evaluate timing of tie points
hist(x = sapply(seq_len(no_of_samples), function(x) t_tp()["t1"]),
     freq = FALSE,
     xlab = "Time [Myr]",
     main = "Timing of first tie point")
hist(x = sapply(seq_len(no_of_samples), function(x) t_tp()["t2"]),
     freq = FALSE,
     xlab = "Time [Myr]",
     main = "Timing of second tie point")


## -----------------------------------------------------------------------------
# limits on sed. rates
lower_limit = c(0.1,2,0.1,10)
upper_limit = c(0.2,3,2,12)

# strat intervals where sed rates are defined
s = c(h_min - 1, 30,75, 80, h_max + 1)

## -----------------------------------------------------------------------------
# define function factory
sed_rate_fun = function(){
  # draw sed rates from uniform distribution
  aa_1 = c(runif(1,lower_limit[1],upper_limit[1]),
         runif(1, lower_limit[2],upper_limit[2]),
         runif(1,lower_limit[3],upper_limit[3]),
         runif(1, lower_limit[4], upper_limit[4]))
  aa = c(aa_1, aa_1[4])
  # define sed rate "realization" based on samples from uniform distribution
 sed_rate_fun = approxfun(x = s,
                          y = aa,
                          method = "constant")
 # return the function
 return(sed_rate_fun)
}

## -----------------------------------------------------------------------------
plot(NULL,
     xlim = c(h_min, h_max),
     ylim = c(0, max(upper_limit)),
     xlab = "Stratigraphic Height [m]",
     ylab = "Sedimentation Rate")

no_of_sedrates = 3 # no. of sed rates displayed
h = seq(h_min,h_max, by = 0.1) # strat. positions where sed rates are plotted

for (i in seq_len(no_of_sedrates)){
  # generate sed rate from the factory
  sed_rate_sample = sed_rate_fun()
  
  # plot sed rate in the section
  lines(h, sed_rate_sample(h))
}

## -----------------------------------------------------------------------------
h = seq(h_min,h_max, by = 1) # strat. positions where ADMs are estimated
no_of_rep = 10 # no. of ADMs estimated

## -----------------------------------------------------------------------------
my_adm = sedrate_to_multiadm(h_tp = h_tp(),
                             t_tp = t_tp(),
                             sed_rate_gen = sed_rate_fun,
                             h = h,
                             no_of_rep = no_of_rep)

## ----echo=FALSE---------------------------------------------------------------
plot(my_adm)

## ----eval=FALSE---------------------------------------------------------------
#  vignette("adm_from_trace_cont")

