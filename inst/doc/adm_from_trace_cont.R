## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(admtools)

## -----------------------------------------------------------------------------
h_min = 10 # stratigraphic height of lower tie point [m]
h_max = 20 # stratigraphic height of upper tie point [m]

## ----h_tp function------------------------------------------------------------
h_tp = function(){
  return(c("h1" = h_min,
           "h2" = h_max))
}

## ----Evaluate stratigraphic positions of tie points---------------------------
h_tp()

## -----------------------------------------------------------------------------
t_tp = function() {
  # timing first tie point
  t1 = rnorm(n = 1, mean = 0, sd = 0.5)
  
  # timing second tie point
  t2 = runif(n = 1, min = 9, max = 11)
  
  return(c("t1" = t1, "t2" = t2))
}

## ----echo=FALSE, fig.show="hold"----------------------------------------------
no_of_samples = 1000
hist(sapply( seq_len(no_of_samples), function(x) t_tp()["t1"]),
     freq = FALSE,
     xlab = "Time [Myr]",
     main = "Timing of lower tie point")
hist(sapply(seq_len(no_of_samples), function(x) t_tp()["t2"]),
     freq = FALSE,
     xlab = "Time [Myr]",
     main = "Timing of upper tie point")

## -----------------------------------------------------------------------------
time_cont_gen = function(){
  time_cont = function(x) return(rep(1, length(x)))
  return(time_cont)
}

## -----------------------------------------------------------------------------
# generate one function from time_cont_gen
time_cont = time_cont_gen()
# time where to evaluate the function
t = seq(0,10,by = 0.1)
plot(x = t,
     y = time_cont(t),
     type = "l",
     xlab = "Time [Myr]",
     ylab = "Tracer Input into the Sediment [1/Myr]")


## -----------------------------------------------------------------------------
locations = c("h_1" = h_min, "h_2" = mean(c(h_min, h_max)), "h_3" = h_max)

## -----------------------------------------------------------------------------
mean_vals = c("mu_1" = 10, "mu_2" = 1, "mu_3" = 8)
sd_vals = c("sig_1" = 1, "sig_2" = 0.1, "sig_3" = 0.8)

## -----------------------------------------------------------------------------
strat_cont_gen = function(){
   loc = locations
   trac_vals = rnorm(n = length(mean_vals),
                     mean = mean_vals,
                     sd = sd_vals)
   strat_cont = approxfun(x = loc,
                          y = trac_vals,
                          yleft = trac_vals[1],
                          yright = trac_vals[3])
   return(strat_cont)
}

## -----------------------------------------------------------------------------
n = 3 # number of sampled tracer values 
h = seq(h_min, h_max, by = 0.1) # determine age-depth model every 0.1 m
plot(NULL,
     xlim = c(h_min, h_max),
     ylim = c(0, max(mean_vals) +2 * max(sd_vals)),
     xlab = "Stratigraphic Height [m]",
     ylab = "Measured Tracer [1/m]")
cols = c("red", "blue", "black")
for (i in seq_len(n)){
  strat_cont = strat_cont_gen()
  lines(x = h,
        y = strat_cont(h),
        col = cols[i])
}


## -----------------------------------------------------------------------------
h = seq(h_min,h_max, by = 0.25) # strat. positions where ADMs are estimated
no_of_rep = 10 # no. of ADMs estimated

## -----------------------------------------------------------------------------
my_adm = strat_cont_to_multiadm(h_tp = h_tp,
                                t_tp = t_tp,
                                strat_cont_gen = strat_cont_gen,
                                time_cont_gen = time_cont_gen,
                                h = h,
                                no_of_rep = no_of_rep)

## -----------------------------------------------------------------------------
plot(my_adm)

## ----eval=FALSE---------------------------------------------------------------
#  vignette("adm_from_sedrate")

