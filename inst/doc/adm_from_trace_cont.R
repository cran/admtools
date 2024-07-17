## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(admtools)

## -----------------------------------------------------------------------------
h_min = 3 # bottom of section
h_max = 8 # top of section
sampling_bin_borders = seq(h_min, h_max, by = 1) # limits of sampling bins
mean_3He = seq(from = 15, to = 5, length.out = length(sampling_bin_borders) - 1) # assumed 3He mean vals
sd_3He = runif(n = length(mean_3He), min = 0.1 * mean_3He, max = 0.2 * mean_3He) # standard deviation of 3He
He_measurements = data.frame("mean" = mean_3He, "sd" = sd_3He)

## -----------------------------------------------------------------------------
h = seq(h_min, h_max, by = 0.1)
T_unit = "dimensionless"
L_unit = "m"

## -----------------------------------------------------------------------------
tp_height = tp_height_det(heights = c(h_min, h_max))

## -----------------------------------------------------------------------------
tp_time = tp_time_floating_scale()

## -----------------------------------------------------------------------------
flux = flux_const()

## -----------------------------------------------------------------------------
observed_tracer = strat_cont_gen_from_tracer(bin_borders = sampling_bin_borders, 
                                          df = He_measurements,
                                          distribution = "normal")

## ----echo=FALSE---------------------------------------------------------------
plot(NULL,
     xlim = range(h),
     ylim = c(0,max(He_measurements$mean) + 2 * max(He_measurements$sd)),
     xlab = paste("Height [", L_unit, "]", sep = ""),
     ylab = "3He observed")
lines(h, observed_tracer()(h), type = "p", col = "red", pch = 3)
lines(h, observed_tracer()(h), type = "p", col = "blue", pch = 19)
lines(h, observed_tracer()(h), type = "p", col = "black")

## -----------------------------------------------------------------------------
my_adm = strat_cont_to_multiadm(h_tp = tp_height,
                                t_tp = tp_time,
                                strat_cont_gen = observed_tracer,
                                time_cont_gen = flux,
                                h = h,
                                T_unit = T_unit,
                                L_unit = L_unit)

## -----------------------------------------------------------------------------
plot(my_adm, mode = "envelope")
T_axis_lab()
L_axis_lab()
make_legend()

## -----------------------------------------------------------------------------
mean_adm = mean_adm(my_adm, h)
plot(mean_adm)

## -----------------------------------------------------------------------------
h_min = 10 # stratigraphic height of lower tie point [m]
h_max = 20 # stratigraphic height of upper tie point [m]

## ----h_tp function------------------------------------------------------------
h_tp = function(){
  return(c(h_min, h_max))
}

## ----Evaluate stratigraphic positions of tie points---------------------------
h_tp()

## -----------------------------------------------------------------------------
t_tp = function() {
  repeat{ 
    # timing first tie point
    t1 = rnorm(n = 1, mean = 0, sd = 0.5)
    # timing second tie point
    t2 = runif(n = 1, min = 9, max = 11)
    if (t1 < t2){ # if order is correct, return values
        return(c(t1, t2))
    }
  }
}

## -----------------------------------------------------------------------------
t_tp() # evaluating the function returns a random pair of times drawn from the specified distribution

## ----echo=FALSE, fig.show="hold"----------------------------------------------
no_of_samples = 1000
hist(sapply( seq_len(no_of_samples), function(x) t_tp()[1]),
     freq = FALSE,
     xlab = "Time [Myr]",
     main = "Timing of lower tie point")
hist(sapply(seq_len(no_of_samples), function(x) t_tp()[2]),
     freq = FALSE,
     xlab = "Time [Myr]",
     main = "Timing of upper tie point")

## -----------------------------------------------------------------------------
time_cont_gen = function(){
  time_cont = function(x) return( rep(1, length(x)))
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
     ylab = "Tracer Input into the Sediment [X/Myr]")


## -----------------------------------------------------------------------------
h_min = 2
h_max = 5
locations = c( h_min,  mean(c(h_min, h_max)),  h_max)

## -----------------------------------------------------------------------------
mean_vals = c(10,  1,  8)
sd_vals = c( 1, 0.1, 0.8)

## -----------------------------------------------------------------------------
strat_cont_gen = function(){
  # draw sample tracer values from specified distributions
   trac_vals = rnorm(n = length(mean_vals),
                     mean = mean_vals,
                     sd = sd_vals)
   # define function that linearly interpolates between drawn values
   strat_cont = approxfun(x = locations,
                          y = trac_vals,
                          yleft = trac_vals[1],
                          yright = trac_vals[3])
   return(strat_cont)
}

## -----------------------------------------------------------------------------
n = 3 # number of sampled tracer values 
h = seq(h_min, h_max, by = 0.1) # determine tracer values every 0.1 m 
plot(NULL,
     xlim = c(h_min, h_max),
     ylim = c(0, max(mean_vals) +2 * max(sd_vals)),
     xlab = "Stratigraphic Height [m]",
     ylab = "Measured Tracer [1/m]")
cols = c("red", "blue", "black")
for (i in seq_len(n)){
  strat_cont = strat_cont_gen() # draw sample path from stoch process 
  # draw sample path
  lines(x = h,
        y = strat_cont(h),
        col = cols[i])
}


## ----eval=FALSE---------------------------------------------------------------
#  vignette("adm_from_sedrate")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("adm_plotting)

## ----eval=FALSE---------------------------------------------------------------
#  vignette("admtools_doc")

