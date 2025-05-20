## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(admtools)

## -----------------------------------------------------------------------------
h_min = 2 # lower boundary of the section
h_max = 10 # upper boundary of the section
T_unit = "Myr"
L_unit = "m"

## -----------------------------------------------------------------------------
h = seq(h_min, h_max, by = 0.1)

## -----------------------------------------------------------------------------
h1 = 5
mean_age = -66
sd = 0.25 

## -----------------------------------------------------------------------------
t_tp = tp_time_norm(mean = mean_age, sd = sd)

## -----------------------------------------------------------------------------
h_tp = tp_height_det(heights = h1)

## -----------------------------------------------------------------------------
h_tp()
t_tp()

## -----------------------------------------------------------------------------
sedrate_max_y = c(2,5,8,5)
sedrate_max_x = c(1,4,6,10)
sedrate_min_y = c(1,1,7,0.5)
sedrate_min_x = sedrate_max_x 

## -----------------------------------------------------------------------------
sedrate = sed_rate_gen_from_bounds(h_l = sedrate_min_x,
                                   s_l = sedrate_min_y,
                                   h_u = sedrate_max_x,
                                   s_u = sedrate_max_y,
                                   rate = 1)

## -----------------------------------------------------------------------------
plot(NULL,
     xlim = range(h),
     ylim = c(0, max(c(sedrate_max_y))),
     xlab = "Height [m]",
     ylab = "Sedimentation Rate [m/Myr]")
no_sedrates = 3
cols = c("red", "blue", "black")
for (i in seq_len(no_sedrates)){
  sedrate_sample = sedrate()
  lines(h, sedrate_sample(h), lwd = 3, col = cols[i])
}

## -----------------------------------------------------------------------------
my_adm = sedrate_to_multiadm(h_tp = h_tp,
                             t_tp = t_tp,
                             sed_rate_gen = sedrate,
                             h = h,
                             T_unit = T_unit,
                             L_unit = L_unit)

## -----------------------------------------------------------------------------
plot(my_adm)

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
h_min = 10
h_max = 90
# limits on sed. rates 
lower_limit = c(0.1,2,0.1,10) 
upper_limit = c(0.2,3,2,12)  
# strat intervals where sed rates are defined 
s = c(h_min, 30,65, 80, h_max)

## -----------------------------------------------------------------------------
# define function factory 
sed_rate_fun = function(){   
  # draw sed rates from uniform distribution  
  aa = runif(n = length(lower_limit), min = lower_limit, max = upper_limit)   
  # define sed rate "realization" based on samples from uniform distribution 
  sed_rate_fun = approxfun(x = s, 
                           y = c(aa, aa[length(aa)]), 
                           method = "constant", 
                           rule = 2,  
                          f = 1) 
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
cols = c("red", "blue", "black")
for (i in seq_len(no_of_sedrates)){
  # generate sed rate from the factory
  sed_rate_sample = sed_rate_fun()
  
  # plot sed rate in the section
  lines(h, sed_rate_sample(h), col = cols[i])
}

## ----eval=FALSE---------------------------------------------------------------
# vignette("adm_from_trace_cont")

## ----eval=FALSE---------------------------------------------------------------
# vignette("adm_plotting)

## ----eval=FALSE---------------------------------------------------------------
# vignette("admtools_doc")

## ----eval=FALSE---------------------------------------------------------------
# browseVignettes(package = "admtools")

