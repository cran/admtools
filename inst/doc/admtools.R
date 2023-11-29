## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(admtools)

## -----------------------------------------------------------------------------
library(admtools)

## -----------------------------------------------------------------------------
data("CarboCatLite_data")

## -----------------------------------------------------------------------------
# see ?tp_2_adm for detailed documentation
my_adm = tp_2_adm(t = CarboCATLite_data$time_myr,
                  h = CarboCATLite_data$height_2_km_offshore_m,
                  L_unit = "m",
                  T_unit = "Myr")

## -----------------------------------------------------------------------------
summary(my_adm)

## -----------------------------------------------------------------------------
# see ?plot.adm for plotting options for adm objects
plot(my_adm,
     col_hiat = "red",
     lwd_cons = 3)

## -----------------------------------------------------------------------------
is_destructive(adm = my_adm,
               t = c(0.1,0.5)) 

## -----------------------------------------------------------------------------
h = c(30,120) # stratigraphic positions
get_time(adm = my_adm,
             h = h)

## -----------------------------------------------------------------------------
t = c(0.2,1.4)
get_height(adm = my_adm,
           t = t)

## -----------------------------------------------------------------------------
t = c(0.2,1.4)
get_height(adm = my_adm,
           t = t,
           destructive = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  vignette("adm_from_sedrate")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("adm_from_trace_cont")

