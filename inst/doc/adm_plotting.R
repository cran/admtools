## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(admtools)

## -----------------------------------------------------------------------------
adm = tp_to_adm(t = CarboCATLite_data$time_myr,
                h = CarboCATLite_data$height_4_km_offshore_m,
                L_unit = "m",
                T_unit = "Myr")

## -----------------------------------------------------------------------------
plot(adm,
     lwd_destr = 1,
     lty_destr = 4,
     col_destr = "blue",
     lwd_acc = 1,
     lty_acc = 1,
     col_acc = "black")

## -----------------------------------------------------------------------------
plot(adm,
     lty_destr = 0,
     lwd_acc = 3)
T_axis_lab(label = "Elapsed Model Time")
L_axis_lab(label = "Stratigraphic Height")

