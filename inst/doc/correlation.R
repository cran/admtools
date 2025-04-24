## ----setup--------------------------------------------------------------------
library(admtools)

## -----------------------------------------------------------------------------
# simulation data
# entries in vectors are coeval bc simulation time steps were identical
h1 = CarboCATLite_data$height_2_km_offshore_m
h2 = CarboCATLite_data$height_12_km_offshore_m
ddc1 = tp_to_ddc(h1 = h1,
                 h2 = h2,
                 L_unit_1 = "m", # associate length units
                 L_unit_2 = "m",
                 sec_1 = "2 km offshore", # name of correlated sections
                 sec_2 = "12 km offshore")

## -----------------------------------------------------------------------------
adm_2km = tp_to_adm(t = CarboCATLite_data$time_myr,
                    h = CarboCATLite_data$height_2_km_offshore_m,
                    L_unit = "m",
                    T_unit = "Myr")

adm_8km = tp_to_adm(t = CarboCATLite_data$time_myr,
                    h = CarboCATLite_data$height_8_km_offshore_m,
                    L_unit = "m",
                    T_unit = "Myr")

ddc2 = adm_to_ddc(adm1 = adm_2km, adm2 = adm_8km)
# assign section names
ddc2 = set_section_names(ddc2, sec_names = c("2 km from shore", "8 km from shore"))

## -----------------------------------------------------------------------------
plot(ddc1,
     type = "l",
     xlab = "",
     ylab = "")
mtext(get_section_names(ddc1)[1], side = 1, line = 3)
mtext(get_section_names(ddc1)[2], side = 2, line = 3)

## -----------------------------------------------------------------------------
summary(ddc1)

## -----------------------------------------------------------------------------
plot(ddc1, type = "l", xlab = "", ylab = "") # correlation from 2 km offshore to 12 km offshore
mtext(get_section_names(ddc1)[1], side = 1, line = 3)
mtext(get_section_names(ddc1)[2], side = 2, line = 3)
ddc3 = flip_ddc(ddc1)
plot(ddc3, type = "l", xlab = "", ylab = "") # correlates 12 km offshore with 2 km offshore
mtext(get_section_names(ddc3)[1], side = 1, line = 3)
mtext(get_section_names(ddc3)[2], side = 2, line = 3)

