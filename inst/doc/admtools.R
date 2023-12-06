## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(admtools)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("remotes")
#  

## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github(repo = "MindTheGap-ERC/admtools",
#                          build_vignettes = TRUE,
#                          ref = "v0.1.0")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("admtools")

## ----eval=FALSE---------------------------------------------------------------
#  library(admtools)

## ----eval=FALSE---------------------------------------------------------------
#  help(package = "admtools")

## ----eval=FALSE---------------------------------------------------------------
#  ?admtools

## ----eval=FALSE---------------------------------------------------------------
#  browseVignettes(package = "admtools") # opens in Browser
#  #or
#  vignette(package = "admtools")

## -----------------------------------------------------------------------------
data("CarboCATLite_data")

## -----------------------------------------------------------------------------
# see ?tp_to_adm for detailed documentation
my_adm = tp_to_adm(t = CarboCATLite_data$time_myr,
                  h = CarboCATLite_data$height_2_km_offshore_m,
                  L_unit = "m",
                  T_unit = "Myr")

## -----------------------------------------------------------------------------
my_adm

## -----------------------------------------------------------------------------
summary(my_adm)

## -----------------------------------------------------------------------------
str(my_adm)

## -----------------------------------------------------------------------------
# see ?plot.adm for plotting options for adm objects
plot(my_adm,
     col_hiat = "red",
     lwd_cons = 2)

## -----------------------------------------------------------------------------
get_total_duration(my_adm) #total time covered by the age-depth model
get_total_thickness(my_adm) # total thickness of section represented by the amd
get_completeness(my_adm) # stratigraphic completeness as proportion
get_incompleteness(my_adm) # stratigraphic incompleteness (= 1- strat. incompleteness)
get_hiat_no(my_adm) # number of hiatuses

## -----------------------------------------------------------------------------
hist(x = get_hiat_duration(my_adm),
     freq = TRUE,
     xlab = "Hiatus duration [Myr]",
     main = "Hiatus duration 2 km offshore")

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

## -----------------------------------------------------------------------------
#install.packages("ape") Package for analyses of phylogenetics and evolution
# see ?ape::rlineage for help
set.seed(1)
tree_in_time = ape::rlineage(birth = 1.8,
                             death = 0.2,
                             Tmax = 2)
plot(tree_in_time) # see also ?ape::plot.phylo
axis(1)
mtext("Time [Myr]", side = 1, line = 2.5)

## -----------------------------------------------------------------------------
tree_in_strat_domain = time_to_strat(obj = tree_in_time,
                                     x = my_adm)

## -----------------------------------------------------------------------------
plot(tree_in_strat_domain, direction = "upwards")
axis(side = 2)
mtext("Stratigraphic Height [m]",
      side = 2,
      line = 2)

## ----eval=FALSE---------------------------------------------------------------
#  vignette("adm_from_sedrate")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("adm_from_trace_cont")

