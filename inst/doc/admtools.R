## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(admtools)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("admtools")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("remotes")
#  

## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github(repo = "MindTheGap-ERC/admtools",
#                          build_vignettes = TRUE,
#                          ref = "HEAD",
#                          dependencies = TRUE)

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

## ----eval=FALSE---------------------------------------------------------------
#  ?CarboCATLite_data

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
     col_destr = "red",
     lwd_acc = 2)
T_axis_lab() # plot time axis label, see ?T_axis_lab for details
L_axis_lab() # plot height axis label, see ?L_axis_lab for details

## -----------------------------------------------------------------------------
plot_sed_rate_t(my_adm)

## -----------------------------------------------------------------------------
get_total_duration(my_adm) #total time covered by the age-depth model
get_total_thickness(my_adm) # total thickness of section represented by the adm
get_completeness(my_adm) # stratigraphic completeness as proportion
get_incompleteness(my_adm) # stratigraphic incompleteness (= 1- strat. incompleteness)
get_hiat_no(my_adm) # number of hiatuses

## -----------------------------------------------------------------------------
hist(x = get_hiat_duration(my_adm),
     freq = TRUE,
     xlab = "Hiatus duration [Myr]",
     main = "Hiatus duration 2 km offshore")

## -----------------------------------------------------------------------------
is_destructive(my_adm,
               t = c(0.1,0.5)) 

## -----------------------------------------------------------------------------
h = c(30,120) # stratigraphic positions
get_time(my_adm,
         h = h)

## -----------------------------------------------------------------------------
t = c(0.2,1.4)
get_height(my_adm,
           t = t)

## -----------------------------------------------------------------------------
t = c(0.2,1.4)
get_height(my_adm,
           t = t,
           destructive = FALSE)

## -----------------------------------------------------------------------------
#install.packages("ape") Package for analyses of phylogenetics and evolution
# see ?ape::rlineage for help
#set.seed(1)

ape::plot.phylo(timetree) # see also ?ape::plot.phylo
axis(1)
mtext("Time [Myr]", side = 1, line = 2.5)

## -----------------------------------------------------------------------------
tree_in_strat_domain = time_to_strat(obj = timetree,
                                     x = my_adm)

## -----------------------------------------------------------------------------
ape::plot.phylo(tree_in_strat_domain, direction = "upwards")
axis(side = 2)
mtext("Stratigraphic Height [m]",
      side = 2,
      line = 2)

## -----------------------------------------------------------------------------
t = seq(0, 2, by = 0.001) # times
BM = function(t){
  #" Simulate Brownian motion at times t
  li = list("t" = t,
            "y" = cumsum(c(0, rnorm(n = length(t) - 1, mean = 0, sd = sqrt(diff(t))))))
  class(li) = c("timelist", "list") # assign class `timelist` for easy plotting, see ?plot.timelist
  return(li)
}
evo_list = BM(t)
plot(x = evo_list,
     xlab = "Time [Myr]",
     ylab = "Trait Value",
     type = "l")

## -----------------------------------------------------------------------------
strat_list = time_to_strat(obj = evo_list,
                            x = my_adm)
plot(x = strat_list,
     orientation = "lr",
     type = "l",
     xlab = "Stratigraphic Height [m]",
     ylab = "Trait Value",
     main = "Trait Evolution 2 km Offshore")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("admtools_doc")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("adm_plotting)

## ----eval=FALSE---------------------------------------------------------------
#  vignette("adm_from_sedrate")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("adm_from_trace_cont")

## ----eval=FALSE---------------------------------------------------------------
#  vignette("correlation")

