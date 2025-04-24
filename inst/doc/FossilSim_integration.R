## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(admtools)

## -----------------------------------------------------------------------------
set.seed(42)
# construct age-depth model
my_adm = tp_to_adm(t = CarboCATLite_data$time_myr,
                  h = CarboCATLite_data$height_2_km_offshore_m,
                  L_unit = "m",
                  T_unit = "Myr")
# simulate phylogenetic tree
t = ape::rbdtree(birth = 3, death = 1, Tmax = 2)
# simulate taxonomy along the tree
s = FossilSim::sim.taxonomy(tree = t)
# simulate fossils based on taxonomy
f = FossilSim::sim.fossils.poisson(rate = 4, taxonomy = s)
# plot tree with taxonomy and fossil times
FossilSim:::plot.fossils(f, tree = t, taxonomy = s, show.taxonomy = TRUE)

## transform everything into the strat domain
t_strat = time_to_strat(t, my_adm)
s_strat = time_to_strat(s, my_adm, destructive = FALSE)
f_strat = time_to_strat(f, my_adm, destructive = FALSE)
FossilSim:::plot.fossils(f_strat, tree = t_strat, taxonomy = s_strat, show.taxonomy = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  vignette("paleotree", package = "FossilSim")

