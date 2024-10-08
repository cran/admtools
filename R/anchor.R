anchor = function(x, index = "last", t_anchor = NULL, n = 1000L){
  #' @export
  #' 
  #' @title anchor age-depth model
  #' 
  #' @param x age-depth model
  #' @param index "last" or "first", or an integer (marked by L, e.g. 2L), specifying at which tie point the age-depth model will be anchored. If i is passed as integer, the i-th tie point is anchored.
  #' @param t_anchor time at which the adm is anchored. must be a function that takes no arguments and returns the timing of the tie point. see example or vignettes for details
  #' @param n integer, number of samples drawn from the tie point
  #' 
  #' @description
    #' anchors a deterministic age-depth model (adm object) at a tie point that is associated with uncertainty. 
    #' 
    #' 
  #' 
  #' @returns a collection of age-depth models (a multiadm object)
  #' 
  #' @examples
    #' t_anchor = function() rnorm(1) # normally distributed uncertainty
    #' x = tp_to_adm(t = c(1,2, 3), h = c(2,3, 4)) # simple age-depth model
    #' m = anchor(x, index = "last", t_anchor = t_anchor, n = 100) # anchor age-depth model
    #' plot(m)
    #' m = anchor(x, index = 2L, t_anchor = t_anchor, n = 100)
    #' plot(m)
    #' 
    #' 
  if (! (index %in% c("last", "first") | is.integer(index))){
    stop("Invalid input for index. Use `first`, `last`, or an integer.")
  }
  
  if (! (is.function(t_anchor))){
    stop("expect functions as inputs for t_anchor")
  }
  adm_list = list()
  if (index == "last"){
    t_ref = max_time(x)
  }
  if (index == "first"){
    t_ref = min_time(x)
  }
  if (is.integer(index)){
    t = get_T_tp(x)
    l = length(t)
    if (index > l | index < 1){
      stop(paste0("index must be between 1 and ", l, " or character" ))
    }
    t_ref = t[index]
  }
  
  for (i in seq_len(n)){
    t_offset = t_anchor()
    t = get_T_tp(x) - t_ref + t_offset
    h = get_L_tp(x)
    L_unit = get_L_unit(x)
    T_unit = get_T_unit(x)
    adm_list[[i]] = tp_to_adm(t, h, T_unit, L_unit)
  }
  
  multiadm = list("t" = list(),
                  "h" = list(),
                  "destr" = list(),
                  "no_of_entries" = length(adm_list),
                  "T_unit" = get_T_unit(x),
                  "L_unit" = get_L_unit(x))
  
  for (i in seq_along(adm_list)){
    adm = adm_list[[i]]
    multiadm[["t"]][[i]] = adm$t
    multiadm[["h"]][[i]] = adm$h
    multiadm[["destr"]][[i]] = adm$destr
  }
  class(multiadm) = "multiadm"
  return(multiadm)
}