quantile_adm = function(x, h, p){
  #' @export
  #' @title get quantile ADM
  #' 
  #' @description
  #' returns the quantile adm of a multiadm object
  #' 
  #' @param x a multiadm object
  #' @param h the heights at which to evaluate the adm
  #' @param p percentile, as number between 0 and 1
  #' 
  #' @seealso [median_adm()] to extract the median adm, [mean_adm()] for the mean adm
  #' 
  #' @returns an adm object
  
  h_list = get_time(x, h)
  h_t= list()
  for ( i in seq_along(h)){
    h_t[[i]] = sapply(h_list, function(x) x[i])
  }
  t = sapply(h_t, function(x) stats::quantile(x, p, na.rm = TRUE))
  adm = tp_to_adm(t = t, h = h, T_unit = get_T_unit(x), L_unit = get_L_unit(x))
  return(adm)
}

median_adm = function(x, h){
  #' @export
  #' @title get median ADM
  #' 
  #' @description
  #' returns the median adm of a multiadm object
  #' 
  #' @param x a multiadm object
  #' @param h the heights at which to evaluate the adm
  #' 
  #' @seealso [mean_adm()] for the mean age-depth model, [quantile_adm()] for the more general implementation 
  #' 
  #' @returns an adm object
  
  h_list = get_time(x, h)
  h_t= list()
  for ( i in seq_along(h)){
    h_t[[i]] = sapply(h_list, function(x) x[i])
  }
  t = sapply(h_t, function(x) stats::quantile(x, 0.5, na.rm = TRUE))
  adm = tp_to_adm(t = t, h = h, T_unit = get_T_unit(x), L_unit = get_L_unit(x))
  return(adm)
}

mean_adm = function(x, h){
  #' @export
  #' @title get mean ADM
  #' 
  #' @description
  #' returns the mean adm of a multiadm object
  #' 
  #' @param x a multiadm object
  #' @param h the heights at which to evaluate the adm
  #' 
  #' @seealso [median_adm()] and [quantile_adm()] for median and quantile adms, respectively
  #' 
  #' @returns an adm object
  
  h_list = get_time(x, h)
  h_t= list()
  for ( i in seq_along(h)){
    h_t[[i]] = sapply(h_list, function(x) x[i])
  }
  t = sapply(h_t, function(x) base::mean(x, na.rm = TRUE))
  adm = tp_to_adm(t = t, h = h, T_unit = get_T_unit(x), L_unit = get_L_unit(x))
  return(adm)
}