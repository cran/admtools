is_destructive = function(adm, t, mode = "rcll", bdry_pts_hiat = "destructive", out_dom_mode = "default"){
  
  #' @export
  #' 
  #'
  #' @title Is deposition destructive?
  #' 
  #' @description
  #' Determines whether specified time is destrucive or not
  #' 
  #' 
  #' @usage is_destructive(adm, t, mode = "rcll", 
  #' bdry_pts_hiat = "destructive", out_dom_mode = "default")
  #' 
  #' @param adm an adm object
  #' @param t vector of times
  #' @param mode string, either "rcll", "lcrl", "open", or "closed"
  #' @param bdry_pts_hiat string, "destructive" or "consistent". If the adm starts/ends with a hiatus, should the 
  #' start/end be removed, or treated consistently with mode?
  #' @param out_dom_mode ""default", "destructive", or "conservative" 
  #' 
  #' @returns logical vector of same length as t. Is deposition at time t destructive?
  
  UseMethod("is_destructive")


}
