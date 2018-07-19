#' Title
#'
#' @param data 
#' @param similar 
#' @param hierarchy 
#' @param risk 
#' @param hid 
#' @param th 
#' @param swaprate 
#' @param seed 
#'
#' @return
#' @export
#'
#' @examples
recordSwap <- function(data, similar, hierarchy, risk, hid, th, swaprate, seed = 123456L){
  recordSwap_cpp(data, similar, hierarchy, risk, hid, th, swaprate, seed)
}