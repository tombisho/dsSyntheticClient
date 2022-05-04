#'
#' @title Synthetic dataset generation
#' @description This function is similar to R function \code{syn} from the 
#' \code{synthpop} package.
#' @details The function 'syn' is used to generate one or more synthetic datasets 
#' from an observed dataset. See details in the \code{synthpop} package.
#' @param data a string that is the name of a dataframe containing the variables to be included in the synthetic dataset
#' @param method a single string or a vector of strings of length ncol(data)
#' specifying the synthesising method to be used for each variable in the data.
#' Order of variables is exactly the same as in data.
#' Currently this is overwritten to CART for disclosure protection
#' @param m number of synthetic copies of the original (observed) data to be generated. The default is m = 1.
#' @param k number of rows of data to be generated
#' @param proper a logical value with default set to FALSE. If TRUE proper synthesis is conducted.
#' @param seed an integer to be used as an argument for the set.seed(). If no integer is provided, the default "sample" will generate one and it will be stored. To prevent generating an integer set seed to NA.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a synthetic dataset.
#' @author Tom Bishop
#' @export
#' @import DSI
#'
ds.syn <- function(data=NULL, method="cart", m = 1, k = NULL,
                   proper = FALSE, seed = "sample", datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # verify that both model outcomes are provided
  if(is.null(data)){
    stop(" Please provide the name of the observed data frame!", call.=FALSE)
  }
  
  #rest of variables passed through as-is, NULLS fixed on server side
  calltext <- call('synDS', data, method,m, k, proper, seed)
  study.summary <- DSI::datashield.aggregate(datasources, calltext)
  
  return(study.summary)

}  
