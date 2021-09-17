#'
#' @title Synthetic dataset generation
#' @description This function is similar to R function \code{syn} from the 
#' \code{synthpop} package.
#' @details The function 'syn' is used to generate one or more synthetic datasets 
#' from an observed dataset. There is no disclosure risk from allowing the seed to vary because repeated calls to generate a dataset
#' should not reproduce the original data.
#' @param data a string that is the name of a dataframe containing the variables to be included in the synthetic dataset
#' @param ... further arguments passed to \code{syn}
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a synthetic dataset.
#' @author Tom Bishop
#' @export
#' @import DSI
#'
ds.syn2 <- function(data=NULL, ..., datasources=NULL){
  
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
  
  arguments = list(...)
  
  #calltext <- call('synDS2', data, arguments)
  calltext <- paste0('synDS2("', data, '","',  arguments, '")')
  study.summary <- DSI::datashield.aggregate(datasources, calltext)
  
  return(study.summary)
  
}  
