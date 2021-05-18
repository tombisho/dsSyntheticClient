#'
#' @title Expand the dataset and calculate ratio-of-mediator probability weights
#' @description This function is similar to R function \code{neWeight} from the 
#' \code{mmedflex} package.
#' @details The function 'neWeight' both expands the data along hypothetical exposure
#' values and calculates ratio-of-mediator probability weights.
#' @param object a string character, the name of an object used to select a method.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{neWeight.data}. 
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a data frame of class c("data.frame", "expData", "weightData") is assigned at
#' the server-side of each study.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
ds.neWeight <- function(object=NULL, newobj=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # verify that model outcomes are provided
  if(is.null(object)){
    stop(" Please provide the name of the fitted model object!", call.=FALSE)
  }
  
  if(is.null(newobj)){
    newobj <- 'neWeight.data'
  }
  
  calltext <- call('neWeightDS', object)
  DSI::datashield.assign(datasources, newobj, calltext)

}  
  
  