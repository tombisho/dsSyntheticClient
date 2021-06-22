#'
#' @title Significance Test for Treatment-Mediator Interaction in Causal Mediation Analysis
#' @description This function is similar to R function \code{test.TMint} from the 
#' \code{mediation} package.
#' @details The function 'test.TMint' is used to test whether the average causal mediation
#' effects and direct effects are significantly different between the treatment and control
#' conditions.
#' @param x the name of the output of the \code{ds.mediate} function saved at each server.
#' @param conf.level the level of the returned two-sided confidence intervals for the effect
#' differences. By default it is set to the value used in the original mediate call.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return an object of class "htest" when applied to a mediate object. See t.test for more
#' explanations of the contents. The function returns an object of class "htest.order" which
#' has its own print method included in this package.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
ds.test.TMint <- function(x=NULL, conf.level=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # verify that the name of the output object saved at each server by ds.mediate function, is provided
  if(is.null(x)){
    stop(" Please provide the name of the assigned output object from the ds.mediate function!", call.=FALSE)
  }

  calltext <- call('test.TMintDS', x, conf.level)
  out <- DSI::datashield.aggregate(datasources, calltext)
  
  return(out)

}  
