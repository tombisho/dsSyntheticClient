#'
#' @title Fit a natural effect model
#' @description This function is similar to R function \code{neModel} from the 
#' \code{medflex} package.
#' @details The function 'neModel' is used to fit a natural effect model on the
#' expanded dataset.
#' @param formula a formula object providing a symbolic description of the 
#' natural effect model.
#' @param family aa description of the error distribution and link function to be
#' used in the model. For glm this can be a character string naming a family 
#' function, a family function or the result of a call to a family function. 
#' For glm.fit only the third option is supported.
#' @param expData the expanded dataset (of class "expData").
#' @param se character string indicating the type of standard errors to be calculated.
#' The default type is based on the bootstrap.
#' @param nBoot number of bootstrap replicates.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{neModel.out}.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a summary table of the object of class 'neModel'.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
ds.neModel <- function(formula=NULL, family=NULL, expData=NULL, se="bootstrap", nBoot=1000, newobj=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # verify that 'formula' was set
  if(is.null(formula)){
    stop(" Please provide a valid regression formula!", call.=FALSE)
  }
  
  formula <- stats::as.formula(formula)
  
  # check that 'family' was set
  if(is.null(family)){
    stop(" Please provide a valid 'family' argument!", call.=FALSE)
  }
  
  # if the argument 'expData' is set, check that the data frame is defined (i.e. exists) on the server side
  if(!(is.null(expData))){
    defined <- isDefined(datasources, expData)
  }
  
  # check if 'se' is either 'bootstrap' or 'robust'
  if(!(se %in% c("bootstrap", "robust"))){
    stop(" Argument 'se' must be either 'bootstrap' or 'robust'!", call.=FALSE)
  }
  
  if(is.null(newobj)){
    newobj <- 'neModel.out'
  }
  
  calltext <- call('neModelDS', formula, family, expData, se, nBoot, newobj)
  out <- DSI::datashield.aggregate(datasources, calltext)
  
  return(out)
  
}   
  
  