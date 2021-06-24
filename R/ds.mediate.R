#'
#' @title Causal Mediation Analysis
#' @description This function is similar to R function \code{mediate} from the 
#' \code{mediation} package.
#' @details The function 'mediate' is used to estimate various quantities for 
#' causal mediation analysis, including average causal mediation effects 
#' (indirect effect), average direct effects, proportions mediated, and total effect.
#' @param model.m a string character, the name of a fitted model object for mediator.
#' @param model.y a string character, the name of a fitted model object for outcome.
#' @param treat a character string indicating the name of the treatment variable used
#' in the models. The treatment can be either binary (integer or a two-valued factor)
#' or continuous (numeric).
#' @param mediator a character string indicating the name of the mediator variable
#' used in the models.
#' @param boot a logical value. if 'FALSE' a quasi-Bayesian approximation is used for
#' confidence intervals; if 'TRUE' nonparametric bootstrap will be used. Default is 'FALSE'.
#' @param conf.level the level of the returned two-sided confidence intervals. Default
#' is to return the 2.5 and 97.5 percentiles of the simulated quantities.
#' @param robustSE a logical value. If 'TRUE', heteroskedasticity-consistent standard
#' errors will be used in quasi-Bayesian simulations. Ignored if 'boot' is 'TRUE' or
#' neither 'model.m' nor 'model.y' has a method for vcovHC in the sandwich package. 
#' Default is 'FALSE'.
#' @param sims a number of Monte Carlo draws for nonparametric bootstrap or 
#' quasi-Bayesian approximation.
#' @param seed a number of a seed random number generator. Default value is NULL.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{med.out}.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a summary table of the object of class 'mediate'.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#' @import DSI
#'
ds.mediate <- function(model.m=NULL, model.y=NULL, treat = NULL, mediator = NULL, boot=FALSE,
                       conf.level=0.95, robustSE=FALSE, sims=1000, seed=NULL, newobj=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # verify that both model outcomes are provided
  if(is.null(model.m)){
    stop(" Please provide the name of the fitted model object for mediator!", call.=FALSE)
  }
  if(is.null(model.y)){
    stop(" Please provide the name of the fitted model object for outcome!", call.=FALSE)
  }

  treat.name <- treat
  med.name <- mediator
  
  if(is.null(newobj)){
    newobj <- 'med.out'
  }

  calltext <- call('mediateDS', model.m, model.y, treat.name, med.name, boot, 
                   conf.level, robustSE, sims, seed, newobj)
  study.summary <- DSI::datashield.aggregate(datasources, calltext)
  
  return(study.summary)

}  
