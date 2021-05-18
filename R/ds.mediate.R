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
#' @param robustSE a logical value. If 'TRUE', heteroskedasticity-consistent standard
#' errors will be used in quasi-Bayesian simulations. Ignored if 'boot' is 'TRUE' or
#' neither 'model.m' nor 'model.y' has a method for vcovHC in the sandwich package. 
#' Default is 'FALSE'.
#' @param sims a number of Monte Carlo draws for nonparametric bootstrap or 
#' quasi-Bayesian approximation.
#' @param seed a number of a seed random number generator. Default value is NULL.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a summary table of the object of class 'mediate'.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
ds.mediate <- function(model.m=NULL, model.y=NULL, treat = NULL, mediator = NULL, boot=FALSE,
                       robustSE=FALSE, sims=1000, seed=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
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

  calltext <- call('mediateDS', model.m, model.y, treat.name, med.name, boot=boot, robustSE=robustSE, sims=sims,
                   seed=seed)
  study.summary <- DSI::datashield.aggregate(datasources, calltext)
  
  return(study.summary)

}  
  
  