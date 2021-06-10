#'
#' @title Estimation and Sensitivity Analysis for Multiple Causal Mechanisms
#' @description This function is similar to R function \code{multimed} from the 
#' \code{mediation} package.
#' @details The function 'multimed' is used for causal mediation analysis when
#' post-treatment mediator-outcome confounders, or alternative mediators causally
#' preceding the mediator of interest, exist in the hypothesized causal mechanisms.
#' It estimates the average causal mediation effects (indirect effects) and the
#' average direct effects under the homogeneous interaction assumption based on a
#' varying-coefficient linear structural equation model. The function also performs
#' sensitivity analysis with respect to the violation of the homogenous interaction
#' assumption. The function can be used for both the single experiment design and
#' the parallel design.
#' @param outcome a string character, the name of the outcome variable in 'data'.
#' @param med.main a string character, the name of the mediator of interest. Under
#' the parallel design this is the only mediator variable used in the estimation.
#' @param med.alt vector of character strings indicating the names of the 
#' post-treatment confounders, i.e., the alternative mediators affecting both the
#' main mediator and outcome. Not needed for the parallel design.
#' @param treat a string character, the name of the treatment variable in 'data'.
#' @param covariates vector of character strings representing the names of the
#' pre-treatment covariates. Cannot be used for the parallel design.
#' @param data a string character, the name of data frame containing all the 
#' above variables.
#' @param sims a number of bootstrap samples used for the calculation of 
#' confidence intervals.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a summary table of the object of class 'multimed'.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
ds.multimed <- function(outcome = NULL, med.main = NULL, med.alt = NULL, treat = NULL, covariates = NULL,
                        data = NULL, sims = 1000, datasources = NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # verify that outcome, med.main, treat variables are provided
  if(is.null(outcome)){
    stop(" Please provide the name of the outcome variable!", call.=FALSE)
  }
  if(is.null(med.main)){
    stop(" Please provide the name of the mediator of interest!", call.=FALSE)
  }
  if(is.null(treat)){
    stop(" Please provide the name of the treatment variable!", call.=FALSE)
  }
  
  outcome.name <- outcome
  med.main.name <- med.main
  treat.name <- treat
  data.name <- data
  covariates.transmit <- paste0(as.character(covariates), collapse=",")
  
  
  calltext <- call('multimedDS', outcome.name, med.main.name, med.alt, treat.name, covariates.transmit, data, sims)
  study.summary <- DSI::datashield.aggregate(datasources, calltext)
  
  return(study.summary)
  
}  

