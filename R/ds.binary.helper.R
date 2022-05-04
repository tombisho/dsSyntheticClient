#' @title Function to create dummy variables from factor variables
#' @description This is a function to assist in converting several multi-level factor variables into dummy or binary variables
#' @details This function has been written as a helper for the ds.genCorFlex.helper, which currently does not accept multi level factors.
#' @param dataframe the name of the dataframe that will be used to generate a synthetic dataset. Required.
#' @param factor_variables a vector of multi level factor variable names to be converted to binary
#' @param newobj character string, the name of the new dataframe to be created that holds the binaries.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login to DataSHIELD - should be a single server
#' @return a data frame of binary variables on the server side.
#' @author Tom R. P. Bishop (2021)

#'
#' @export
ds.binary.helper <- function(dataframe = NULL, factor_variables, newobj = NULL, datasources = NULL) 
{
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # this function only works on a single study at the moment
  # TODO improve this. First check if all objects are all factors in all studies, then check levels match.
  # TODO simpler would be a check of existence of variables in the single study
  if(length(datasources) > 1){
    stop("The 'datasources' argument has multiple entries. This function is designed to generate a set of binary variables for a single server. Please consider providing a single server connection using square brackets or the dollar notation: e.g. connections[1] or connections$server1", call.=FALSE)
  }
  
  # if both factor_variables or dataframe null then give error
  if ( is.null(factor_variables) | is.null(dataframe)  )
  {
    stop("There must be  a list of factor variables and a data frame", call.=FALSE) 
  }

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "asbinary.newobj"
  }
  
  #keep track of variables created so they can be put into a dataframe
  my_names = c()
  
  for(var in factor_variables){
    my_levels = dsBaseClient::ds.levels(paste0(dataframe, "$",var), datasources)[[1]]$Levels
    my_length = length(my_levels)
    if (my_length > 2){ # no need to deal with variables that are already binary
      # ds.asFactor (1) generates a matrix, which is not so easy to work with, and (2) creates the dummy variables as numeric (3) gives the dummy variables the same name every time
      factorInfo = dsBaseClient::ds.asFactor(input.var.name = paste0(dataframe, "$", var), fixed.dummy.vars = TRUE, newobj.name = "dummy", datasources = datasources)
      my_length = length(factorInfo$all.unique.levels) - 1
      new_levels = list(NULL,paste0(c(1:my_length),"_",var))
      mat_name = "temp_mat"
      dsBaseClient::ds.matrixDimnames(M1="dummy", dimnames = new_levels, newobj=mat_name, datasources = datasources)
      #needs converting back into dataframeS
      dsBaseClient::ds.dataFrame(x = mat_name, newobj = mat_name, datasources = datasources)
      #convert to factors
      cols = dsBaseClient::ds.colnames(x=mat_name, datasources = datasources)[[1]]
      for(fac_name in cols){
        dsBaseClient::ds.asFactor(input.var.name = paste0(mat_name, "$",fac_name), newobj.name = fac_name, datasources = datasources)
      }
      my_names = c(my_names, cols)
    }
    else {
      #tidy up
      dsBaseClient::ds.rm(x.names = c("dummy", my_names, mat_name), datasources)
      stop(paste0("This function is intended to convert multi-level factors into binaries, but the variable ", var, " is already binary. Please remove it from the input vector."), call.=FALSE)
    }
    
  }
  
  dsBaseClient::ds.dataFrame(x = my_names, newobj = newobj, datasources = datasources)
  
  #tidy up
  dsBaseClient::ds.rm(x.names = c("dummy", my_names, mat_name), datasources)
}