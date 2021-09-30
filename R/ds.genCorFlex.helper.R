#' @title Function to generate synthetic data from private data on server using the package simstudy.
#' @description This is a function to generate synthetic data from private data on server. The function returns this synthetic data
#'          which can be used by the analyst to perform testing of code, visualization of data (on analyst computer) and testing of
#'          harmonization code.
#' @details This is a function to generate synthetic data from private data on server. The function returns this synthetic data
#'          which can be used by the analyst to perform testing of code, visualization of data (on analyst computer) and testing of
#'          harmonization code.
#' @param dataframe the name of the dataframe that will be used to generate a synthetic dataset. Required.
#' @param cont_variables a list of continuous variables.
#' @param factor_variables a list of factor variables. Multi level factors must be converted into binaries (a limitation of simstudy). The function XXXX can help with this.
#' @param num_rows number of rows of synthetic data to be generated. Default is 10000.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login to DataSHIELD.
#' @return a data frame of synthetic data
#' @author Soumya Banerjee and Tom R. P. Bishop (2021)
#' @examples
#' \dontrun{
#'      
#'    # The login data object is a data.frame
#'    str_table_name = "DASIM.DASIM1"  # "DASIM.DASIM3"
#'    builder <- DSI::newDSLoginBuilder()
#'    builder$append(server="server1", url="http://192.168.56.100:8080",
#'                     user="administrator", password="datashield_test&", table = str_table_name)#table = "TESTING.DATASET1")
#'    logindata <- builder$build()
#'
#'    # Then perform login in each server
#'    library(DSOpal)
#'    library(dsBaseClient)
#'    datashield.logout(conns = connections)
#'    connections <- datashield.login(logins=logindata, assign = TRUE)
#'
#'    cont_vars = c("LAB_TSC", "LAB_TRIG", "LAB_HDL", "LAB_GLUC_FASTING", "PM_BMI_CONTINUOUS")
#'    factor_vars = c("DIS_CVA", "DIS_DIAB", "DIS_AMI")
#'    synthetic_data <- generate_synthetic_data_simstudy(connections = connections, list_cont_variables = cont_vars, list_factor_variables = factor_vars)

#'    datashield.logout(conns = connections)
#' }
#'
#' @export
ds.genCorFlex.helper <- function(dataframe = NULL, cont_variables, factor_variables, num_rows = 10000, datasources = NULL) 
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
  if(length(datasources) > 1){
    stop("The 'datasources' argument has multiple entries. This function is designed to generate a synthetic dataset for a single server. Please consider providing a single server connection using square brackets or the dollar notation: e.g. connections[1] or connections$server1", call.=FALSE)
  }
  
  # if both list_factor_variables and list_cont_variables null then give error
  if ( is.null(cont_variables) & is.null(factor_variables) )
  {
    stop("There must be at least one parameter: a list of continuous variables or a list of factor variables ", call.=FALSE) 
  }
  
  # if dataframe name is null then give error
  if ( is.null(dataframe) )
  {
    stop("Please provide the name of a dataframe to generate synthetic data ", call.=FALSE) 
  }  
  
  # check factors have been converted to binary - this is required
  if ( !is.null(factor_variables))
  {  
    for(var in factor_variables){
      my_levels = ds.levels(paste0(dataframe, "$", var), datasources)[[1]]$Levels
      my_length = length(my_levels)
      if (my_length > 2){
        stop(paste0("The variable ", var, " is not a binary factor. Please use the function XXXXX to decompose factors with multiple levels into binary factors."), call.=FALSE) 
      }

    }
  }
  
  # check that variables exist in all studies
  #TODO
  
  # first case - factors present

  if ( !is.null(factor_variables))
  {  
    
    for(var in factor_variables)
    {
      # convert factor to numeric
      ds.asNumeric(paste0(dataframe,"$",var), newobj = paste0(var,"_num"), datasources = datasources)
      if(!ds.exists(x="facD", datasources = datasources)[[1]]){
        ds.dataFrame(x=c(paste0(var,"_num")), newobj = "facD", datasources = datasources)
      }
      else {
        ds.dataFrame(x=c("facD", paste0(var,"_num")), newobj = "facD", datasources = datasources)
      }
    }
    factor_variables = paste0(factor_variables,"_num")
  }  
  
  # second case - continuous only
  
  if (!is.null(cont_variables) )
  {
    
    ds.subset(x=dataframe, subset='numD', cols=cont_variables, datasources = datasources)
  }  
  
  # combine dataframes
  if ( !is.null(factor_variables) & !is.null(cont_variables)){
    ds.dataFrame(x=c("facD", "numD"), newobj = "final", datasources = datasources)
  }
  else if (!is.null(factor_variables)) {
    ds.dataFrame(x=c("facD"), newobj = "final", datasources = datasources)
  }
  else if (!is.null(cont_variables)) {
    ds.dataFrame(x=c("numD"), newobj = "final", datasources = datasources)
  }
  
  
  # Now we use `ds.cor()` to generate a correlation matrix. Similarly, `ds.mean()` and `ds.var()` are used to obtain the means and variances:
  
  Dnames = ds.colnames("final", datasources = datasources)[[1]]
  
  corrs = ds.cor(x="final", datasources = datasources)[[1]]$`Correlation Matrix`
  colnames(corrs) <- Dnames
  rownames(corrs) <- Dnames
  means = numeric()
  vars = numeric()
  
  for (var in Dnames)
  {
    means = c(means, ds.mean(paste0("final$",var), datasources = datasources)$Mean.by.Study[1])
    vars = c(vars, ds.var(paste0("final$",var), datasources = datasources)$Variance.by.Study[1])
  }
  
  # Once we have the summary information, we use it to build a **simstudy** definition table. This is then used to generate the synthetic data.
  
  for (i in 1:length(Dnames))
  {
    if (Dnames[i] %in% cont_variables){
      distribution = "normal"
    }
    if (Dnames[i] %in% factor_variables){
      distribution = "binary"
    }
    if(!exists("def")){
      def <- defData(varname = Dnames[i], formula = means[i], variance = vars[i], dist = distribution)
    }
    else {
      def <- defData(def, varname = Dnames[i], formula = means[i], variance = vars[i], dist = distribution)
    }
    
  }
  
  # generate synthetic data
  dd <- genCorFlex(num_rows, def, rho = NULL, tau = NULL, corMatrix = corrs)
  
  
  # tidy up
  
  ds.rm(x.names = c("final", "facD", "numD", factor_variables), datasources)
  
  return(dd)
  
}