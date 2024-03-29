% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.genCorFlex.helper.R
\name{ds.genCorFlex.helper}
\alias{ds.genCorFlex.helper}
\title{Function to generate synthetic data from private data on server using the package simstudy.}
\usage{
ds.genCorFlex.helper(
  dataframe = NULL,
  cont_variables,
  factor_variables,
  num_rows = 10000,
  datasources = NULL
)
}
\arguments{
\item{dataframe}{the name of the dataframe that will be used to generate a synthetic dataset. Required.}

\item{cont_variables}{a list of continuous variables.}

\item{factor_variables}{a list of factor variables. Multi level factors must be converted into binaries (a limitation of simstudy). The function XXXX can help with this.}

\item{num_rows}{number of rows of synthetic data to be generated. Default is 10000.}

\item{datasources}{a list of \code{\link{DSConnection-class}} objects obtained after login to DataSHIELD.}
}
\value{
a data frame of synthetic data
}
\description{
This is a function to generate synthetic data from private data on server. The function returns this synthetic data
         which can be used by the analyst to perform testing of code, visualization of data (on analyst computer) and testing of
         harmonization code.
}
\details{
This is a function to generate synthetic data from private data on server. The function returns this synthetic data
         which can be used by the analyst to perform testing of code, visualization of data (on analyst computer) and testing of
         harmonization code.
}
\examples{
\dontrun{
     
   # The login data object is a data.frame
   str_table_name = "DASIM.DASIM1"  # "DASIM.DASIM3"
   builder <- DSI::newDSLoginBuilder()
   builder$append(server="server1", url="http://192.168.56.100:8080",
                    user="administrator", password="datashield_test&",
                     table = str_table_name)#table = "TESTING.DATASET1")
   logindata <- builder$build()

   # Then perform login in each server
   library(DSOpal)
   library(dsBaseClient)
   datashield.logout(conns = connections)
   connections <- datashield.login(logins=logindata, assign = TRUE)

   cont_vars = c("LAB_TSC", "LAB_TRIG", "LAB_HDL", "LAB_GLUC_FASTING", "PM_BMI_CONTINUOUS")
   factor_vars = c("DIS_CVA", "DIS_DIAB", "DIS_AMI")
   synthetic_data <- generate_synthetic_data_simstudy(connections = connections,
    list_cont_variables = cont_vars, list_factor_variables = factor_vars)
   datashield.logout(conns = connections)
}

}
\author{
Soumya Banerjee and Tom R. P. Bishop (2021)
}
