init.studies.dataset.cnsim <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
        if (ds.test_env$driver == "OpalDriver")
        {
            builder <- DSI::newDSLoginBuilder(.silent = TRUE)
            builder$append(server = "sim1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "CNSIM.CNSIM1")
            builder$append(server = "sim2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "CNSIM.CNSIM2")
            builder$append(server = "sim3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "CNSIM.CNSIM3")
            ds.test_env$login.data <- builder$build()
        }
        else 
        {
            ds.test_env$login.data <- DSLite::setupCNSIMTest("dsBase", env = ds.test_env)
        }
        ds.test_env$stats.var <- variables
    }
}

connect.studies.dataset.cnsim <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.studies.dataset.cnsim(variables)
    log.in.data.server()
}

disconnect.studies.dataset.cnsim <- function()
{
    log.out.data.server()
}
