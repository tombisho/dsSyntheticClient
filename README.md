# dsSyntheticClient

`dsSyntheticClient` is a DataSHIELD client side package for generating synthetic data.

DataSHIELD is a client-server framework for privacy preserving computation.

Please also look at the server side package `dsSynthetic`

https://github.com/tombisho/dsSynthetic


## Quick start

```r

install.packages('devtools')
library(devtools)

devtools::install_github('tombisho/dsSyntheticClient')

```

Please see [this bookdown](https://tombisho.github.io/synthetic_bookdown/) for detailed guidance 

This uses the Opal demo server which has all server side packages installed

https://opal-sandbox.mrc-epid.cam.ac.uk/




## Installation



* Install R Studio and the development environment as described below:

    * https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/12943461/Getting+started


* Install the virtual machines as described below:

    * https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/931069953/Installation+Training+Hub-+DataSHIELD+v6

    * https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/1657634881/Testing+100+VM

    * https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/1657634898/Tutorial+6.1.0+100+VM

* Install dsBase and dsSynthetic on Opal server in the Virtual Machine (type tombisho/dsSynthetic and main in the textboxes) 

See the link below on how to install a package in Opal

https://opaldoc.obiba.org/en/latest/web-user-guide/administration/datashield.html#add-package



* Install the package in R

```r

install.packages('devtools')
library(devtools)

devtools::install_github('tombisho/dsSyntheticClient')

```


* Follow the bookdown which as executable code and synthetic data

https://tombisho.github.io/synthetic_bookdown/


This uses the Opal demo server which has all server side packages installed

https://opal-sandbox.mrc-epid.cam.ac.uk/




## Usage

Please see [this bookdown](https://tombisho.github.io/synthetic_bookdown/) for detailed guidance 

This uses the Opal demo server which has all server side packages installed

https://opal-sandbox.mrc-epid.cam.ac.uk/

## Acknowledgements

Thanks to the DataSHIELD team for providing the plaform on which these functions are based.

Thanks to OBiBa and Epigeny for the Opal data warehouse which we use to run DataSHIELD

## Contact

Tom R.P. Bishop and Soumya Banerjee

sb2333@cam.ac.uk


## Citation

If you like or use this work, please cite the following manuscript

Banerjee S, Bishop TRP. dsSynthetic: Synthetic data generation for the DataSHIELD federated analysis system. BMC Res. Notes. 2022;15 (1) :230

https://bmcresnotes.biomedcentral.com/articles/10.1186/s13104-022-06111-2

or

dsSynthetic: Synthetic data generation for the DataSHIELD federated analysis system

https://osf.io/tkxqm/


