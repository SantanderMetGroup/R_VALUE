THIS PACKAGE IS DEPRECATED
==========================

Please visit the [VALUE package repository](https://github.com/SantanderMetGroup/VALUE) for an up-to-date set of indices and measures, implementing and extending the VALUE validation framework  

<!--
The R.VALUE package
===================

The main objective of the [VALUE](http://www.value-cost.eu) COST action is the validation and integration of downscaling methods for climate change research. To this aim, a number of [indices and measures](http://www.value-cost.eu/reports) have been identified in order to validate different aspects regarding the performance of the downscaling methods. These indices have been implemented in R by the [VALUE cross-cutting group](http://www.value-cost.eu/cross-cutting) and are collected in this public package for further collaboration and extension with other initiatives, as well as research reproducibility.   

The package includes R functions used to read the observational datasets (and output downscaled predictions) in [VALUE data format](http://www.value-cost.eu/WG2/stationdataformat) as well as the auxiliary (and wrapper) functions used by the [VALUE validation portal](http://www.value-cost.eu/validationportal) to compute these indices. The data structures are integrated with other climate data access and analysis tools namely [loadeR](https://github.com/SantanderMetGroup/loadeR), for local and remote data access (for instance to the Santander MetGroup User Data Gateway, [UDG](http://www.meteo.unican.es/en/dataservices)) and [downscaleR](https://github.com/SantanderMetGroup/downscaleR), a R package for bias correction and statistical downscaling.

[climate4R.value](https://github.com/SantanderMetGroup/climate4R.value) provides a wrapper of this package for integration into the [climate4R framework](https://github.com/SantanderMetGroup/climate4R).

### Package installation

A direct method for installing the most recent stable release requires the package `devtools`. Within R, just type:

```r
devtools::install_github("SantanderMetGroup/R_VALUE")
```

Alternatively, you can download the sources from the [releases tab](https://github.com/SantanderMetGroup/R_VALUE/releases)

Once, installed, for a quick overview:

```r
library(R.VALUE)
help(package="R.VALUE")
```

