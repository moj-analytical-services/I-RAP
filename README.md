# iRAP

Process data into a consistent format compatible with Explore Justice Statistics

## Overview

The iRAP (integrated RAP) package aims to provide a single package that allows multiple MoJ statistical data sources to be processed according to a common set of code into a standardised format. The final format output is compatible with the Explore Education Statistics (EES) platform (https://explore-education-statistics.service.gov.uk/) from which we aim to develop an MoJ 'Explore Justice Statistics' version.

Currently, iRAP works with the following data sources:
  - Prison population (partial)
  - Prison receptions (partial)
  - Prison releases (partial)

### Installation

```r
devtools::install_github("moj-analytical-services/I-RAP")
```

## Core functions:

  -  [iRAP_build_data](#iRAP_build_data)
  -  [iRAP_build_table](#iRAP_build_table)
  
Each of the core functions accepts a large number of arguments to enable to processing of a range of different datasets. Descriptions of each argument can be found by accessing the relevant Help file in R Studio.
  
### iRAP_build_data

Renders input data into an iRAP format data frame. The iRAP data format is largely analogous to Tidy data.

### iRAP_build_table

Renders an iRAP format data frame into an aggregated data table in a format suitable for upload to EES.

## Source-specific functions:

  -  [prison_pop_data](#prison_pop_data)
  -  [prison_pop_tables](#prison_pop_tables)
  -  [prison_receptions_data](#prison_receptions_data)
  -  [prison_receptions_tables](#prison_receptions_tables)
  -  [prison_releases_data](#prison_releases_data)
  -  [prison_releases_tables](#prison_releases_tables)
  
Source-specific functions are specific forms of the core functions in which the arguments have been pre-set to values needed to process a particular data source, or generate a pre-defined set of tables.

Each function includes two arguments:
  - **dates**: A vector of dates corresponding to the time period of data required
  - **SHA**: A SHA corresponding to the version of lookup tables used to generate the data. This is used to ensure outputs created by the package can be recreated even if the lookup files change.
  
### prison_pop_data

```r
prison_pop_data(dates=c("20200331","20200630","20200930","20201231"), SHA="main")
```

A specific form of iRAP_build_data with preset parameters for processing the prison population dataset used in OMSQ. It outputs an iRAP format dataset.

### prison_pop_tables

```r
prison_pop_tables(dates=c("20200331","20200630","20200930","20201231"), SHA="main")
```

A specific form of iRAP_build_tables with preset parameters for processing the prison population dataset used in OMSQ. It outputs a list of data frames, each of which can be exported as a CSV and uploaded to EES.

This function runs prison_pop_data. It does not need to be run separately. 

### prison_receptions_data

```r
prison_receptions_data(dates=c("2020q1","20200q2","2020q3","2020q4"), SHA="main")
```

A specific form of iRAP_build_data with preset parameters for processing the prison receptions dataset used in OMSQ. It outputs an iRAP format dataset.

### prison_receptions_tables

A specific form of iRAP_build_tables with preset parameters for processing the prison receptions dataset used in OMSQ. It outputs a list of data frames, each of which can be exported as a CSV and uploaded to EES.

This function runs prison_receptions_data. It does not need to be run separately. 

```r
prison_receptions_tables(dates=c("2020q1","20200q2","2020q3","2020q4"), SHA="main")
```

### prison_releases_data

```r
prison_releases_data(dates=c("2020q1","20200q2","2020q3","2020q4"), SHA="main")
```

A specific form of iRAP_build_data with preset parameters for processing the prison releases dataset used in OMSQ. It outputs an iRAP format dataset.

### prison_releases_tables

A specific form of iRAP_build_tables with preset parameters for processing the prison releases dataset used in OMSQ. It outputs a list of data frames, each of which can be exported as a CSV and uploaded to EES.

This function runs prison_releases_data. It does not need to be run separately. 

```r
prison_releases_tables(dates=c("2020q1","20200q2","2020q3","2020q4"), SHA="main")
```



