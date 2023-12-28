data.tools: Tools for data manipulation
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

Package that includes functions for data manipulation. It is suggested
to use the package with
[data.table](https://github.com/Rdatatable/data.table).

## Installation

You can install the package using devtools:

``` r
devtools::install_github("santiagohermo/data.tools")
```

## Usage

``` r
library(data.tools)

# Simulate panel data
dt <- data.table::data.table(unit = c("A", "A", "B", "B", "C", "C", "D", "D", "E", "E"), 
                             time = rep(c(1, 2), 5))
dt[, y := rnorm(.N)]

# Create equal-sized groups
dt[, terciles_y        := cut_in_n(y, n=3)]
dt[, terciles_y_within := cut_in_n(y, n=3), by=time]

# Save the data set
save_data(dt, key = c("unit", "time"), 
          outfile = "data.csv")
```
