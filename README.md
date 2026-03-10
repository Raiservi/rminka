
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rminka <a href="https://dplyr.tidyverse.org"><img src="man/figures/logo1.png" alt="Logo de rminka" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Raiservi/rminka/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Raiservi/rminka/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/Raiservi/rminka/graph/badge.svg?token=PB3K1RMR9Y)](https://codecov.io/gh/Raiservi/rminka)
[![Development
Status](https://img.shields.io/badge/development%20status-In%20Development-blue)](https://github.com/raiservi/rminka)
[![GitHub last
commit](https://img.shields.io/github/last-commit/Raiservi/rminka)](https://github.com/Raiservi/rminka/commits/main)
<br> [![GitHub package
version](https://img.shields.io/github/r-package/v/Raiservi/rminka)](https://github.com/Raiservi/rminka/blob/main/DESCRIPTION)
[![GitHub
contributors](https://img.shields.io/github/contributors/Raiservi/rminka)](https://github.com/Raiservi/rminka/graphs/contributors)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](https://github.com/Raiservi/rminka/blob/master/inst/CODE_OF_CONDUCT.md)
<!-- badges: end -->

## Usage

Minka is a citizen science platform created by the research group
[EMBIMOS](https://www.icm.csic.es/en/research-group/embimos) of the
[Institut de Cienciès del Mar (ICM-CSIC)](https://icm.csic.es/en). The
platform is open for all users to create their projects, collaborate on
others, help the community with identifications or simply upload
observations that will be validated with the help of the community and
that can be used, in the future, to contribute openly to science and the
improvement of the environment. <br> The link to access Minka’s website
is <https://minka-sdg.org/>

The goals of the `rminka` package are:

1.  Directly access the data stored in the Minka platform to be able to
    process them with R through the API.

2.  Treat the data to be able to use them directly with other packages
    such as `vegan` or `dismo`.

## Overview

`rminka` is a toolkit for interacting with the [Minka
API](https://api.minka-sdg.org/v1/docs/), providing a consistent set of
functions to help you query and retrieve biodiversity data. The
package’s functions are grouped by the type of data they return:

**a) Project Queries:** A set of complementary functions to find
projects and their associated observations.

- `mnk_proj_byname()`: Finds a project’s ID using an approximate project
  name.
- `mnk_proj_info()`: Retrieves detailed project information using its
  known ID.
- `mnk_proj_obs()`: Fetches all observations for a specific year within
  that project.

**b) User Queries:** Functions to find users and their contributed
observations.

- `mnk_user_byname()`: Finds a user’s ID from their approximate login
  name.
- `mnk_user_proj()`: Find the projects to which a user has explicitly
  subscribed based on their user_ID.
- `mnk_user_obs()`: Retrieves all observations contributed by that user
  for a given year ( all the year or only a specific month) from their
  id_user.

**c) Place Queries:** Functions to find places and retrieve their
spatial data.

- `mnk_places_byname()`: Finds the ID for a location using an
  approximate place name.
- `mnk_place_sf()`: Returns the `sf` geometry for a place, ready for
  plotting with packages like `ggplot2` or `leaflet`.

**d) Observation Queries:** A variety of functions to fetch observation
data based on different parameters.

- `mnk_obs_id()`: Retrieves a single observation’s complete data using
  its unique ID.
- `mnk_obs()`: Fetches observations based on various parameters for a
  full year, a specific month, or a single day.
- `mnk_obs_bydays()`: Retrieves all observations within a date range in
  the same year.

**e) Auxiliary functions:** a set of functions with different utilities
that complement Minka’s observational data and help in processing them
when used in other R packages (dismo, vegan,..).

- `get_wrm_tax()`: Retrieves the complete taxonomy and additional
  information (terrestrial/marine…) of a species given its scientific
  name.
- `shrt_name()`: Returns the CEP name (abbreviated scientific name) with
  separation point, given the scientific name.

These functions are designed to be used together. For queries that span
multiple years, you can easily loop through the years of interest, run
the appropriate function, and then combine the resulting tibbles with
`dplyr::bind_rows()`.

## Installation

You can install the development version of rminka from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Raiservi/rminka")
```

## Using rminka

If you are new to `rminka` you are better off starting with a starting
web page of `rminka` in the github page of the project.

1.  The main page directions is [rminka
    website](https://Raiservi.github.io/rminkav/)

2.  The starting web page is [rminka
    starting](https://development-biomarine.github.io/rminkav3/articles/rminkav3.html)

## Getting help

There are two main places to get help with `rminka`:

1.  The [RStudio community](https://forum.posit.co/) is a friendly place
    to ask any questions about R.

2.  [Stack Overflow](https://stackoverflow.com/) is a great source of
    answers to common R questions. It is also a great place to get help,
    once you have created a reproducible example that illustrates your
    problem.

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/tidyverse/dplyr/issues).

## Code of conduct

Please note that this project is released following a [Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
