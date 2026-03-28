# Introduction to rminka

``` r
library(rminka)
```

### About

`rminka` is a wrapper for Minka APIs for accessing the observations.

### Installation

The easiest way to get `rminka` is to install from github repository
with `pak`:

``` r
pak::pak("Raiservi/rminka")
```

### Quickstart guide

These functions are designed to be used together. For queries that span
multiple years, you can easily loop through the years of interest, run
the appropriate function, and then combine the resulting tibbles with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
\> It is recommended to set the `quality` argument to `"research"` in
order to get more reliable data that has been validated by several
contributors.

#### Project Queries

A set of complementary functions to find projects and their associated
observations.

- ## mnk_proj_byname \*

  **- mnk_proj_byname**

[`mnk_proj_byname()`](https://raiservi.github.io/rminka/reference/mnk_proj_byname.md)Finds
a project’s ID using an approximate project name.

``` r


prj_names <- mnk_proj_byname("biomarato")
prj_names 
#> # A tibble: 10 × 8
#>       id title     place_id slug  created_at updated_at project_type description
#>    <int> <chr>        <int> <chr> <chr>      <chr>      <chr>        <chr>      
#>  1   417 BioMARat…      244 biom… 2025-03-2… 2025-10-3… collection   "La BioMAR…
#>  2   283 BioMARat…      244 biom… 2024-03-2… 2025-10-3… collection   "La BioMAR…
#>  3   124 BioMARat…      244 biom… 2023-03-1… 2025-10-3… collection   "La BioMAR…
#>  4   281 BioMARat…      245 biom… 2024-03-2… 2025-08-1… collection   "La BioMAR…
#>  5   418 BioMARat…      245 biom… 2025-03-2… 2025-08-2… collection   "La BioMAR…
#>  6   280 BioMARat…      249 biom… 2024-03-2… 2025-12-1… collection   "La BioMAR…
#>  7    20 BioMARat…      244 biom… 2022-04-1… 2025-10-3… collection   "La BioMAR…
#>  8   419 BioMARat…      249 biom… 2025-03-2… 2025-10-3… collection   "La BioMAR…
#>  9   123 BioMARat…      248 biom… 2023-03-1… 2025-11-1… collection   "La BioMAR…
#> 10     1 BioMARat…      245 biom… 2022-04-1… 2025-05-2… collection   "La BioMAR…
```

- mnk_proj_info \*\*

``` r

prj_names <- mnk_proj_byname("2025")

prj_names[,c(1:5)]
#> # A tibble: 10 × 5
#>       id title                                         place_id slug  created_at
#>    <int> <chr>                                            <int> <chr> <chr>     
#>  1   417 BioMARató 2025 (Catalunya)                         244 biom… 2025-03-2…
#>  2   418 BioMARató 2025 (Girona)                            245 biom… 2025-03-2…
#>  3   419 BioMARató 2025 (Tarragona)                         249 biom… 2025-03-2…
#>  4   420 BioMARató 2025 (Barcelona)                         248 biom… 2025-03-2…
#>  5   424 BioMARatona 2025                                   701 biom… 2025-04-0…
#>  6   233 Biodiverciutat 2025 - Àrea Metropolitana de …       NA biod… 2023-12-0…
#>  7   228 Biodiverciutat 2025 - Castelldefels (Repte N…      277 biod… 2023-12-0…
#>  8   224 Biodiverciutat 2025 - Barcelona (Repte Natur…      311 biod… 2023-12-0…
#>  9   415 Biodiversitat NauticinBlu 2025                     247 biod… 2025-03-2…
#> 10   524 Exposició de Bolets de la Serralada Litoral …      437 expo… 2025-11-0…
```

**- mnk_proj_info**

[`mnk_proj_info()`](https://raiservi.github.io/rminka/reference/mnk_proj_info.md):
Retrieves detailed project information using its known ID.

``` r

prj_info <- mnk_proj_info(420)

kable(as.data.frame(prj_info))
```

|  id | title                      | created_at                | subscrib_users | place_id | slug                     | description                                                                                                                                                                                                                                                                                         |
|----:|:---------------------------|:--------------------------|---------------:|---------:|:-------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 420 | BioMARató 2025 (Barcelona) | 2025-03-25T17:00:14+01:00 |             16 |      248 | biomarato-2025-barcelona | La BioMARató és un esdeveniment de ciència ciutadana que té l’objectiu de conèixer la biodiversitat del litoral de Catalunya mitjançant una competició amistosa entre les persones participants. En aquest projecte es recullen les observacions marines i costaneres de la província de Barcelona. |

It´s posible to known the users subscrived in this projects chnging de
parameter ‘user = TRUE’ and the function gives a list of the id users.

**- mnk_proj_obs**
[`mnk_proj_obs()`](https://raiservi.github.io/rminka/reference/mnk_proj_obs.md):
Fetches all observations for a specific year within that project.
