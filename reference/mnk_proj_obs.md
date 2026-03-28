# Observations from a Minka project

This function downloads observation data for a Minka project,in a given
year, optionally, a specific month. It automatically manages if you
download it must be done month by month or for a single month.

## Usage

``` r
mnk_proj_obs(project_id, year, month = NULL)
```

## Arguments

- project_id:

  A single id for a Minka project.

- year:

  Return all observations of project only in that year (can only be one
  year, not a range of years).

- month:

  A number from 1 to 12 to download only that month. If it is NULL, the
  entire year will be downloaded.

## Value

A data frame with all the year/month records with full details.

## Details

Before using this function, you need to know the `project_id`. If the ID
is unknown, you can first use the function
[`mnk_proj_byname`](https://raiservi.github.io/rminka/reference/mnk_proj_byname.md)
to find it based on the project's name.

## Examples

``` r
if (FALSE) { # \dontrun{
  #If the project_id is known you can use directly the function
  biomarato <- mnk_proj_byname(420,year=2025)
  #First of all it is necessary to obtain the project_id of the project.
  projects <- mnk_proj_byname(query="Biomarato 2025")
  #Select the id_project
  obs_june_2025 <- mnk_proj_obs( projects$project_id[1], year=2025, month=6)
  obs_2025 <- mnk_proj_obs( projects$project_id[1], year=2025, month=NULL)
} # }
```
