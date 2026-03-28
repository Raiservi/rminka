# Information on Minka projects

Get information on Minka projects selected by a string contained in the
project name.

## Usage

``` r
mnk_proj_byname(query)
```

## Arguments

- query:

  A string that is contained in the project name.

## Value

A data frame with all the projects that contain the string with some
details of those projects.

## Examples

``` r
if (FALSE) { # \dontrun{
mnk_obs <- mnk_proj_byname(query="Biomarato 2025")
mnk_obs_id(m_obs$id[1])
} # }
```
