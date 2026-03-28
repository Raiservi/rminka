# Information on a specific observation Get information on a specific observation by observation id.

Information on a specific observation Get information on a specific
observation by observation id.

## Usage

``` r
mnk_obs_id(id, meta = FALSE)
```

## Arguments

- id:

  A single integer number for a Minka observation record.

- meta:

  Downloand metadata.

## Value

A dataframe with all details on a given record

## Examples

``` r
if (FALSE) { # \dontrun{
m_obs <- mnk_prj_obs(420, year = 2025)
mnk_obs_id(m_obs$id[1])
} # }
```
