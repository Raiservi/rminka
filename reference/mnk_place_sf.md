# Get the sf geometry of a Minka place

Gets the geometry in sf format of a Minka place given its id_place.The
CRS of the resulting geometry is CRS = 4326 (WGS84)

## Usage

``` r
mnk_place_sf(id)
```

## Arguments

- id:

  A single integer number id for a Minka place This id_place number is
  unique for each Minka place

## Value

A tibble with the sf geometry place

## Examples

``` r
if (FALSE) { # \dontrun{
sf_sant_feliu <- mnk_place_sf(id= 265)
} # }
```
