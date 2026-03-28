# Get complete taxonomy from WoRMS as a flat tibble

This function downloads the complete taxonomy from the World Register of
Marine Species (WoRMS) for a given scientific name or taxon.

## Usage

``` r
get_wrm_tax(scientific_name)
```

## Arguments

- scientific_name:

  A string with the scientific name to search for.

## Value

A single-row \`tibble\` containing the taxonomic and habitat information
for the specified name. Returns \`NULL\` invisibly if the taxon is not
found or in case of an API/network error.

## Details

You need to provide the exact scientific name (e.g., "Genus species") or
a higher-level taxon (e.g., "Genus", "Familia"). The function will
return the information for the first exact match found.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get data for a species
diplodus_sargus_df <- get_wrm_tax("Diplodus sargus")
print(diplodus_sargus_df)

# Get data for a genus
diplodus_genus_df <- get_wrm_tax("Diplodus")
print(diplodus_genus_df)
} # }
```
