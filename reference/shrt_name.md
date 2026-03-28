# Generate a Short Name from a Scientific Name

From a full scientific name, this function creates a standard
abbreviation by taking the first three letters of each word, in
lowercase, and joining them with a period.

## Usage

``` r
shrt_name(scientific_name)
```

## Arguments

- scientific_name:

  A character string or vector of character strings with the scientific
  names.

## Value

A character string or vector of character strings with the
abbreviations.

## Examples

``` r
if (FALSE) { # \dontrun{
shrt_name("Diplodus sargus")
shrt_name("Diplodus sargus sargus")
shrt_name(c("Diplodus cervinus", "Diplodus vulgaris","Diplodus sargus"))
} # }
```
