# Search Minka user by part of login name

Get information on a specific Minka user selected by a string contained
in the user login name

## Usage

``` r
mnk_user_byname(query)
```

## Arguments

- query:

  A string that is contained in the the user login name

## Value

A data frame with all some details of the differents users that contains
the string

## Details

This fucntion is mainly used for obtain the user id and use this id for
other functions.

## Examples

``` r
if (FALSE) { # \dontrun{
m_obs <- mnk_user_byname(query="xavier")
} # }
```
