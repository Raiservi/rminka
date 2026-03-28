# Projects subscribed by a Minka user

Retrieves the projects to which a user is subscribed from the Minka page
web, and returns them as a tibble.

## Usage

``` r
mnk_user_proj(id_user)
```

## Arguments

- id_user:

  (numeric): A single integer number corresponding to the ID_user

## Value

A tibble where each row is a project and the columns contain relevant
information about the project. Returns an empty tibble if there are no
projects or if an error occurs.

## Examples

``` r
if (FALSE) { # \dontrun{
# It´s necesary to known the project_id
projects_for_user_6 <- mnk_user_proj(6)
print(projects_for_user_6)
} # }
```
