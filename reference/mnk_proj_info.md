# Get Information About a Specific Minka Project

This function downloads information for a specific Minka project using
either its unique \`project_id\` or its group identifier (\`grpid\`).

## Usage

``` r
mnk_proj_info(project_id = NULL, grpid = NULL, users = FALSE)
```

## Arguments

- project_id:

  A single character string or number representing the unique Minka
  project ID.

- grpid:

  The group identifier (slug) or ID for the project.

- users:

  A logical value (\`TRUE\` or \`FALSE\`). If \`TRUE\`, the function
  returns a tibble containing only the IDs of users subscribed to the
  project. If \`FALSE\` (the default), it returns a list with the main
  project information.

## Value

By default, a list containing key details about the project. If \`users
= TRUE\`, it returns a \`tibble\` with a single column \`id_users\`
containing the numeric IDs of subscribed users. If the project is not
found or a network error occurs, it returns \`NULL\` and prints a
message.

## Details

You must provide either a \`project_id\` or a \`grpid\`. If you don't
know the ID, you can use the \`mnk_proj_byname()\` function to find it.

A Minka \`grpid\` or slug is typically the project name formatted as a
single string with words separated by hyphens (e.g.,
'biomarato-barcelona-2025'). You can find it in the URL of the project's
page on the Minka website.

## Examples

``` r
if (FALSE) { # \dontrun{
# First, find a project ID
mnk_proj_byname("Biomarato Barcelona 2025")

# Get main information for project ID '420'
mnk_proj_info(project_id = "420")

# Get only the subscriber IDs for the same project
mnk_proj_info(project_id = "420", users = TRUE)
} # }
```
