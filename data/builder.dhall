let Website = ./website.dhall
in
{ `2023-08-11-01-43-29.html` = Website.schemaToHTML ./2023-08-11-01-43-29.dhall
}