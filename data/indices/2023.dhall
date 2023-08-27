let Website = ../website.dhall
in  { template = "data/index.html"
    , uri = "2023.html"
    , input =
      { title = "2023"
      , items =
        [ ../items/2023-08-11-01-43-29.dhall
        ]
      }
    }
