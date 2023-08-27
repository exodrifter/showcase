let Website = ../website.dhall
in  { template = "data/index.html"
    , distUri = "2023.html"
    , data =
      { title = "2023"
      , items =
        [ ../items/2023-08-11-01-43-29.dhall
        ]
      }
    }
