let vods =
  [ ./vod/2023-08-11-01-43-29.dhall
  ]

let indices =
  [ { template = "data/page/catalog.html"
    , uri = "2023.html"
    , input =
        { title = "2023"
        , items = vods
        }
    }
  ]

let pages =
  [ { template = "data/page/index.html"
    , uri = "index.html"
    }
  ]

in
  { routes =
    { vods
    , indices
    , pages
    }
  }
