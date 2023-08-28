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

in
  { routes =
    { vods
    , indices
    }
  }
