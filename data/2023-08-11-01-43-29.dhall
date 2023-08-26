let Website = ./website.dhall

in    { duration = 13716
      , shorts =
        [ { links =
            [ { id = "PeppyEndearingBubbleteaEleGiggle-N10C3Tl60qT7M82Q"
              , service = Website.Service.Twitch
              }
            ]
          , name = "i have to drink all the water"
          }
        , { links =
            [ { id = "FrozenRamshackleTubersDAESuppy-dOOGqxikrrkKQ7DH"
              , service = Website.Service.Twitch
              }
            ]
          , name = "i want a sub"
          }
        ]
      , tags =
        [ { category = "software", value = "fl studio" }
        , { category = "raided by", value = "CAI_TAn" }
        , { category = "raided by", value = "ShinyOlivia" }
        , { category = "raiding", value = "earend" }
        , { category = "song", value = "whale fall" }
        , { category = "category", value = "music production" }
        , { category = "service", value = "twitch" }
        ]
      , thumbPath = "/assets/thumbs/2023-08-11-01-43-29.jpg"
      , thumbUri = "/videos/853749451/pictures/1709397524"
      , timestamp = "2023-08-10T20:43:29-05:00"
      , title = "mystery enchanted music making?"
      , videoId = "853749451"
      }
    : Website.Schema
