let List/map =
      https://prelude.dhall-lang.org/v11.1.0/List/map
        sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let Service = < Twitch | YouTube >

let RawLink = { id : Text, service : Service }

let ProcessedLink = { id : Text, url : Text, service : Service }

let RawShort = { links : List RawLink, name : Text }

let ProcessedShort = { links : List ProcessedLink, name : Text }

let Tag = { category : Text, value : Text }

let RawSchema =
      { duration : Natural
      , shorts : List RawShort
      , tags : List Tag
      , thumbPath : Text
      , thumbUri : Text
      , timestamp : Text
      , title : Text
      , videoId : Text
      }

let ProcessedSchema =
      { duration : Natural
      , shorts : List ProcessedShort
      , tags : List Tag
      , thumbPath : Text
      , thumbUri : Text
      , timestamp : Text
      , title : Text
      , videoId : Text
      }

let processLink =
      \(link : RawLink) ->
              link
          //  { url =
                  merge
                    { Twitch =
                        "https://www.twitch.tv/exodrifter_/clip/${link.id}"
                    , YouTube = "https://www.youtube.com/watch?v=${link.id}"
                    }
                    link.service
              }
        : ProcessedLink

let processShort =
      \(short : RawShort) ->
              short
          //  { links = List/map RawLink ProcessedLink processLink short.links }
        : ProcessedShort

let processSchema =
      \(schema : RawSchema) ->
              schema
          //  { shorts =
                  List/map RawShort ProcessedShort processShort schema.shorts
              }
        : ProcessedSchema

in  { Service, processSchema }
