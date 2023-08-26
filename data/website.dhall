let concatMap = https://prelude.dhall-lang.org/v23.0.0/Text/concatMap.dhall

let concatMapSep =
      https://prelude.dhall-lang.org/v23.0.0/Text/concatMapSep.dhall

let Service = < Twitch | YouTube >

let Link = { id : Text, service : Service }

let Short = { links : List Link, name : Text }

let Tag = { category : Text, value : Text }

let Schema =
      { duration : Natural
      , shorts : List Short
      , tags : List Tag
      , thumbPath : Text
      , thumbUri : Text
      , timestamp : Text
      , title : Text
      , videoId : Text
      }

let linkToHTML =
      \(link : Link) ->
        merge
          { Twitch =
              "<a href=\"https://www.twitch.tv/exodrifter_/clip/${link.id}\">Twitch</a>"
          , YouTube =
              "<a href=\"https://www.youtube.com/watch?v=${link.id}\">YouTube</a>"
          }
          link.service

let shortToHTML =
      \(short : Short) ->
        "<li>${short.name} - ${concatMapSep
                                 ", "
                                 Link
                                 linkToHTML
                                 short.links}</li>"

let tagToHTML = \(tag : Tag) -> "<li>${tag.category}: ${tag.value}</li>"

let videoIdToHTML =
      \(videoId : Text) ->
      \(title : Text) ->
        "<div style=\"padding:56.25% 0 0 0;position:relative;\"><iframe src=\"https://player.vimeo.com/video/${videoId}?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479\" frameborder=\"0\" allow=\"autoplay; fullscreen; picture-in-picture\" style=\"position:absolute;top:0;left:0;width:100%;height:100%;\" title=\"${title}\"></iframe></div><script src=\"https://player.vimeo.com/api/player.js\"></script>"

let schemaToHTML =
      \(schema : Schema) ->
        ''
        <html>
          <head>
            <title>${schema.title}</title>
          </head>
          <body>
            <h1>${schema.title}</h1>
            <p>Posted on ${schema.timestamp}
            ${videoIdToHTML schema.videoId schema.title}

            <p>Shorts:</p>
            <list>
              ${concatMapSep "\n      " Short shortToHTML schema.shorts}
            </list>

            <p>Tags:</p>
            <list>
              ${concatMapSep "\n      " Tag tagToHTML schema.tags}
            </list>
          </body>
        </html>
        ''

in  { Schema, Service, Link, schemaToHTML }
