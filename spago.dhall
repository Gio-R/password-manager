{ name = "password-generator"
, sources = [ "src/**/*.purs" ]
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "arraybuffer"
  , "arraybuffer-builder"
  , "arraybuffer-types"
  , "arrays"
  , "b64"
  , "bifunctors"
  , "bigints"
  , "concur-core"
  , "concur-react"
  , "console"
  , "control"
  , "datetime"
  , "decimals"
  , "effect"
  , "either"
  , "encoding"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "fortuna"
  , "functions"
  , "http-methods"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "react"
  , "record"
  , "spec"
  , "strings"
  , "subtlecrypto"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "web-events"
  , "web-file"
  ]
, packages = ./packages.dhall
}
