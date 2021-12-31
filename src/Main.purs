module Main
  ( State
  , main
  , render
  , view
  , viewComponent
  ) where

import Data.Array ((:))
import Effect (Effect)
import Halogen (AttrName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (attr, prop)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Model (Appointment, LocationResult)
import Model.JsonDate (stringify, unsafeFromString)
import Prelude (class Applicative, Unit, Void, bind, pure, unit, ($), (<$>))
import Web.HTML.Common (PropName(..))

type State = Array (LocationResult)

view :: forall t1 t2. HH.HTML t1 t2
view = HH.div []
  [ HH.h1 [] [ HH.text "Hello world" ]
  ]

timeEntry :: forall r t1 t2. Appointment r -> HH.HTML t1 t2
timeEntry { reservationUrl, time } =
  HH.div []
    [ HH.a [ prop (PropName "href") reservationUrl ]
        [ HH.text (stringify time)
        ]
    ]

entry :: forall t1 t2. LocationResult -> HH.HTML t1 t2
entry { locationName, appointments, address } =
  HH.div [] $
    HH.h1 [] [ HH.text locationName ] : HH.h3 [] [ HH.text address ] : (timeEntry <$> appointments)

explanation :: String
explanation =
  """
This site aggregates COVID test availability from several sources around Denver.
Currently the only family of providers it aggregates is AFC Urgent Care. The reason for
that is that they're the first website I found making an HTTP request for current availability,
but each location shows only local availability. I don't know if there are other
providers with an endpoint to hit for availability, but if there are, please
let me know on Twitter at
"""

twitterLink :: forall t12 t13. HH.HTML t12 t13
twitterLink = HH.a [ prop (PropName "href") "https://twitter.com/james_santucci" ] [ HH.text "@james_santucci" ]

centeredColStyle :: String
centeredColStyle = "margin: auto; width: 50%; padding: 3rem;"

render :: forall m. State -> H.ComponentHTML Void () m
render appointments = HH.div [ attr (AttrName "style") centeredColStyle ] $
  HH.p [] [ HH.text explanation, twitterLink, HH.text "."]
    : (entry <$> appointments)

initialState
  :: forall t14
   . Applicative t14
  => t14 State
initialState = pure
  [ { locationName: "AFC Five Points"
    , appointments:
        [ { time: unsafeFromString "2022-02-02T00:00:00"
          , reservationUrl: "https://google.com"
          }
        , { time: unsafeFromString "2022-02-03T00:00:00"
          , reservationUrl: "https://google.com"
          }
        , { time: unsafeFromString "2022-02-04T00:00:00"
          , reservationUrl: "https://google.com"
          }
        ]
    , address: "999 Grant St, Denver, CO 80203"
    }
  , { locationName: "AFC Five Points"
    , appointments:
        [ { time: unsafeFromString "2022-02-02T00:00:00"
          , reservationUrl: "https://google.com"
          }
        , { time: unsafeFromString "2022-02-03T00:00:00"
          , reservationUrl: "https://google.com"
          }
        , { time: unsafeFromString "2022-02-04T00:00:00"
          , reservationUrl: "https://google.com"
          }
        ]
    , address: "999 Grant St, Denver, CO 80203"
    }
  , { locationName: "AFC Five Points"
    , appointments:
        [ { time: unsafeFromString "2022-02-02T00:00:00"
          , reservationUrl: "https://google.com"
          }
        , { time: unsafeFromString "2022-02-03T00:00:00"
          , reservationUrl: "https://google.com"
          }
        , { time: unsafeFromString "2022-02-04T00:00:00"
          , reservationUrl: "https://google.com"
          }
        ]
    , address: "999 Grant St, Denver, CO 80203"
    }
  ]

viewComponent :: forall q i o m. H.Component q i o m
viewComponent = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
  }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI viewComponent unit body