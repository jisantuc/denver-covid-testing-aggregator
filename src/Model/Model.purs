module Model where

import Model.JsonDate (JsonDate)

type Appointment r = {
    time:: JsonDate,
    reservationUrl:: String
    | r
}

type LocationResult = {
    appointments :: Array (Appointment ()),
    locationName :: String,
    address :: String
}