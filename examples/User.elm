module User exposing (..)

import Validation as Validation exposing (RecordValidation, Validation)


type alias Input =
    { firstname : String
    , secondname : String
    , age : Maybe String
    , heightMeter : Maybe String
    , weightKilogram : Maybe String
    }


type alias User =
    { firstname : String
    , secondname : String
    , age : Int
    , bmi : Float
    , createdManually : Bool
    , thisFieldIsAlwaysInvalid : ()
    }


userValidation : RecordValidation String Input User
userValidation =
    let
        floatValidation : Validation String (Maybe String) Float
        floatValidation =
            Validation.fromMaybe
                |> Validation.compose Validation.toFloat

        toBmi : ( Float, Float ) -> Float
        toBmi ( h, w ) =
            h / w ^ 2
    in
    Validation.record User
        |> Validation.field .firstname Validation.isNotEmptyString
        |> Validation.field .secondname Validation.isNotEmptyString
        |> Validation.field .age
            (Validation.fromMaybe
                |> Validation.compose Validation.toInt
                |> Validation.compose Validation.isNonNegativeNumber
                |> Validation.compose (Validation.intIsLowerThan 100)
            )
        |> Validation.field
            (\input -> ( input.heightMeter, input.weightKilogram ))
            (Validation.tuple ( floatValidation, floatValidation )
                |> Validation.compose (Validation.lift toBmi)
                |> Validation.compose (Validation.floatIsLowerThan 40)
            )
        |> Validation.field identity (Validation.succeed True)
        |> Validation.field identity
            (Validation.fail "Sorry, this field is simply invalid")
        |> Validation.endRecord
