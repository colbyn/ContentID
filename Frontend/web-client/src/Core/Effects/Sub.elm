module Core.Effects.Sub exposing (subscriptions)


-- ~
import Core.Model       as Core
import Core.Effects.Msg as Core
-- ~


subscriptions : Core.Model -> Sub Core.Msg
subscriptions model =
    Sub.none