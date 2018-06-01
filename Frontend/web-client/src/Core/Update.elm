module Core.Update exposing (update)


-- ~
-- + Local
import Core.Effects.Msg as Core
import Core.Model       as Core
-- ~


update : Core.Msg -> Core.Model -> (Core.Model, Cmd Core.Msg)
update msg model =
    (model, Cmd.none)


