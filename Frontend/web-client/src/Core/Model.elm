module Core.Model exposing (Model, init, initModel)


-- ~
-- + Local
import Core.Effects.Msg   as Core
-- ~


type alias Model =
    {
    }

init : (Model, Cmd Core.Msg)
init = (initModel, Cmd.none)


initModel : Model
initModel =
    { 
    }


