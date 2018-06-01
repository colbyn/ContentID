module Main exposing (..)


-- ~
import Html
import Html.Styled            as H
import Html.Styled.Attributes as A
import Html.Styled.Events     as E
import Css

-- + Local
import Core.Model        as Model
import Core.Update       as Update
import Core.View         as View

import Core.Effects.Msg as Msg
import Core.Effects.Sub as Sub
-- ~




main : Program Never Model.Model Msg.Msg
main = Html.program
    { init = Model.init
    , view = View.view >> H.toUnstyled
    -- , view = Prototype.view >> H.toUnstyled
    , update = Update.update
    , subscriptions = Sub.subscriptions
    }









