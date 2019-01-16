module Concur.React.Widgets where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Effect.Class (liftEffect)
import Unsafe.Coerce (unsafeCoerce)

-- | A Text input that returns its contents on enter
textInputEnter :: String -> String -> Boolean -> Widget HTML String
textInputEnter value hint reset = do
    e <- D.input [P.defaultValue value, P.onKeyEnter, P.placeholder hint]
    new <- pure $ P.unsafeTargetValue e
    when reset $ liftEffect (P.resetTargetValue "" e)
    pure new

-- | A Text input that has a button attached
-- | Returns its contents on the user pressing enter, or clicking the button
-- textInputWithButton :: forall m. ShiftMap (Widget HTML) m => MultiAlternative m => ShiftUp (Widget HTML) m => Monad m => String -> String -> Boolean -> String -> m String
textInputWithButton :: String -> String -> Boolean -> String -> Widget HTML String
textInputWithButton value hint reset buttonlabel = do
  rr <- liftEffect P.createRef
  _ <- D.div'
    [ unit <$ D.input [P.onKeyEnter, P.defaultValue value, P.placeholder hint, P.ref rr]
    , D.text " "
    , unit <$ D.button [P.onClick] [D.text buttonlabel]
    ]
  textElem <- liftEffect $ P.refGetter rr
  -- HACK: Using forced do notation, to force evaluation of the text input value before resetting it
  v <- pure $ (unsafeCoerce textElem).value
  when reset $ liftEffect (P.resetValue "" textElem)
  pure v
