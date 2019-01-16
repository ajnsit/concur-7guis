module Main where

import Prelude

import Concur.Core (Widget)
import Concur.Core.FRP (display, dyn, hold, loopS, stepMaybe)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Array as A
import Data.Int (fromString, toNumber)
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.String (length, split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Global (readFloat)

{- Concur is a UI framework which lets you build `Widgets`.
   To run a Concur Widget, use `runWidgetInDom`

   Ignore the other parts in the main function below.
   Come back when you understand the Concur Widget Model.
-}

main :: Effect Unit
main = runWidgetInDom "root" $ D.div'
    [ frame "Counter" counterWidget
    , frame "Temperature Converter" tempWidget
    , frame "Flight Booker" flightWidget
    , frame "Timer" timerWidget
    , frame "CRUD" crudWidget
    , frame "Circle Drawer" circleWidget
    , frame "Cells" cellsWidget
    ]
  where
    frame s w = D.div
      [ P.style {width: "500px", padding: "10px", border: "1px solid lightgray"} ]
      [ D.h3' [D.text s], w]


{- Concur has a simple Widget model

   You can define static widgets with the combinators defined in Concur.React.DOM and Concur.React.Props
   e.g. D.div' [D.text "Here comes a button", D.button [P.width 100] [D.text "Hello"]]

   Widgets are also monadic values. They have the type
     `Widget HTML a`
   where `a` is the return value for the widget.
   All static widgets will automatically have the type `forall a. Widget HTML a` since they never return.

   For dynamic widgets, you can attach event props to a dom element inside the widget. All the events are defined in
   Concur.React.Props and start with `on`, e.g. `onClick`, 'onChange'.

   When any event inside a widget fires, the entire widget will return with the event object.
   Sometimes, it makes sense to ignore the actual event object, e.g. for a button click, it's good enough to know that it's been clicked.
   But often, you would need to extract some useful information out of the event. For that look at the event datatypes defined in React.SyntheticEvent. You can also just unsafeCoerce the event object to get the propert you want (and know exists). E.g. to get the changed value of an input on an `onChange` event, you can use `(unsafeCoerce e).target.value`.

   If there are multiple events inside a widget, you can use the Functor instance for either Widgets or Props to tag return values. E.g. to get events from left and right buttons, you can do either -
   `D.div' [D.button [Left <$> P.onClick] [D.text "Left button"], D.button [Right <$> P.onClick] [D.text "Right button"]]`
   OR
   `D.div' [Left <$> D.button [P.onClick] [D.text "Left button"], Right <$> D.button [P.onClick] [D.text "Right button"]]`
   They are equivalent.

   Finally, once you get a value out of a widget, you can draw another widget (which might be the same widget, just lightly modified). Thanks to VDOM, only the parts of the widget that actually changed will be modified in the DOM. A good generic way to build widgets is through stateful recursion. This is basically The Elm Architecture -

   myWidget = go initialState
     where
     go oldState = do
       action <- renderMyWidget oldState
       let newState = updateMyState action oldState
       go newState

   Now you are ready to read the Counter example!
-}

-- 7GUIs: The first task: Counter

-- The Counter widget never ends
counterWidget :: forall a. Widget HTML a
-- Start with counter 0
counterWidget = go 0
  where
    go x = do
      -- Render the GUI and wait for an event (button click)
      D.div'
        -- Show the count
        [ D.text (show x)
        -- And a button to increment the counter
        , void $ D.button [P.onClick] [D.text "Count"]
        ]
      -- Button was clicked. Recurse with incremented counter
      go (x + 1)

{- The other big concept in Concur is Signals. They are a thin layer on top of Widgets and represent never-ending Widgets which are very common in UIs.
   The various signal combinators are defined in Concur.Core.FRP. As the namespace suggests, Signals are independent of the view layer (i.e. React), and will work, out of the box, with any other view layer.

   A Signal can be thought of as a Widget that is repeatedly invoked, but the value returned by the last invocation is made available as well. i.e `Signal v a = (Widget v a, a)`. This is the `Cofree` structure. So `Signal a = Cofree (Widget v) a`. This is how they are defined in Concur.Core.FRP.

   Whenever you have a widget that has events you want to handle, but the widget is mostly never removed from the screen, a signal may be a good option to simplify your code.

   The simplest ways to construct Signals are -

     hold => Takes an initial value, and a widget to repeatedly invoke
     hold :: a -> Widget v a -> Signal v a

     step => Takes an initial value, and a widget that returns a new signal to transform into
     step :: a -> Widget v (Signal v a) -> Signal v a

     stepMaybe => Takes only a widget, but the signal may not have a value in the beginning (hence the Maybe)
     stepMaybe :: Widget v a -> Signal v (Maybe a)

   Signals can also be composed with monadic bind, but unlike Widgets, the previous signals don't disappear when you schedule a new one. Also the return value from each signal represents the last known value returned by the underlying widget.
   With these properties, Signals form a close approximation of FRP (but better since operations on values returned by Signals don't need to be lifted. i.e. Signals return plain values `a`, not `Event a` or `Behaviour a`).

   One problem with FRP using monadic bind is that values returned by later signals cannot be used in earlier signals.
   Some FRP frameworks like Reflex use monad-fix for this. Concur has a much simpler solution - loops!

   For example, using the `loopS` combinator, the value returned by the signal can be passed again back to the first signal. You need to pass the initial value to start the looping process.

   loopS :: a -> (a -> Signal v a) -> Signal v a

   Now we can recreate the counter example with signals -

   counterSignal =
     -- Loop because we need the button (which comes later) to affect the input (which comes earlier)
     loopS 0 $ \count -> do
       -- Label showing the last count. `hold unit` is also called `display`
       hold unit $ D.text (show count)
       -- Button which when clicked, sends a new count to the top of the loop
       (count+1) <$ D.button [P.onClick] [D.text "Count"]

   Finally, there are a variety of ways to run Signals. But the easiest is `dyn` which simply converts a Signal back into a never ending Widget. So we can do -
   counterWidget :: forall a. Widget HTML a
   counterWidget = dyn counterSignal

   Some other ways to run signals (like `oneShot`) are useful when you want the Signal to end at some point. Read Concur.Core.FRP to know more about those.

   Side note: It's also important to mention, that never-ending widgets or never-ending signals can still be controlled, and removed from the DOM by their enclosing parent widget. So they are only immortal as much as their own little universe is concerned.

   Now you can read the temperature widget example.

   The simplest way to implement the temperature widget with signals would have been -

   -- Start with 5 degrees Celsius as the temperature
   tempWidget = dyn $ loopS 5 $ \c -> do
     -- This input can modify the initial value of C
     c' <- hold c $ readFloat <$> D.input [P.value (show c), P.unsafeTargetValue <$> P.onChange]
     -- Static text
     display $ D.text " Celsius = "
     -- Compute the value of F
     let f = c2f c'
     -- This input can modify the value of F
     f' <- hold f $ readFloat <$> D.input [P.value (show f), P.unsafeTargetValue <$> P.onChange]
     -- Static text
     display $ D.text " Fahrenheit"
     -- Loop the modified value of F back to the top after converting to celsius
     pure (f2c f')

   However, this example has an added complication of having a 2-way binding with "lossy" round-trips (due to floating point errors). This means that this identity does not hold true - (c2f (f2c f)) !== f.
   So with the naive code, the value of the input being modified by the user will also get overwritten by something that's not necessarily the same value the user entered. This leads to a very jarring experience.

   In the actual implementation below, we distinguish between the user changing one of the inputs, vs, the input displaying a computed value from the other input. Each of our inputs return a Maybe value using `stepMaybe`. Whent the value was computed it's Nothing, else it's Just newValue. We also store both the fahrenheit and celsius values so that if the other input is not being changed by the user (i.e. it's output is Nothing), we just display the user input rather than computing the value of the current input (it's easier to understand the code than this explanation).

   As you can see, the code is still about the same length as the naive version.
-}

-- 7GUIs: The second task: Temperature Converter

-- Correctly handles, 2-way bindings with "lossy" round-trips
tempWidget :: forall a. Widget HTML a
tempWidget = dyn $ loopS {c:5.0, f:c2f 5.0} $ \t -> do
  mc <- stepMaybe $ readFloat <$> D.input [P.value (show t.c), P.unsafeTargetValue <$> P.onChange]
  display $ D.text " Celsius = "
  mf <- stepMaybe $ readFloat <$> D.input [P.value (show (maybe t.f c2f mc)), P.unsafeTargetValue <$> P.onChange]
  display $ D.text " Fahrenheit"
  pure {c:maybe (fromMaybe t.c mc) f2c mf, f:fromMaybe (maybe t.f c2f mc) mf}
  where
    c2f c = (c * 9.0/5.0) + 32.0
    f2c f = ((f - 32.0) * 5.0)/9.0


{- The flight booker code below follows the same scheme as above.
   It just has some more code to handle the disabling fields, background colors, dropdowns, parsing dates etc.
-}

-- 7GUIs: The third task: Flight Booker

flightWidget :: forall a. Widget HTML a
flightWidget = dyn $ loopS {oneWay: true, inDate: "27.03.2014", outDate: "27.03.2014"} $ \st -> do
  oneWay <- hold st.oneWay $ map (_ == "oneway") $ D.div [P.style {padding: "5px 0"}] $ pure $
    D.select [P.unsafeTargetValue <$> P.onChange, P.defaultValue (if st.oneWay then "oneway" else "return")]
      [ D.option [P.value "oneway"] [D.text "one-way flight"]
      , D.option [P.value "return"] [D.text "return flight"]
      ]
  inDate <- hold st.inDate $ D.div [P.style {padding: "5px 0"}] [D.input [P.unsafeTargetValue <$> P.onChange, P.value st.inDate, bgRedUnless (validInDate st)]]
  outDate <- hold st.outDate $ D.div [P.style {padding: "5px 0"}] [D.input [P.disabled st.oneWay, P.unsafeTargetValue <$> P.onChange, P.value st.outDate, bgRedUnless (validOutDate st)]]
  pure {oneWay: oneWay, inDate:inDate, outDate:outDate}
  where
    bgRedUnless p = P.style {background: if p then "white" else "red"}
    validInDate st = dateValid st.inDate
    validOutDate st = st.oneWay || (dateGreater st.outDate st.inDate && dateValid st.outDate)
    dateValid s = A.all (\is -> isJust (fromString is)) ss && [2,2,4] == map length ss where ss = split (Pattern ".") s
    dateGreater s1 s2 = A.reverse (split (Pattern ".") s1) >= A.reverse (split (Pattern ".") s2)


{- Concur allows easy and uniform handling of arbitrary Synchronous and Asynchronous effects.

   Use `liftAff` to convert any arbitrary async Aff into a Widget.
   Use `liftEffect` to convert any arbitrary sync Effect into a Widget.
   These converted widgets can then be composed with combinators just like normal widgets.

   So now, you can look at the timer code below.
   It follows pretty much a similar scheme as before, but now we also need to handle timer ticks. To do that we simply convert an async effect which fires every 100ms to a Widget, and on every fire, increase the elapsed time by 100.
  Then we use `hold st.elapsed` to convert it to a Signal which always has the current value of the elapsed time.
  Note that we also allow an actual widget (the reset button) to change the elapsed time, and that composes without issues with the timer.
-}

-- 7GUIs: The fourth task: Timer

timerWidget :: forall a. Widget HTML a
timerWidget = dyn $ loopS {elapsed: 0, duration: 100000} $ \st -> do
  hold unit $ D.div [] [D.text "Elapsed Time: ", D.progress [P.value (show st.elapsed), P.max (show st.duration)] []]
  hold unit $ D.div' [D.text (show (toNumber st.elapsed / 1000.0) <> "s")]
  duration <- hold st.duration $ D.div'
    [ D.text "Duration: "
    , map (fromMaybe 0 <<< fromString) $ D.input
        [P._type "range", P.min "1", P.max "100000", P.defaultValue (show st.duration), P.unsafeTargetValue <$> P.onChange]
    ]
  elapsed <- hold st.elapsed $ (st.elapsed + 100) <$ if st.elapsed > st.duration then mempty else liftAff (delay $ Milliseconds 100.0)
  elapsed' <- hold elapsed $ 0 <$ D.button [P.onClick] [D.text "Reset"]
  pure (st{elapsed=elapsed', duration=duration})

{- TODO: The rest of the widgets still need to be implemented -}

crudWidget :: forall a. Widget HTML a
crudWidget = D.text "placeholder"

circleWidget :: forall a. Widget HTML a
circleWidget = D.text "placeholder"

cellsWidget :: forall a. Widget HTML a
cellsWidget = D.text "placeholder"
