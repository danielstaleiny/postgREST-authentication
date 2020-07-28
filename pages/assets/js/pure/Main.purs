module Main where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd)
import Data.Unit (unit)
import Debug.Trace (spy, trace, traceM)
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, joinFiber, launchAff, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (empty, new, put, read, tryPut, tryRead, tryTake, take) as Avar
import Effect.Aff.Bus (BusW', make, read, split, write)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Timer (clearTimeout, setTimeout)
import Web.DOM.DOMTokenList (add, remove, toggle) as DOM
import Web.DOM.Element (Element, classList, className, setClassName, toNode, toEventTarget)
import Web.DOM.Node (textContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (fromElement, hidden, setHidden)
import Web.HTML.Window (document)

-- Note: if you want default value in function, use record {}
-- Note: Use AX.request for using headers, or more tailored request.
-- If you don't need to use headers, or you want to put them to uri, use GET POST etc.
--
-- AX.GE POST etc. have variantion for ignoring response.
-- main :: Effect Unit
-- main = launchAff_ do
--   result <- AX.request (AX.defaultRequest { url = "https://dog.ceo/api/breeds/image/random", method = Left GET, responseFormat = ResponseFormat.json })
--   case result of
--     Left err -> liftEffect $ log $ "GET /api response failed to decode: " <> AX.printError err
--     Right response -> liftEffect $ log $ "GET /api response: " <> J.stringify response.body
-- There is import Web.DOM.MutationObserver have a look how to implement it. It might be exactly what I need.
-- Have a look on how to implement simple ( throttle. )
-- Have a look on how to implement simple delay.
-- Have a look on how to implement simple debounce.
-- import Web.DOM.Document (createElement)
-- >>= bind
-- <$> map or fmap from haskell
-- >>> pipe
-- String <- Effect String
-- name <- value
-- M a | a = String, Maybe etc.
-- let name = value
-- getJson = unit
-- post = unit
-- get = unit
--
-- void :: forall f a. Functor f => f a -> f Unit
--
fadeToggle :: Element -> Effect Unit
fadeToggle elem = do
  classList <- classList elem
  bool <- DOM.toggle classList "opacity-0"
  -- void $ pure bool
  logShow bool

-- toggle element add or remove element and return bool value if it added or remove it.
-- remove false
-- added true
fadeToggle_ :: Maybe Element -> Effect Unit
fadeToggle_ Nothing = log "couldn't find it"

fadeToggle_ (Just elem) = fadeToggle elem

fadeIn :: Element -> Effect Unit
fadeIn elem = do
  classList <- classList elem
  DOM.remove classList "opacity-0"

fadeIn_ :: Maybe Element -> Effect Unit
fadeIn_ Nothing = log "couldn't find it"

fadeIn_ (Just elem) = fadeIn elem

fadeOut :: Element -> Effect Unit
fadeOut elem = do
  classList <- classList elem
  DOM.add classList "opacity-0"

fadeOut_ :: Maybe Element -> Effect Unit
fadeOut_ Nothing = log "couldn't find it"

fadeOut_ (Just elem) = fadeOut elem

toggleHidden :: Element -> Effect Unit
toggleHidden elem = do
  let
    htmlele_ = fromElement elem
  case htmlele_ of
    (Nothing) -> log "Didn't find element"
    (Just htmlele) -> do
      b <- hidden htmlele
      a <- case b of
        true -> setHidden false htmlele
        false -> setHidden true htmlele
      pure a

toggleHidden_ :: Maybe Element -> Effect Unit
toggleHidden_ Nothing = log "Didn't find element"

toggleHidden_ (Just elem) = toggleHidden elem

addClickEvent :: EventListener -> Element -> Effect Unit
addClickEvent cb elem = do
  let
    et = toEventTarget elem
  addEventListener (EventType "click") cb false et

-- JS addEventListener
-- options = {passive: true}
-- false -- indicating thot useCapture is false, Add this for best compatibility.
-- expect to have opacity-0 on element
-- alternatively add opacity-100 instead
-- test if you have opacity-0 and opacity-100 what has preccedance.
-- el.classList.add('transition-opacity');
-- el.classList.add('duration-300');
-- el.classList.remove('opacity-0');
-- resolve Nothing = pure "ok"
-- resolve _ = pure "ok"
ignore :: forall m. Applicative m => m Unit
ignore = pure unit

ignore_ :: forall m a. a -> Applicative m => m Unit
ignore_ _ = pure unit

-- main :: Effect Unit
-- main = launchAff_ $ liftEffect do
--   log "works"
--
--
getElem :: Effect (Maybe Element)
getElem = do
  window >>= document >>= toNonElementParentNode >>> (getElementById "hide-elem")

hook bus = do
  promise <-
    forkAff
      $ do
          state <- read bus
          liftEffect $ traceM $ state.text <> " from loop"
          hook bus
  joinFiber promise

hook2 bus = do
  promise <-
    forkAff
      $ do
          state <- read bus
          liftEffect $ traceM $ state.text2 <> " text2 from loop2"
          hook2 bus
  joinFiber promise

--
-- Avar = timeout = null
-- clearTimeout(timeout)
-- setTimeout time func()
-- set Avar = timeout = timereff
--
-- Avar.empty -- creates empty Avar
-- Avar.tryTake -- returns maybe avar value, leaves it empty, takes it and puts empty in
-- Avar.tryRead -- returns maybe avar value, leaves it empty, just reads it.
-- Avar.tryPut -- tries to fill it, and when it is already filled, do nothing. Maybe this is the one I need ??
-- put -- Sets the value of the AVar. If the AVar is already filled, it will be queued until the value is emptied. Multiple puts will resolve in order as the AVar becomes available.
--
-- This calls the function stack after certain time.
-- debounce wait = do
--   state <- Avar.empty -- empty state
--   reff_ <- Avar.tryTake state
--   _ <- liftEffect $ case reff_ of
--     Nothing -> ignore
--     Just reff -> clearTimeout reff
--   newReff <- liftEffect $ setTimeout wait (log "Yaay from debounce")
--   _ <- Avar.tryPut newReff state
--   liftEffect $ log "done"
-- Avar.empty
-- Avar.tryTake
-- Avar.tryRead
-- Avar.tryPut
-- Avar.put
-- function throttle (callback, limit) {
--   var wait = false;
--   return function () {
--     if (!wait) {
--       callback.apply(null, arguments);
--       wait = true;
--       setTimeout(function () {
--         wait = false;
--       }, limit);
--     }
--   }
-- }
initThrottleState :: Aff (AVar Boolean)
initThrottleState = Avar.new true

throttle :: AVar Boolean -> Int -> Aff Unit -> Aff Unit
throttle state waiting func = do
  continue <- Avar.take state
  case continue of
    true -> do
      Avar.put false state
      _ <-
        liftEffect $ setTimeout waiting
          $ do
              launchAff_ do
                _ <- Avar.take state
                Avar.put true state
      func
      ignore
    false -> Avar.put false state

-- | Ths function updates bus when event is triggered. Throttled by miliseconds.
-- | Int -> Takes time in miliseconds, for Throttle function
-- | BusW' -> Takes writeable Bus with type `a`
-- | a -> Takes new state to be written of type `a`
updateEvent :: forall a r. Int -> BusW' r a -> a -> Aff EventListener
updateEvent time bus state = do
  initState <- initThrottleState
  liftEffect $ eventListener
    $ \evt ->
        launchAff_ $ throttle initState time
          $ do
              _ <- forkAff $ write state bus
              ignore

main :: Effect Unit
main =
  launchAff_ do
    -- Needs, AVar for keeping prev, state to know which bus to update and which one, if you need it for input fields.
    -- Try to save primitives values into bus, to benefit from it the most. Otherwise it will push new object and everything has to re-render.
    -- It needs totally different strategy if the bus is primitive, object, or array/list
    -- For all it is nice to check if it changed and if it didn't don't trigger bus.
    -- For primitives, re-render everything it uses that value
    -- For Object it re-renders also parts which doesn't need to be re-rendered.
    -- For Array state needs to be passed down to function to figure out what to add. Find the diff and resolve it.
    bus <- make -- Initalize buss. -> BusRW a.
    elem_ <- liftEffect getElem
    fn <- updateEvent 1000 bus { text: "Text from the BUSS", text2: "" }
    liftEffect $ fromMaybe ignore $ addClickEvent fn <$> elem_
    fn2 <-
      updateEvent 1000 bus
        { text: "text"
        , text2: "Text from the BUSS"
        }
    liftEffect $ fromMaybe ignore $ addClickEvent fn2 <$> elem_
    _ <- forkAff $ hook bus
    _ <- forkAff $ hook2 bus
    ignore

-- joinFiber
-- write "something new" bus
-- text <- read bus
-- liftEffect $ traceM $ "Welcome"
-- bus <- make -- Initalize buss. -> BusRW a.
-- liftEffect $ traceM $ bus
-- liftEffect $ traceM $ "Read bus"
-- liftEffect $ traceM $ read bus
-- liftEffect $ traceM $ "Bus 1"
-- liftEffect $ traceM $ bus
-- liftEffect $ traceM $ "Write bus"
-- liftEffect $ traceM $ write "Hello from the bus" bus
-- liftEffect $ traceM $ "Bus 2"
-- liftEffect $ traceM $ bus
-- liftEffect $ traceM $ "Read bus 3"
-- liftEffect $ traceM $ read bus
-- liftEffect $ traceM $ "Bus  4"
-- liftEffect $ traceM $ bus
-- bus <- make -- Initalize buss. -> BusRW a.
-- write "something new" bus
-- text <- read bus
-- liftEffect $ log text
-- liftEffect $ spy "show bus" $ traceM bus
-- liftEffect $ spy "read bus" $ traceM $ read bus
-- liftEffect $ spy "Bus 1" $ traceM $ bus
-- liftEffect $ spy "Write bus" $ traceM $ write "Hello from the bus" bus
-- liftEffect $ spy "Bus 2" $ traceM $ bus
-- liftEffect $ spy "read bus 2" $ traceM $ read bus
-- liftEffect $ spy "Bus 3" $ traceM $ bus
-- bus
-- read bus
-- pure bus
-- write "Hello from the bus" bus
-- pure bus
-- read bus
-- pure bus
-- liftEffect $ spy "show bus" $ traceM bus
-- liftEffect $ spy "read bus" $ traceM $ read bus
-- liftEffect $ spy "Bus 1" $ traceM $ bus
-- liftEffect $ spy "Write bus" $ traceM $ write "Hello from the bus" bus
-- liftEffect $ spy "Bus 2" $ traceM $ bus
-- liftEffect $ spy "read bus 2" $ traceM $ read bus
-- liftEffect $ spy "Bus 3" $ traceM $ bus
-- traceM $ read bus
-- trace "write bus"
-- write "Hello from the bus" bus
-- that <- read bus
-- traceM bus
-- traceM bus
-- write "Hello from the bus2" bus
-- liftEffect $ traceM that
-- liftEffect $ traceM that
-- liftEffect $ traceM bus
-- fn <- do -- Event -> Effect a
--   eventListener $ \evt -> log "fn, Clicky"
-- fn1 <- do -- Event -> Effect a
--   eventListener $ \evt -> log "fn1, Clicky"
-- fromMaybe ignore $ addClickEvent fn <$> elem_
-- fromMaybe ignore $ addClickEvent fn <$> elem2_
-- case elem_ of
--      Nothing -> ignore
--      Just elem -> addClickEvent fn1 elem
-- log "Main finished."
-- main :: Effect Unit
-- main = launchAff_ $ liftEffect do
--   doc <- window >>= document
--   elem_ <- toNonElementParentNode >>> getElementById "hide-elem" $ doc -- Maybe elem
--   elem2_ <- toNonElementParentNode >>> getElementById "hide-elem2" $ doc -- Maybe elem
--   fn <- do -- Event -> Effect a
--     eventListener $ \evt -> log "fn, Clicky"
--   fn1 <- do -- Event -> Effect a
--     eventListener $ \evt -> log "fn1, Clicky"
--   -- If you add same EventListener to same element it will not be added 2 times. or called 2 times. Only onced.
--   -- But if you add same EventListener into 2 different elements, then it would be called correctly 2 times.
--   fromMaybe ignore $ addClickEvent fn <$> elem_
--   fromMaybe ignore $ addClickEvent fn <$> elem2_
--   case elem_ of
--        Nothing -> ignore
--        Just elem -> addClickEvent fn1 elem
--   log "Main finished."
-- Evt.addEventListener
-- Evt.removeEventListener
-- fadeToggle_ elem_
-- toggleHidden_ elem_
-- fadeIn_ elem_ -- Maybe Effect unit
-- elem1 <- fadeIn <$> elem
-- fromMaybe "ok" <$> elem1 >>> pure
-- log
getById :: String -> String -> Effect String
getById fallback id =
  window
    >>= document
    >>= toNonElementParentNode
    >>> pure
    >>= getElementById id
    >>= map toNode
    >>> map textContent
    >>> fromMaybe (pure fallback)

-- forall a. Show a => a -> Effect Unit
-- logShow
--
-- String -> Effect Unit
-- log
-- forall a m. MonadEffect m => Effect a -> m a
-- liftEffect
--
-- Milliseconds -> Aff Unit
-- delay
--
-- forall a. Aff a -> Aff (Fiber a)
-- forkAff
-- forall a. Fiber a -> Aff a
-- joinFiber
-- forall a. Aff a -> Effect Unit
-- launchAff_
-- forall a. Aff a -> Effect (Fiber a)
-- launchAff
--
--
-- main :: Effect Unit
-- main = D.trace "hello" \_ -> log "there"
-- main = D.traceM $ "hello3"
-- main = D.spy "hello" $ log "there"
-- main :: Effect Unit
-- main = getById "Couldn't find anything" "hide-elem" >>= log
