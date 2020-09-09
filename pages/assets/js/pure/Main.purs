module Main where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
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
import Foreign.Object as Object
import Web.DOM.DOMTokenList (add, remove, toggle) as DOM
import Web.DOM.Element (Element, classList, className, fromEventTarget, getAttribute, getElementsByTagName, setClassName, toEventTarget, toNode)
import Web.DOM.HTMLCollection (toArray, namedItem)
import Web.DOM.Node (textContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Text (fromNode, wholeText)
import Web.Event.Event (EventType(..), preventDefault, currentTarget, target)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (hidden, offsetParent, setHidden)
import Web.HTML.HTMLInputElement (HTMLInputElement, fromElement, value)
import Web.HTML.Window (document)

log_ :: String -> Aff Unit
         -- log msg = liftEffect $ Console.log msg -- Needs wrapping with $ ()
log_ = log >>> liftEffect -- PIPE Input goes to Front

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

-- toggleHidden :: Element -> Effect Unit
-- toggleHidden elem = do
--   let
--     htmlele_ = fromElement elem
--   case htmlele_ of
--     (Nothing) -> log "Didn't find element"
--     (Just htmlele) -> do
--       b <- hidden htmlele
--       a <- case b of
--         true -> setHidden false htmlele
--         false -> setHidden true htmlele
--       pure a

-- toggleHidden_ :: Maybe Element -> Effect Unit
-- toggleHidden_ Nothing = log "Didn't find element"

-- toggleHidden_ (Just elem) = toggleHidden elem

addClickEvent :: EventListener -> Element -> Effect Unit
addClickEvent cb elem = do
  let et = toEventTarget elem
  addEventListener (EventType "click") cb false et


addSubmitEvent :: EventListener -> Element -> Effect Unit
addSubmitEvent cb elem = do
  let et = toEventTarget elem
  addEventListener (EventType "submit") cb false et

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



type Profile = { email :: String, password :: String } 

main :: Effect Unit
main = launchAff_ do
  doc <- liftEffect $ window >>= document
  elem_ <- liftEffect $ toNonElementParentNode >>> getElementById "formSubmit" $ doc -- Maybe elem
  fn <- do -- Event -> Effect a
    liftEffect $ eventListener $ \evt -> launchAff_ do
      liftEffect $ preventDefault evt
      case target evt of
        (Nothing) -> ignore
        (Just evtTarget) -> do
          case fromEventTarget evtTarget of
            (Nothing) -> ignore
            (Just elem) -> do
              inputs <- liftEffect $ getElementsByTagName "input" elem -- get array of inputs under form  -- Effect HTMLCollection
              -- namedItem looks for id or name string
              email_ <- liftEffect $ namedItem "email" inputs -- Maybe Element
              password_ <- liftEffect $ namedItem "password" inputs -- Maybe Element
              email <- case email_ of
                (Nothing) -> pure ""
                (Just elementko) -> do
                  case fromElement elementko of
                    (Nothing) -> pure ""
                    (Just tttt) -> do -- HTMLInputElement
                      text <- liftEffect $ value tttt
                      pure text

              password <- case password_ of
                (Nothing) -> pure ""
                (Just elementko) -> do
                  case fromElement elementko of
                    (Nothing) -> pure ""
                    (Just tttt) -> do -- HTMLInputElement
                      text <- liftEffect $ value tttt
                      pure text


              -- -- do validation
              let profile = {email: email, password: password} :: Profile
              result <- AX.post ResponseFormat.json "http://localhost:3000/rpc/login" (Just (RequestBody.json (encodeJson profile)))
              case result of
                Left err -> liftEffect $ log $ "GET /api response failed to decode: " <> AX.printError err
                Right response -> liftEffect $ log $ "GET /api response: " <> J.stringify response.body
              -- traceM email
              -- traceM password
              log_ "fn, Clicky2"

  liftEffect $ fromMaybe ignore $ addSubmitEvent fn <$> elem_
  log_ "hello"

  -- doc <- liftEffect $ window >>= document
  -- elem_ <- liftEffect $ toNonElementParentNode >>> getElementById "formSubmit" $ doc -- Maybe elem
  -- case elem_ of
  --   (Nothing) -> ignore
  --   (Just elem) -> do
  --     inputs <- liftEffect $ getElementsByTagName "input" elem -- get array of inputs under form  -- Effect HTMLCollection
  --     -- namedItem looks for id or name string
  --     fname <- liftEffect $ namedItem "fname" inputs -- Maybe Element
  --     lname <- liftEffect $ namedItem "lname" inputs -- Maybe Element
  --     -- do validation
  --     -- result <- AX.request (AX.defaultRequest { url = "http://localhost:3000/", method = Left GET, responseFormat = ResponseFormat.json })
  --     -- case result of
  --     --   Left err -> liftEffect $ log $ "GET /api response failed to decode: " <> AX.printError err
  --     --   Right response -> liftEffect $ log $ "GET /api response: " <> J.stringify response.body
  --     traceM fname
  --     traceM lname
  --     -- traceM inputs
  -- log_ "should works"


--
  -- doc <- window >>= document
  -- elem_ <- toNonElementParentNode >>> getElementById "formSubmit" $ doc -- Maybe elem
  -- case elem_ of
  --   (Nothing) -> ignore
  --   (Just elem) -> do
  --     inputs <- getElementsByTagName "input" elem -- get array of inputs under form  -- Effect HTMLCollection
  --     -- namedItem looks for id or name string
  --     fname <- namedItem "fname" inputs -- Maybe Element
  --     lname <- namedItem "lname" inputs -- Maybe Element
  --     -- do validation
  --     -- result <- AX.request (AX.defaultRequest { url = "http://localhost:3000/", method = Left GET, responseFormat = ResponseFormat.json })
  --     -- case result of
  --     --   Left err -> liftEffect $ log $ "GET /api response failed to decode: " <> AX.printError err
  --     --   Right response -> liftEffect $ log $ "GET /api response: " <> J.stringify response.body
  --     traceM fname
  --     traceM lname
  --     traceM inputs

  -- fn <- do -- Event -> Effect a
  --   eventListener $ \evt -> do
  --     preventDefault evt
  --     case target evt of
  --       (Nothing) -> ignore
  --       (Just evtTarget) -> do
  --         traceM elem_
  --         log "fn, Clicky2"

  --     log "fn, Clicky"

  -- fromMaybe ignore $ addClickEvent fn <$> elem_

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
