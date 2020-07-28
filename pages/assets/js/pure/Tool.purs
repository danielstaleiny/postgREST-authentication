module Tool where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console


-- Ex1
-- import Effect.Aff (Aff, launchAff, launchAff_)
-- Ex2
-- import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
-- Ex3
import Effect.Aff (Aff,Milliseconds(..), delay, forkAff, joinFiber, launchAff_)
-- Ex4
import Effect.Aff (Aff,Milliseconds(..), delay, suspendAff, joinFiber, launchAff_)

-- forkAff -- create Future promise and start execution imidiately.
-- suspendAff -- create Future promise and waits for execution. It starts execution when it is called.
-- when calling joinFiber multiple times, it is cached. Only 1 execution with multiple call to Fiber.



-- log is the same as Console.log but lifted to Aff
log :: String -> Aff Unit
-- log msg = liftEffect $ Console.log msg -- Needs wrapping with $ ()
log = Console.log >>> liftEffect -- PIPE Input goes to Front
-- log = liftEffect <<< Console.log -- COMPOSE input goes to Back

-- logShow = Console.logShow >>> liftEffect

-- main :: Effect Unit
-- main = do
--   Console.log "This is an Effect computation (Effect monadic context).\n"
--   void $ launchAff do
--     log "This is an Aff computation (Aff monadic context)."
--     log "Aff provides the `launchAff` function that enables an \
--                \Aff computation to run inside an Effect monadic context.\n"
--   launchAff_ do
--     log "`launchAff_` is just `void $ launchAff`.\n"
--   Console.log "Program finished."

-- main :: Effect Unit
-- main = launchAff_ do
--   log "Let's print something to the console and then \
--              \wait 1 second before printing another thing."

--   delay $ Milliseconds 1000.0 -- 1 second

--   log "1 second has passed."
--   log "Program finished."



-- forkAff :: forall a. Aff a -> Aff (Fiber a)
-- forkAff
-- launchAff :: forall a. Aff a -> Effect (Fiber a)
-- launchAff


-- main :: Effect Unit
-- main = launchAff_ do
--   let
--     fiber1 = "Fiber 1"
--     fiber2 = "Fiber 2"
--     fiber3 = "Fiber 3"

--   log "Let's run multiple computations concurrently. Then, \
--              \we'll use `joinFiber` to ensure all computations have \
--              \finished before we do another computation."

-- -- creates Future Promise which is Canceleable
--   firstFiber <- forkAff do
--     log $ fiber1 <> ": Waiting for 1 second until completion."
--     delay $ Milliseconds 100.0
--     log $ fiber1 <> ": Finished computation."

--   secondFiber <- forkAff do
--     log $ fiber2 <> ": Computation 1 (takes 300 ms)."
--     delay $ Milliseconds 300.0
--     log $ fiber2 <> ": Computation 2 (takes 300 ms)."
--     delay $ Milliseconds 300.0
--     log $ fiber2 <> ": Computation 3 (takes 500 ms)."
--     delay $ Milliseconds 300.0
--     log $ fiber2 <> ": Finished computation."

--   thirdFiber <- forkAff do
--     log $ fiber3 <> ": Nothing to do. Just return immediately."
--     log $ fiber3 <> ": Finished computation."

-- -- When using joinFiber, this order is guarantite. #1
-- -- No matter in what how long fibers take. It always waits for 1, then it waits for 2. then for 3th.
--   joinFiber firstFiber
--   log $ fiber1 <> " has finished. Now joining on " <> fiber2
-- -- #2
--   joinFiber secondFiber
--   log $ fiber3 <> " has finished. Now joining on " <> fiber3
-- -- #3
--   joinFiber thirdFiber
--   log $ fiber3 <> " has finished. All fibers have finished their \
--                          \computation."

--   log "Program finished."
--

-- main :: Effect Unit
-- main = launchAff_ do
--   let
--     fiber1 = "Fiber 1"
--     fiber2 = "Fiber 2"
--     fiber3 = "Fiber 3"

--   log "Let's setup multiple computations. Then, we'll use \
--              \`joinFiber` to actually run the computations for the first time. \
--              \When they run, they will block until finished.\n"

--   log "Setting up the first fiber, but not yet running its computation."
--   firstFiber <- suspendAff do
--     log $ fiber1 <> ": Waiting for 1 second until completion."
--     delay $ Milliseconds 1000.0
--     log $ fiber1 <> ": Finished computation."

--   log "Setting up the second fiber, but not yet running its computation."
--   secondFiber <- suspendAff do
--     log $ fiber2 <> ": Computation 1 (takes 300 ms)."
--     delay $ Milliseconds 300.0

--     log $ fiber2 <> ": Computation 2 (takes 300 ms)."
--     delay $ Milliseconds 300.0

--     log $ fiber2 <> ": Computation 3 (takes 500 ms)."
--     delay $ Milliseconds 500.0
--     log $ fiber2 <> ": Finished computation."

--   log "Setting up the third fiber, but not yet running its computation."
--   thirdFiber <- suspendAff do
--     log $ fiber3 <> ": Nothing to do. Just return immediately."
--     log $ fiber3 <> ": Finished computation."

--   delay $ Milliseconds 1000.0

--   log "Now running the first fiber's computation and blocking \
--              \until it finishes."
--   joinFiber firstFiber

--   log "Now running the second fiber's computation and blocking \
--              \until it finishes."
--   joinFiber secondFiber

--   log "Now running the third fiber's computation and blocking \
--              \until it finishes."
--   joinFiber thirdFiber

--   log $ "All fibers have finished their computation."

--   log "Program finished."
