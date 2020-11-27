module Yas
  ( runYas
  ) where

import Import

runYas :: RIO App ()
runYas = do
  logInfo "We're inside the application!"
