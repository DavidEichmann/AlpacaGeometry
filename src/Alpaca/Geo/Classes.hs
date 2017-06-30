{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.Classes where

import           Alpaca.Geo.HMath
import           Alpaca.Geo.P2
import           Alpaca.Geo.V2

class (a :* P2, a :* V2) => Transform a
