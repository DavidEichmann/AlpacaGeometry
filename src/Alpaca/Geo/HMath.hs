{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.HMath where

-- | Hetterogenious addition.
class a :+ b where
    type a .+ b;
    (.+) :: a -> b -> a .+ b
infixr 7 .+

-- | Hetterogenious subtraction.
class a :- b where
    type a .- b;
    (.-) :: a -> b -> a .- b
infixr 7 .-

-- | Hetterogenious Multiplication.
class a :* b where
    type a .* b;
    (.*) :: a -> b -> a .* b
infixr 7 .*

-- | Hetterogenious division.
class a :/ b where
    type a ./ b;
    (./) :: a -> b -> a ./ b
infixr 7 ./
