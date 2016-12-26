{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

--  class Optional<A> {
--    Optional(A a) {} // Full
--    Optional() {} // Empty
--  }

-- Optional è un data type che consente di modellare la presenza o l'assenza di un dato
-- è un contenitore che assume il valore Empty se non è presente il dato
data Optional a = Full a | Empty deriving (Eq, Show)

mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional _ Empty    = Empty
mapOptional f (Full a) = Full (f a)

-- bindOptional è una funzione che prende
--       - una "funzione che vuole una variabile e restituisce un Optional"
--       - un Optional
-- ed estrae il contenuto dell'Optional e ci applica la funzione (che già di suo restituisce un Optional)
-- a meno che non sia empty, in quel caso restituisce empty
bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional _ Empty    = Empty
bindOptional f (Full a) = f a

(??) :: Optional a -> a -> a
Empty ?? d  = d
Full a ?? _ = a

(<+>) :: Optional a -> Optional a -> Optional a
Empty <+> o = o
k <+> _     = k

-- in applyOptional ho una funzione in un contenitore, e un valore in un contenitore
-- applyOptional, nel suo happy path,  estrae la funzione dal contenitore, estrae il valore, applica la funzione e
-- mette il risultato in un contenitore
applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional (\a' -> f' a') a) f


-- in twiceOptional ho una funzione che vuole due argomenti e due valori racchiusi dentro il loro contenitore
-- twiceOptional, nel suo happy path,  estrae dai contenitori il primo e il secondo valore, poi applica la funzione e
-- infine mette il risultato in un contenitore
twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Functor Optional where
  fmap =
    M.liftM

instance A.Applicative Optional where
  (<*>) =
    M.ap
  pure =
    Full

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full
