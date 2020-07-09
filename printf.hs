{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits
import Data.Kind (Type)
import Data.Proxy (Proxy(..))

data a1 :<< a2
infixr 5 :<<

class HasPrintf a where
    type Printf a :: Type
    format :: String -> Proxy a -> Printf a
    pure_printf :: Proxy a -> Printf a
    pure_printf = format ""
    -- | printf has an ambiguous type
    printf :: Printf a
    printf = format "" (Proxy @ a)



instance KnownSymbol a => HasPrintf a where
    type Printf a = String
    format s _ = s <> symbolVal (Proxy @ a)

instance (KnownSymbol b, HasPrintf a) => HasPrintf (b :<< a) where
    type Printf (b :<< a) = Printf a
    format s _ = format (s <> symbolVal (Proxy @b)) (Proxy @a)

instance ( HasPrintf a, Show (b :: Type) ) => HasPrintf (b :<< a) where
    type Printf (b :<< a) = b -> Printf a
    format s _ b = format (s <> show b) (Proxy @ a)

z :: String
z = printf @ (Int :<< "+" :<< Int :<< "=3 : ") 1 2
