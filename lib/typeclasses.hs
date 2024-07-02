{-# LANGUAGE MultiParamTypeClasses #-}

class ToList a b where
  toDepthList :: a -> [b]

class ToSMT-LIB a where
  parseToString :: a -> String
  -- parseToAST :: a -> SMTLIBAST

class PropData a where
  propName :: a -> String  
