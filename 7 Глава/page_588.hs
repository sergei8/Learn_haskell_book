class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0     = False
  yesno _     = True

instance YesNo (Maybe m) where
  yesno Nothing = False
  yesno _       = True

instance YesNo Char where
  yesno ' '  = False
  yesno _   = True

instance YesNo [a] where
  yesno []  = False
  yesno _   = True
