import Control.Applicative

seqA :: (Applicative f) => [f a] -> f [a]
-- seqA []     = pure []
-- seqA (x:xs) = undefined
seqA = foldr (\x acc -> (:) <$> x <*> acc) empty where
  empty = pure []
