import Control.Monad.State

-- newtype Stack a = Stack {getA :: a} 
type Stack = [Int]

pop :: Stack -> State Stack Int
pop (x:xs) = state (x,xs)