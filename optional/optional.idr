import Data.String

data O = S | N

data Optional : O -> Type -> Type where
    Some : a -> Optional S a
    None : Optional N a

textToInt : String -> (o ** Optional o Int)
textToInt s = case parseInteger {a=Int} s of
                Just n => (_ ** Some n)
                Nothing => (_ ** None)

addText : Int -> String -> (o ** Optional o Int)
addText n s = case textToInt s of
                (_ ** Some m) => (_ ** Some (n + m))
                (_ ** None) => (_ ** None)

mapDPair : (f : a -> b) -> ({x : a} -> p x -> q (f x)) -> DPair a p -> DPair b q
mapDPair f g (o ** x) = (f o ** g x)

addText' : Int -> String -> (o ** Optional o Int)
addText' n s = mapDPair id f $ textToInt s
  where
    f : Optional o Int -> Optional o Int
    f (Some m) = Some $ n + m
    f None = None

implementation Functor (Optional o) where
    map f (Some n) = Some (f n)
    map _ None = None

addText'' : Int -> String -> (o ** Optional o Int)
addText'' n s = mapDPair id (map (+ n)) $ textToInt s

{-
data O2 = S2 | N2

data Optional2 : O2 -> Type -> Type where
    Some2 : a -> Optional2 S2 a
    None2 : Optional2 N2 a

foo : O -> O2
foo S = S2
foo N = N2


m : (f : O -> O2) -> ({o : O} -> Optional o x -> Optional2 (f o) y) -> DPair O (\o => Optional o x) -> DPair O2 (\o => Optional2 o y)
m f g (S ** o) = (f S ** g o)

n : (x : a) -> p x -> DPair a p
n = MkDPair
-}
