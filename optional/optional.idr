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
