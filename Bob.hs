module Bob (responseFor) where

import Data.Char

responseFor :: String -> String

data Action = Question | Yelling | YellingQuestion | Silence | AnythingElse


responseFor said = reaction
    where
        getAction :: String -> Action
        getAction [] = Silence
        getAction (x : xs)
            | isUppercase x = yelling xs
            | isSpecialSymbolOrNumber x = specialSymbol xs
            | ord x == 20 = silence xs
            | otherwise = anythingElse xs

        action = getAction said
        reaction =
            case action of
                Question -> "Sure."
                Yelling -> "Whoa, chill out!"
                YellingQuestion -> "Calm down, I know what I'm doing!"
                Silence -> "Fine. Be that way!"
                AnythingElse -> "Whatever."


yelling :: String -> Action -- Yelling | YellingQuestion | AnythingElse
yelling (x : []) = if x == '?' then YellingQuestion else Yelling
yelling (x : xs)
    | isUppercase x = yelling xs
    | isSpecialSymbolOrNumber x = yelling xs
    | otherwise = anythingElse xs

anythingElse :: String -> Action -- AnythingElse | Question
anythingElse (x : []) = if x == '?' then Question else AnythingElse
anythingElse (x : xs) = anythingElse xs

silence :: String -> Action -- Silence | AnythingElse
silence [] = Silence
silence (x : xs)
    | ord x == 20 = silence xs
    | isSpecialSymbolOrNumber x = specialSymbol xs
    | isUppercase x = yelling xs
--silence (x : xs) = if ord x == 20 then silence xs else anythingElse xs

specialSymbol :: String -> Action -- Yelling
specialSymbol (x : []) = if x == '?' then Question else AnythingElse
specialSymbol (x : xs) = if isSpecialSymbolOrNumber x then specialSymbol xs else yelling xs

isSpecialSymbolOrNumber :: Char -> Bool
isSpecialSymbolOrNumber l = (&&) (not $ isUppercase l) (isNotLowerCase l)

isUppercase :: Char -> Bool
isUppercase c = (&&) ((<=) 65 (ord c)) ((<=) (ord c ) 90) -- 65 - 90

isNotLowerCase :: Char -> Bool
isNotLowerCase l = not ((&&) ((<=) 97 (ord l)) ((<=) (ord l) 122))










