module Bob (responseFor) where

import Data.Char

responseFor :: String -> String

data Action = Question | Yelling | YellingQuestion | Silence | AnythingElse

responseFor said = reaction
    where
        getAction :: String -> Action
        getAction [] = Silence
        getAction (x : xs)
            | isEmptySpace asciiChar = silence xs
            | isUppercase x = yelling xs
            | isSpecialCharExceptEmptySpace x = shouting xs
            | otherwise = anythingElse xs
            -- is there no question mark at the beggining of any of the test sentences

        asciiChar = ord x

        action = getAction said
        reaction =
            case action of
                Question -> "Sure."
                Yelling -> "Whoa, chill out!"
                YellingQuestion -> "Calm down, I know what I'm doing!"
                Silence -> "Fine. Be that way!"
                AnythingElse -> "Whatever."


yelling :: String -> Action
yelling (x : []) = if x == '?' then YellingQuestion else Yelling
yelling (x : xs)
    | (||) (isEmptySpace x) (isUppercase x) -> yelling xs
    | isSpecialCharExceptEmptySpace x = yelling xs
    | isQuestionMark x = yellingQuestion xs
    | otherwise = anythingElse xs

yellingQuestion :: String -> Action
yellingQuestion [] -> YellingQuestion
yellingQuestion (x : xs)
    | isEmptySpace x -> yellingQuestion xs
    | isQuestionMark x -> yellingQuestion xs
    | isUppercase x -> yelling xs
    | otherwise x -> anythingElse xs

anythingElse :: String -> Action
anythingElse (x : []) = if x == '?' then Question else AnythingElse
anythingElse (x : xs) = anythingElse xs

silence :: String -> Action
silence [] = Silence
silence (x : xs)
    | isEmptySpace x = silence xs
    | isSpecialCharExceptEmptySpace x = shouting xs
    | isUppercase x = yelling xs
    | otherwise = anythingElse xs


shouting :: String -> Action
shouting  [] = AnythingElse
shouting  (x : xs) = if isSpecialCharExceptEmptySpace x then shouting xs else yelling xs -- desmembrar isSpecial ...


------------------------------------------------------------------------------------------------------------------------------------------------

isEmptySpace :: Int -> Bool
isEmptySpace = (||) (x == 32) (x == 9) $ (||) x == 13 $ (||) x == 10 =  silence xs -- space, horizontal tab (\t), carriage return (\r), line feed (\n)

isQuestionMark :: Char -> Bool
isQuestionMark c = c == '?'

isSpecialCharExceptEmptySpace :: Char -> Bool
isSpecialCharExceptEmptySpace c = (&&) (not $ isAlphabet c) (not $ isEmptySpace c)

isAlphabet :: Char -> Bool
isAlphabet = (&&) (isUppercase l) (isLowerCase l)

isUppercase :: Char -> Bool
isUppercase c = (&&) ((<=) 65 (ord c)) ((<=) (ord c ) 90)

isLowerCase :: Char -> Bool
isLowerCase l = (&&) ((<=) 97 (ord l)) ((<=) (ord l) 122)









