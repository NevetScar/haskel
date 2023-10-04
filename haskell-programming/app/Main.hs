module Main (main) where

import Lib
import Data.Char (isAsciiUpper, toLower)
import Data.List (elemIndex, find)

main :: IO ()
main = someFunc

type Digit = Char

type Presses = Int

type DSet = [Char]

data Button = Button Digit DSet deriving (Show)

newtype DaPhone = DaPhone [Button] deriving (Show)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone l) c
            | isAsciiUpper c = ('*' , 1) : (presses . toLower) c
            | otherwise      = presses c
            where
                presses y = maybe [] (\(Button d ds) -> [(d, 1 + index y ds)]) $ findKey y
                findKey x     = find (\(Button _ ds) -> x `elem` ds) l
                index z chars =
                    case elemIndex z chars of Just i -> i

keypad :: [Button]
keypad = [one, two, three, four, five, six, seven , eight, nine, star, zero, pound]
            where
                one = Button '1' "1"
                two = Button '2' "abc2"
                three = Button '3' "def3"
                four = Button '4' "ghi4"
                five = Button '5' "jkl5"
                six = Button '6' "mno6"
                seven = Button '7' "pqrs7"
                eight = Button '8' "tuv8"
                nine = Button '9' "wxyz"
                star = Button '*' "^*"
                zero = Button '0' "+ 0"
                pound = Button '#' ".,#"

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have you ever tasted alcohol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty",
         "Lol ya",
         "Just making sure rofl ur turn"]
