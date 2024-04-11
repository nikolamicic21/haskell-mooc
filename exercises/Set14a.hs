module Set14a where

-- Remember to browse the docs of the Data.Text and Data.ByteString
-- libraries while working on the exercises!

import Mooc.Todo

import Data.Bits
import Data.Char
import Data.Text.Encoding
import Data.Word
import Data.Int
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Semigroup as Word8

------------------------------------------------------------------------------
-- Ex 1: Greet a person. Given the name of a person as a Text, return
-- the Text "Hello, <name>!". However, if the name is longer than 15
-- characters, output "Hello, <first 15 characters of the name>...!"
--
-- PS. the test outputs and examples print Text values as if they were
-- Strings, just like GHCi prints Texts as Strings.
--
-- Examples:
--  greetText (T.pack "Martin Freeman") ==> "Hello, Martin Freeman!"
--  greetText (T.pack "Benedict Cumberbatch") ==> "Hello, Benedict Cumber...!"

greetText :: T.Text -> T.Text
greetText text =
    if T.length text > 15
        then T.concat [T.pack "Hello, ", T.take 15 text, T.pack "...!"]
        else T.concat [T.pack "Hello, ", text, T.pack "!"]


------------------------------------------------------------------------------
-- Ex 2: Capitalize every second word of a Text.
--
-- Examples:
--   shout (T.pack "hello how are you")
--     ==> "HELLO how ARE you"
--   shout (T.pack "word")
--     ==> "WORD"

shout :: T.Text -> T.Text
shout text = T.concat $ shouldHelper 0 (T.words text)

shouldHelper :: Int -> [T.Text] -> [T.Text]
shouldHelper _ [] = []
shouldHelper index [x] = if even index
    then [T.toUpper x]
    else [x]
shouldHelper index (text:textWords) = if even index
    then T.toUpper text : T.pack " " : shouldHelper (index + 1) textWords
    else text : T.pack " " : shouldHelper (index + 1) textWords

------------------------------------------------------------------------------
-- Ex 3: Find the longest sequence of a single character repeating in
-- a Text, and return its length.
--
-- Examples:
--   longestRepeat (T.pack "") ==> 0
--   longestRepeat (T.pack "aabbbbccc") ==> 4

longestRepeatHelper :: T.Text -> Char -> Int -> Int -> Int
longestRepeatHelper text prev acc countMax = case T.uncons text of
    Nothing -> countMax
    Just (ch, rest) -> if ch == prev
        then longestRepeatHelper rest ch (acc + 1) (max countMax (acc + 1))
        else longestRepeatHelper rest ch 1 (max countMax 1)

longestRepeat :: T.Text -> Int
longestRepeat text = case T.uncons text of
    Nothing -> 0
    Just (ch, rest) -> longestRepeatHelper rest ch 1 1

------------------------------------------------------------------------------
-- Ex 4: Given a lazy (potentially infinite) Text, extract the first n
-- characters from it and return them as a strict Text.
--
-- The type of the n parameter is Int64, a 64-bit Int. Can you figure
-- out why this is convenient?
--
-- Example:
--   takeStrict 15 (TL.pack (cycle "asdf"))  ==>  "asdfasdfasdfasd"

takeStrict :: Int64 -> TL.Text -> T.Text
takeStrict n text = TL.toStrict $ takeStrictHelper n text
    where takeStrictHelper inN inText
            | inN == 0 = TL.pack ""
            | otherwise = case TL.uncons inText of
                Nothing -> TL.pack ""
                Just (ch, rest) -> TL.concat [TL.pack [ch], takeStrictHelper (inN - 1) rest]

------------------------------------------------------------------------------
-- Ex 5: Find the difference between the largest and smallest byte
-- value in a ByteString. Return 0 for an empty ByteString
--
-- Examples:
--   byteRange (B.pack [1,11,8,3]) ==> 10
--   byteRange (B.pack []) ==> 0
--   byteRange (B.pack [3]) ==> 0

byteRange :: B.ByteString -> Word8
byteRange str = byteRangeHelper str (maxBound :: Word8) (minBound :: Word8)
    where byteRangeHelper bStr currMin currMax = case B.uncons bStr of
            Nothing -> if currMin == (maxBound :: Word8) || currMax == (minBound :: Word8)
                then 0
                else currMax - currMin
            Just (next, rest) -> byteRangeHelper rest (min currMin next) (max currMax next)

------------------------------------------------------------------------------
-- Ex 6: Compute the XOR checksum of a ByteString. The XOR checksum of
-- a string of bytes is computed by using the bitwise XOR operation to
-- "sum" together all the bytes.
--
-- The XOR operation is available in Haskell as Data.Bits.xor
-- (imported into this module).
--
-- Examples:
--   xorChecksum (B.pack [137]) ==> 137
--   xor 1 2 ==> 3
--   xorChecksum (B.pack [1,2]) ==> 3
--   xor 1 (xor 2 4) ==> 7
--   xorChecksum (B.pack [1,2,4]) ==> 7
--   xorChecksum (B.pack [13,197,20]) ==> 220
--   xorChecksum (B.pack [13,197,20,197,13,20]) ==> 0
--   xorChecksum (B.pack []) ==> 0

xorChecksum :: B.ByteString -> Word8
xorChecksum str = case B.uncons str of
    Nothing -> 0
    Just (next, rest) -> next `xor` xorChecksum rest

------------------------------------------------------------------------------
-- Ex 7: Given a ByteString, compute how many UTF-8 characters it
-- consists of. If the ByteString is not valid UTF-8, return Nothing.
--
-- Look at the docs of Data.Text.Encoding to find the right functions
-- for this.
--
-- Examples:
--   countUtf8Chars (encodeUtf8 (T.pack "åäö")) ==> Just 3
--   countUtf8Chars (encodeUtf8 (T.pack "wxyz")) ==> Just 4
--   countUtf8Chars (B.pack [195]) ==> Nothing
--   countUtf8Chars (B.pack [195,184]) ==> Just 1
--   countUtf8Chars (B.drop 1 (encodeUtf8 (T.pack "åäö"))) ==> Nothing

countUtf8Chars :: B.ByteString -> Maybe Int
countUtf8Chars str = case decodeUtf8' str of
    Left error -> Nothing
    Right text -> countUtf8CharsHelper 0 text
    where countUtf8CharsHelper count txt = case T.uncons txt of
            Nothing -> Just count
            Just (next, rest) -> countUtf8CharsHelper (count + 1) rest


------------------------------------------------------------------------------
-- Ex 8: Given a (nonempty) strict ByteString b, generate an infinite
-- lazy ByteString that consists of b, reversed b, b, reversed b, and
-- so on.
--
-- Example:
--   BL.unpack (BL.take 20 (pingpong (B.pack [0,1,2])))
--     ==> [0,1,2,2,1,0,0,1,2,2,1,0,0,1,2,2,1,0,0,1]

pingpong :: B.ByteString -> BL.ByteString
pingpong str = BL.cycle $ BL.fromStrict (B.concat [str, B.reverse str])
