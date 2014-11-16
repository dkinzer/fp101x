module HW7 where

import Parsing 

-- Testing HW7 e4.
pListDigits :: Parser String
pListDigits = do char '['
                 d <- digit
                 ds <- many (do char ','
                                digit)
                 char ']'
                 return (d:ds)
