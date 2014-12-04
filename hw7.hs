module HW7 where

import Parsing 

-- e4.
pListDigits :: Parser String
pListDigits = do char '['
                 d <- digit
                 ds <- many (do char ','
                                digit)
                 char ']'
                 return (d:ds)


-- e6
nat :: Parser Int
nat
  = do xs <- many1 digit
       return (read xs)

int :: Parser Int
int
  = (do char '-'
        n <- HW7.nat
        return (-n)) +++ HW7.nat


-- e7
comment :: Parser ()
comment = do string "--"
             many (sat (/= '\n'))
             return ()

-- e8
expr :: Parser Int
expr = do n <- natural
          ns <- many (do symbol "-"
                         natural)
          return (foldl (-) n ns)
