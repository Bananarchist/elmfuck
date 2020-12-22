module BFParser exposing (..)

import Parser as P exposing ((|.), (|=))
import Regex as R


stripComments : String -> String
stripComments bfs =
    case R.fromString "[^\\+\\-\\<\\>\\,\\.\\[\\]]" of
        Nothing ->
            bfs

        Just r ->
            R.replace r (\_ -> "") bfs


gt : P.Parser BFOP
gt =
    chompedRunLength '>'
        |> P.map Ptr


lt : P.Parser BFOP
lt =
    chompedRunLength '<'
        |> P.map negate
        |> P.map Ptr


plus : P.Parser BFOP
plus =
    chompedRunLength '+'
        |> P.map Byte


minus : P.Parser BFOP
minus =
    chompedRunLength '-'
        |> P.map negate
        |> P.map Byte


block : P.Parser BFOP
block =
    P.sequence
        { start = "["
        , separator = ""
        , end = "]"
        , spaces = P.spaces
        , item = bfop
        , trailing = P.Optional
        }
        |> P.map Block


runLengthHelper : String -> Int -> P.Parser (P.Step Int Int)
runLengthHelper char state =
    P.oneOf
        [ P.succeed (\_ -> P.Loop (state + 1))
            |= P.symbol char
        , P.succeed ()
            |> P.map (\_ -> P.Done state)
        ]


bfop : P.Parser BFOP
bfop =
    P.oneOf
        [ plus
        , minus
        , gt
        , lt
        , P.map (\_ -> Print) (P.symbol ".")
        , P.map (\_ -> Read) (P.symbol ",")
        , P.lazy (\_ -> block)
        ]


parser : P.Parser (List BFOP)
parser =
    P.loop [] parserHelper


parserHelper : List BFOP -> P.Parser (P.Step (List BFOP) (List BFOP))
parserHelper stmts =
    P.oneOf
        [ P.succeed (\stmt -> P.Loop (stmt :: stmts))
            |= bfop
        , P.succeed ()
            |> P.map (\_ -> P.Done stmts)
        ]


chompedRunLength : Char -> P.Parser Int
chompedRunLength char =
    (P.getChompedString <|
        P.succeed ()
            |. P.chompIf (\c -> c == char)
            |. P.chompWhile (\c -> c == char)
    )
        |> P.map String.length


type BFOP
    = Byte Int
    | Ptr Int
    | Print
    | Read
    | Block (List BFOP)


parse : String -> Result (List P.DeadEnd) (List BFOP)
parse string =
    string
        |> stripComments
        |> P.run parser
