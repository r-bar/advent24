app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import pf.File
import pf.Arg

main =
    args = Arg.list! {}
    inputFile =
        when args is
            [_progname, filename] -> filename
            [progname, ..] -> crash "Usage: $(progname) <input_file>"
            [] -> crash "unreachable"
    input = File.readUtf8! inputFile
    answer =
        tryParseAll [] input parseProgram
        |> dbg
        |> List.walk { value: 0, status: Enabled } eval
        |> .value
    Stdout.line! (Inspect.toStr answer)

State : { value : I64, status : [Enabled, Disabled] }
Expr : [Mul I64 I64, Do, Dont]
ParseResult a : Result { parsed : a, rest : Str } [ParseError]

# Roc does not like this annotation despite it being correct
# parsePregram : Str -> ParseResult Expr
parseProgram = parseFirst [parseMul, parseDo, parseDont]

parseLiteral : Str, Str -> ParseResult Str
parseLiteral = \input, literal ->
    if Str.startsWith input literal then
        Ok { parsed: literal, rest: Str.dropPrefix input literal }
    else
        Err ParseError

parseInt : Str -> ParseResult I64
parseInt = \input ->
    parseIntHelp (Err ParseError) input

parseIntHelp : ParseResult I64, Str -> ParseResult I64
parseIntHelp = \accum, input ->
    intStrs = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    charResult =
        List.findFirstIndex intStrs \char -> Str.startsWith input char
        |> Result.map \index -> (index, List.get intStrs index |> Result.withDefault "?")
    when (accum, charResult) is
        # successful start condition
        (Err _, Ok (charVal, charStr)) ->
            rest = Str.dropPrefix input charStr
            parseIntHelp (Ok { parsed: Num.toI64 charVal, rest }) rest

        # successful parse continuation
        (Ok { parsed, rest: _rest }, Ok (charVal, charStr)) ->
            rest = Str.dropPrefix input charStr
            parseIntHelp (Ok { parsed: parsed * 10 + (Num.toI64 charVal), rest }) rest

        # end successful parse
        (Ok _, Err _) -> accum
        # total parse failure
        (Err _, Err _) -> Err ParseError

parseMul : Str -> ParseResult Expr
parseMul = \input ->
    { parsed: _prefix, rest: rest1 } = try parseLiteral input "mul("
    { parsed: fstInt, rest: rest2 } = try parseInt rest1
    { parsed: _, rest: rest3 } = try parseLiteral rest2 ","
    { parsed: sndInt, rest: rest4 } = try parseInt rest3
    { parsed: _, rest: rest5 } = try parseLiteral rest4 ")"
    Ok { parsed: Mul fstInt sndInt, rest: rest5 }

parseDo : Str -> ParseResult Expr
parseDo = \input ->
    { parsed: _, rest: rest } = try parseLiteral input "do()"
    Ok { parsed: Do, rest: rest }

parseDont : Str -> ParseResult Expr
parseDont = \input ->
    { parsed: _, rest: rest } = try parseLiteral input "don't()"
    Ok { parsed: Dont, rest: rest }

parseFirst : List (Str -> ParseResult Expr) -> (Str -> ParseResult Expr)
parseFirst = \parsers -> \input ->
        List.walkUntil parsers (Err ParseError) \accum, parser ->
            when parser input is
                Ok res -> Break (Ok res)
                Err _ -> Continue accum

strSplitAt : Str, U64 -> (Str, Str)
strSplitAt = \input, n ->
    bytes = Str.toUtf8 input
    findGoodSplit : List U8, List U8, U64 -> (Str, Str)
    findGoodSplit = \before, after, beforeCodePoints ->
        if beforeCodePoints == n then
            when (Str.fromUtf8 before, Str.fromUtf8 after) is
                (Ok beforeStr, Ok afterStr) -> (beforeStr, afterStr)
                _ -> crash "unreachable"
        else
            when after is
                [] ->
                    (input, "")

                [afterHead, .. as afterTail] if afterHead < 128 ->
                    findGoodSplit (List.append before afterHead) afterTail (beforeCodePoints + 1)

                [afterHead, .. as afterTail] ->
                    findGoodSplit (List.append before afterHead) afterTail beforeCodePoints
    findGoodSplit [] bytes 0

tryParseAll : List a, Str, (Str -> ParseResult a) -> List a
tryParseAll = \accum, input, parser ->
    if input == "" then
        accum
    else
        when parser input is
            Ok { parsed, rest } ->
                List.append accum parsed
                |> tryParseAll rest parser

            Err _ ->
                (_, rest) = strSplitAt input 1
                tryParseAll accum rest parser

eval : State, Expr -> State
eval = \state, expr ->
    when (expr, state) is
        (Mul a b, { value, status: Enabled }) -> { state & value: value + a * b }
        (Do, _) -> { state & status: Enabled }
        (Dont, _) -> { state & status: Disabled }
        (_, { status: Disabled }) -> state
