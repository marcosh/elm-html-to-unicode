module HtmlToUnicode (convert) where

{-|
@docs convert
-}

import String
import Char
import Dict
import Result


{-|
-}
convert : String -> String
convert string = string
    |> String.toList -- convert to a list of chars
    |> convertChars
    |> List.map String.fromChar -- convert to a list of one character strings
    |> String.concat


convertChars : List Char -> List Char
convertChars list = parser list [] []


-- first parameter: list of characters to parsed
-- second parameter: list of characters that are on hold to create the next character
-- third parameter: list of characters already parsed
-- return value: parsed list of characters
parser : List Char -> List Char -> List Char -> List Char
parser charsToBeParsed charsOnParsing charsParsed =
    let
        maybeNextChar = List.head charsToBeParsed
        tailToBeParsed = Maybe.withDefault [] ( List.tail charsToBeParsed )
    in
        case maybeNextChar of
            Nothing -> charsParsed
            Just nextChar ->
                if nextChar == '&' then
                    parser tailToBeParsed [ nextChar ] charsParsed
                else if nextChar == ';' then
                    parser tailToBeParsed [] ( List.append charsParsed ( unicodeConverter nextChar charsOnParsing ))
                else if not ( List.isEmpty charsOnParsing ) then
                    parser tailToBeParsed ( List.append charsOnParsing [ nextChar ] ) charsParsed
                else
                    parser tailToBeParsed [] ( List.append charsParsed [ nextChar ] )


unicodeConverter : Char -> List Char -> List Char
unicodeConverter post list =
    case list of
        [] -> [ post ]
        head::tail ->
            noAmpUnicodeConverter head post tail


-- first parameter: character to prepend if the conversion is not possible
-- second parameter : character to append if the conversion is not possible
-- third parameter: list of characters to convert
noAmpUnicodeConverter : Char -> Char -> List Char -> List Char
noAmpUnicodeConverter pre post list =
    case list of
        [] -> [ pre, post ]
        head::tail ->
            if head == '#' then
                convertNumericalCode [ pre, head ] [ post ] tail
            else
                convertFriendlyCode [ pre ] [ post ] ( head::tail )


convertCode : ( String -> Maybe a ) -> ( a -> List Char ) -> List Char -> List Char -> List Char -> List Char
convertCode mayber lister pre post list =
    let
        string = String.fromList list
        maybe = mayber string
    in
        case maybe of
            Nothing -> List.concat [ pre, list, post ]
            Just something -> lister something


convertNumericalCode : List Char -> List Char -> List Char -> List Char
convertNumericalCode =
    convertCode ( String.toInt >> Result.toMaybe ) ( \int -> [ Char.fromCode int ])


convertFriendlyCode : List Char -> List Char -> List Char -> List Char
convertFriendlyCode =
    convertCode convertFriendlyCodeToChar ( \char -> [ char ])


convertFriendlyCodeToChar : String -> Maybe Char
convertFriendlyCodeToChar string = Dict.get string friendlyConverterDictionary


friendlyConverterDictionary =
    Dict.fromList <| List.map (\(a,b) -> (a, Char.fromCode b))
        [ ( "quot", 34 )
        , ( "amp", 38 )
        , ( "lt", 60 )
        , ( "gt", 62 )
        , ( "nbsp", 160 )
        , ( "iexcl", 161 )
        , ( "cent", 162 )
        , ( "pound", 163 )
        , ( "curren", 164 )
        , ( "yen", 165 )
        , ( "brvbar", 166 )
        , ( "sect", 167 )
        , ( "uml", 168 )
        , ( "copy", 169 )
        , ( "ordf", 170 )
        , ( "laquo", 171 )
        , ( "not", 172 )
        , ( "shy", 173 )
        , ( "reg", 174 )
        , ( "macr", 175 )
        , ( "deg", 176 )
        , ( "plusmn", 177 )
        , ( "sup2", 178 )
        , ( "sup3", 179 )
        , ( "acute", 180 )
        , ( "micro", 181 )
        , ( "para", 182 )
        , ( "middot", 183 )
        , ( "cedil", 184 )
        , ( "sup1", 185 )
        , ( "ordm", 186 )
        , ( "raquo", 187 )
        , ( "frac14", 188 )
        , ( "frac12", 189 )
        , ( "frac34", 190 )
        , ( "iquest", 191 )
        , ( "Agrave", 192 )
        , ( "Aacute", 193 )
        , ( "Acirc", 194 )
        , ( "Atilde", 195 )
        , ( "Auml", 196 )
        , ( "Aring", 197 )
        , ( "AElig", 198 )
        , ( "Ccedil", 199 )
        , ( "Egrave", 200 )
        , ( "Eacute", 201 )
        , ( "Ecirc", 202 )
        , ( "Euml", 203 )
        , ( "Igrave", 204 )
        , ( "Iacute", 205 )
        , ( "Icirc", 206 )
        , ( "Iuml", 207 )
        , ( "ETH", 208 )
        , ( "Ntilde", 209 )
        , ( "Ograve", 210 )
        , ( "Oacute", 211 )
        , ( "Ocirc", 212 )
        , ( "Otilde", 213 )
        , ( "Ouml", 214 )
        , ( "times", 215 )
        , ( "Oslash", 216 )
        , ( "Ugrave", 217 )
        , ( "Uacute", 218 )
        , ( "Ucirc", 219 )
        , ( "Uuml", 220 )
        , ( "Yacute", 221 )
        , ( "THORN", 222 )
        , ( "szlig", 223 )
        , ( "agrave", 224 )
        , ( "aacute", 225 )
        , ( "acirc", 226 )
        , ( "atilde", 227 )
        , ( "auml", 228 )
        , ( "aring", 229 )
        , ( "aelig", 230 )
        , ( "ccedil", 231 )
        , ( "egrave", 232 )
        , ( "eacute", 233 )
        , ( "ecirc", 234 )
        , ( "euml", 235 )
        , ( "igrave", 236 )
        , ( "iacute", 237 )
        , ( "icirc", 238 )
        , ( "iuml", 239 )
        , ( "eth", 240 )
        , ( "ntilde", 241 )
        , ( "ograve", 242 )
        , ( "oacute", 243 )
        , ( "ocirc", 244 )
        , ( "otilde", 245 )
        , ( "ouml", 246 )
        , ( "divide", 247 )
        , ( "oslash", 248 )
        , ( "ugrave", 249 )
        , ( "uacute", 250 )
        , ( "ucirc", 251 )
        , ( "uuml", 252 )
        , ( "yacute", 253 )
        , ( "thorn", 254 )
        , ( "yuml", 255 )
        , ( "euro", 8364 )
        ]
