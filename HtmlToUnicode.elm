module HtmlToUnicode where

import List
import String
import Maybe
import Char
import Dict
import Debug


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


convertNumericalCode : List Char -> List Char -> List Char -> List Char
convertNumericalCode pre post list =
    let
        string = String.fromList list
        keyCodeResult = String.toInt string
    in
        case keyCodeResult of
            Err _ -> List.concat [ pre, list, post ]
            Ok keyCode -> [ Char.fromCode keyCode ]


convertFriendlyCode : List Char -> List Char -> List Char -> List Char
convertFriendlyCode pre post list =
    let
        string = String.fromList list
        charMaybe = convertFriendlyCodeToChar string
    in
        case charMaybe of
            Nothing -> List.concat [ pre, list, post ]
            Just char -> [ char ]


convertFriendlyCodeToChar : String -> Maybe Char
convertFriendlyCodeToChar string = Dict.get (Debug.log "string "string) friendlyConverterDictionary


friendlyConverterDictionary : Dict.Dict String Char
friendlyConverterDictionary
    = Dict.empty
    |> Dict.insert "quot" ( Char.fromCode 34 )
    |> Dict.insert "amp" ( Char.fromCode 38 )
    |> Dict.insert "lt" ( Char.fromCode 60 )
    |> Dict.insert "gt" ( Char.fromCode 62 )
    |> Dict.insert "nbsp" ( Char.fromCode 160 )
    |> Dict.insert "iexcl" ( Char.fromCode 161 )
    |> Dict.insert "cent" ( Char.fromCode 162 )
    |> Dict.insert "pound" ( Char.fromCode 163 )
    |> Dict.insert "curren" ( Char.fromCode 164 )
    |> Dict.insert "yen" ( Char.fromCode 165 )
    |> Dict.insert "brvbar" ( Char.fromCode 166 )
    |> Dict.insert "sect" ( Char.fromCode 167 )
    |> Dict.insert "uml" ( Char.fromCode 168 )
    |> Dict.insert "copy" ( Char.fromCode 169 )
    |> Dict.insert "ordf" ( Char.fromCode 170 )
    |> Dict.insert "laquo" ( Char.fromCode 171 )
    |> Dict.insert "not" ( Char.fromCode 172 )
    |> Dict.insert "shy" ( Char.fromCode 173 )
    |> Dict.insert "reg" ( Char.fromCode 174 )
    |> Dict.insert "macr" ( Char.fromCode 175 )
    |> Dict.insert "deg" ( Char.fromCode 176 )
    |> Dict.insert "plusmn" ( Char.fromCode 177 )
    |> Dict.insert "sup2" ( Char.fromCode 178 )
    |> Dict.insert "sup3" ( Char.fromCode 179 )
    |> Dict.insert "acute" ( Char.fromCode 180 )
    |> Dict.insert "micro" ( Char.fromCode 181 )
    |> Dict.insert "para" ( Char.fromCode 182 )
    |> Dict.insert "middot" ( Char.fromCode 183 )
    |> Dict.insert "cedil" ( Char.fromCode 184 )
    |> Dict.insert "sup1" ( Char.fromCode 185 )
    |> Dict.insert "ordm" ( Char.fromCode 186 )
    |> Dict.insert "raquo" ( Char.fromCode 187 )
    |> Dict.insert "frac14" ( Char.fromCode 188 )
    |> Dict.insert "frac12" ( Char.fromCode 189 )
    |> Dict.insert "frac34" ( Char.fromCode 190 )
    |> Dict.insert "iquest" ( Char.fromCode 191 )
    |> Dict.insert "Agrave" ( Char.fromCode 192 )
    |> Dict.insert "Aacute" ( Char.fromCode 193 )
    |> Dict.insert "Acirc" ( Char.fromCode 194 )
    |> Dict.insert "Atilde" ( Char.fromCode 195 )
    |> Dict.insert "Auml" ( Char.fromCode 196 )
    |> Dict.insert "Aring" ( Char.fromCode 197 )
    |> Dict.insert "AElig" ( Char.fromCode 198 )
    |> Dict.insert "Ccedil" ( Char.fromCode 199 )
    |> Dict.insert "Egrave" ( Char.fromCode 200 )
    |> Dict.insert "Eacute" ( Char.fromCode 201 )
    |> Dict.insert "Ecirc" ( Char.fromCode 202 )
    |> Dict.insert "Euml" ( Char.fromCode 203 )
    |> Dict.insert "Igrave" ( Char.fromCode 204 )
    |> Dict.insert "Iacute" ( Char.fromCode 205 )
    |> Dict.insert "Icirc" ( Char.fromCode 206 )
    |> Dict.insert "Iuml" ( Char.fromCode 207 )
    |> Dict.insert "ETH" ( Char.fromCode 208 )
    |> Dict.insert "Ntilde" ( Char.fromCode 209 )
    |> Dict.insert "Ograve" ( Char.fromCode 210 )
    |> Dict.insert "Oacute" ( Char.fromCode 211 )
    |> Dict.insert "Ocirc" ( Char.fromCode 212 )
    |> Dict.insert "Otilde" ( Char.fromCode 213 )
    |> Dict.insert "Ouml" ( Char.fromCode 214 )
    |> Dict.insert "times" ( Char.fromCode 215 )
    |> Dict.insert "Oslash" ( Char.fromCode 216 )
    |> Dict.insert "Ugrave" ( Char.fromCode 217 )
    |> Dict.insert "Uacute" ( Char.fromCode 218 )
    |> Dict.insert "Ucirc" ( Char.fromCode 219 )
    |> Dict.insert "Uuml" ( Char.fromCode 220 )
    |> Dict.insert "Yacute" ( Char.fromCode 221 )
    |> Dict.insert "THORN" ( Char.fromCode 222 )
    |> Dict.insert "szlig" ( Char.fromCode 223 )
    |> Dict.insert "agrave" ( Char.fromCode 224 )
    |> Dict.insert "aacute" ( Char.fromCode 225 )
    |> Dict.insert "acirc" ( Char.fromCode 226 )
    |> Dict.insert "atilde" ( Char.fromCode 227 )
    |> Dict.insert "auml" ( Char.fromCode 228 )
    |> Dict.insert "aring" ( Char.fromCode 229 )
    |> Dict.insert "aelig" ( Char.fromCode 230 )
    |> Dict.insert "ccedil" ( Char.fromCode 231 )
    |> Dict.insert "egrave" ( Char.fromCode 232 )
    |> Dict.insert "eacute" ( Char.fromCode 233 )
    |> Dict.insert "ecirc" ( Char.fromCode 234 )
    |> Dict.insert "euml" ( Char.fromCode 235 )
    |> Dict.insert "igrave" ( Char.fromCode 236 )
    |> Dict.insert "iacute" ( Char.fromCode 237 )
    |> Dict.insert "icirc" ( Char.fromCode 238 )
    |> Dict.insert "iuml" ( Char.fromCode 239 )
    |> Dict.insert "eth" ( Char.fromCode 240 )
    |> Dict.insert "ntilde" ( Char.fromCode 241 )
    |> Dict.insert "ograve" ( Char.fromCode 242 )
    |> Dict.insert "oacute" ( Char.fromCode 243 )
    |> Dict.insert "ocirc" ( Char.fromCode 244 )
    |> Dict.insert "otilde" ( Char.fromCode 245 )
    |> Dict.insert "ouml" ( Char.fromCode 246 )
    |> Dict.insert "divide" ( Char.fromCode 247 )
    |> Dict.insert "oslash" ( Char.fromCode 248 )
    |> Dict.insert "ugrave" ( Char.fromCode 249 )
    |> Dict.insert "uacute" ( Char.fromCode 250 )
    |> Dict.insert "ucirc" ( Char.fromCode 251 )
    |> Dict.insert "uuml" ( Char.fromCode 252 )
    |> Dict.insert "yacute" ( Char.fromCode 253 )
    |> Dict.insert "thorn" ( Char.fromCode 254 )
    |> Dict.insert "yuml" ( Char.fromCode 255 )
    |> Dict.insert "euro" ( Char.fromCode 8364 )
